(*......................................................................*)
(* MLj - a Standard ML to Java bytecode compiler                        *)
(* Copyright (C) 1999 Persimmon IT Inc.                                 *)
(*                                                                      *)
(* This program is free software; you can redistribute it and/or        *)
(* modify it under the terms of the GNU General Public License          *)
(* as published by the Free Software Foundation; either version 2       *)
(* of the License, or (at your option) any later version.               *)
(*                                                                      *)
(* This program is distributed in the hope that it will be useful,      *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with this program; if not, write to the Free Software          *)
(* Foundation, Inc., 59 Temple Place - Suite 330, Boston,               *)
(* MA 02111-1307, USA.                                                  *)
(*......................................................................*)

(*======================================================================*)
(* Separate compilation of an entity (structure, signature, functor)    *)
(*======================================================================*)
structure SepComp :> SEPCOMP =
struct

open SepCompTypes

(*----------------------------------------------------------------------*)
(* Local transformations to apply					*)
(*----------------------------------------------------------------------*)
val opts = ref ["presimplify", "fullarity", "presimplify"]

(*----------------------------------------------------------------------*)
(* Cached results with the timestamped file reference of the source     *)
(* from which they were	derived. If no ref is present then the result   *)
(* is out of date.                                                      *)
(*----------------------------------------------------------------------*)
val cache = ref (Entity.Map.empty : Cache)

(*----------------------------------------------------------------------*)
(* What SML environment do we have cached for this entity?  		*)
(*----------------------------------------------------------------------*)
fun getE entity = 
case Entity.Map.find(!cache, entity) of
  NONE => 
  NONE

| SOME (Sig (_,E), _) => 
  SOME E

| SOME (Str { E, ...}, _) =>
  SOME E

(*----------------------------------------------------------------------*)
(* Dump the type name map						*)
(*----------------------------------------------------------------------*)
fun tynameTysToString tynameTys =
  Pretty.bigVec 0
  (fn (tyname, ty) => TyName.toString tyname ^ " = " ^ MILTy.toString ty)
  (TyName.Map.listItemsi tynameTys)


fun make { deps, order, classes = usedclasses } =
let

(*----------------------------------------------------------------------*)
(* Construct a skeleton environment for Java classes and packages       *)
(*----------------------------------------------------------------------*)
fun makeClassEnv () =
let
  val info = PackageManager.getTop ()

  fun convert longid (PackageManager.Package { packages, classes }) =
  let
    fun makeTyStr id =
    TyStr.makeConcrete([],
      SMLTy.baseType (TyName.externalClass (longid @ [id])))

    (* Create type bindings for each class *)
    val TE = Symbol.OrdMap.mapi (fn (id, _) => makeTyStr id) classes

    (* Create structure bindings each class; only fill in those used *)
    val SE1 = Symbol.OrdMap.mapi (fn (id, _) =>
      if List.exists (fn c => Eq.list Symbol.equal (c, longid @ [id])) 
        usedclasses
      then TransJava.classToStruct 
        (valOf (PackageManager.getClass (longid @ [id])))
      else EnvOps.emptyE) classes

    (* Create structure bindings for each package *)
    val SE2 = Symbol.OrdMap.mapi
      (fn (id, ref p) => convert (longid @ [id]) p) packages
  in
    Env.Env (EnvOps.SEplusSE SE1 SE2, TE, Symbol.OrdMap.empty)
  end
in
  convert [] info
end

val initialE = makeClassEnv ()
val initialB = EnvOps.BplusE TopEnv.initialB initialE

(*----------------------------------------------------------------------*)
(* Given the imports for a module, calculate four things:		*)
(*   (1) An SML basis B under which to elaborate the module;            *)
(*   (2) A class environment CE under which to elaborate the module;    *)
(*   (3) A map EE from SML exnames to MIL exnames;                      *)
(*   (4) A map strTys from the imported structures to their MIL types;  *) 
(*   (5) A map tynameTys from imported tynames to MIL types/constructors*) 
(*----------------------------------------------------------------------*)
fun makeEnv (prefix, entity) =
case Entity.Map.find(deps, entity) of
  NONE => 
  Debug.fail 
  ("ElabManager.makeEnv: dependency info missing for " ^ 
    EntityOps.description entity)

| SOME imports =>
  let
    fun add ([], result) = 
        (imports, result)

        (* For signatures just add to the SML basis *)
      | add (entity::entities, (B, CE, EE, strTys, tynameTys)) =
        case (entity, Entity.Map.find(!cache, entity)) of
        ((Entity.Sig, sigid), SOME ((Sig sigma, _))) =>
        let
          val singletonG = 
            Symbol.OrdMap.insert(Symbol.OrdMap.empty, sigid, sigma)
        in
          add (entities, 
            (EnvOps.BplusG B singletonG, CE, EE, strTys, tynameTys))
        end

        (* For structures add to everything *)
      | ((Entity.Str, strid), 
          SOME (Str {E, CE = CE', EE = EE', 
                        ty = ty, tynameTys = tynameTys', ...}, _)) =>
        let
          val singletonSE = Symbol.OrdMap.insert(Symbol.OrdMap.empty, strid, E)
          val B = EnvOps.BplusE B (EnvOps.SEinE singletonSE)
          val tynameTys = TyName.Map.unionWith #2 (tynameTys, tynameTys')
          val EE = SMLTy.ExMap.unionWith #2 (EE, TransExn.transEE entity
            tynameTys EE')
          val CE = TyName.Map.unionWith #2 (CE, CE')
          val strTys = Symbol.OrdMap.insert(strTys, strid, ty)
        in
          add (entities, (B, CE, EE, strTys, tynameTys))
        end

      | _ => 
        Debug.fail ("ElabManager.makeEnv:add: elaboration info missing for "
        ^ EntityOps.description entity)

in
  add (prefix, 
    (initialB,
     TyName.Map.empty,SMLTy.ExMap.empty,
     Symbol.OrdMap.empty,TyName.Map.empty))
end

(*----------------------------------------------------------------------*)
(* Actually do the elaboration and update the cache.			*)
(*----------------------------------------------------------------------*)
fun doelab (prefix,entity,topbind,sourcemap,fileref : Entity.FileRef) =
let
  val (imports,(B,CE,EE,strTys,tynameTys)) = makeEnv (prefix, entity)

  val (result, errors) = 
  case topbind of
    [(loc,Syntax.Signature [sigbind])] =>
    let 
      val (sigdata as (_,_,sigma), errors) = 
      PrintManager.process ("Type checking " ^ 
        EntityOps.descriptionWithFile (entity, fileref), true)
        (fn () => Elab.infTopSigExp (B,CE) sigbind)
      
      (* Optionally dump the SML environment to the log *)
      val _ = 
        if Controls.isOn "showSMLEnv" 
        then Debug.print ("\nSML env = " ^ EnvOps.EtoString (#2 sigma))
        else ()
      
    in 
      if List.all (not o Error.isSerious) errors
      then (SOME (Sig sigma), errors)
      else (NONE, errors)
    end

  | [(loc,Syntax.Structure [strbind])] =>
    let

      (* Elaborate the structure *)
      val ((strDE,strEE,strCE,psi,E,e), errors) = 
      PrintManager.process ("Type checking " ^ 
        EntityOps.descriptionWithFile (entity, fileref), true)
        (fn () => Elab.infTopStrExp (B,CE) strbind)

    in
      if List.exists Error.isSerious errors
      then (NONE, errors)
      else PrintManager.process ("Compiling " ^ EntityOps.description entity,
        true)
      (fn () =>
      let
        (* Construct list of pairs of form (strid,milty) *)
        val stridTys = 
          List.mapPartial
          (fn (Entity.Str, x) => SOME (x,valOf(Symbol.OrdMap.find(strTys,x)))
            | _ => NONE) (Entity.Set.listItems imports)

        (* Construct a map from structure identifiers to (var,ty) pairs *)
        val (SE, limit) =
          foldr (fn ((x,ty), (SE,supply)) =>
            let
              val (supply',x') = Var.fresh supply
            in
              (Symbol.OrdMap.insert(SE, x, (x',ty)), supply')
            end) (Symbol.OrdMap.empty, Var.initial) stridTys

        val strVars = Symbol.OrdMap.map #1 SE

        (* Translate the datatype environment *)
        val tynameTysDE = TransType.transDE tynameTys strDE

        val tynameTys = TyName.Map.unionWith #2 (tynameTys, tynameTysDE)
        
        (* Translate the realisation *)
        val tynameTysPsi = TransType.transRealisation tynameTys psi

        val tynameTys = TyName.Map.unionWith #2 (tynameTys, tynameTysPsi)

        val tyTys = TyName.Map.foldri (fn (tyname,ty,m) =>
          MILTy.Map.insert(m, MILTy.tyname tyname, ty)) 
          MILTy.Map.empty tynameTys

        (* Hack *)
        val tynameTys = TyName.Map.map (MILTy.replace tyTys) tynameTys

        (* Optionally dump the SML typed term to the log *)
        val _ = 
          if Controls.isOn "showSMLTerm" 
          then Debug.print ("\nSML term = " ^ SMLTermOps.toString e)
          else ()

        (* Optionally dump the SML environment to the log *)
        val _ = 
          if Controls.isOn "showSMLEnv" 
          then Debug.print ("\nSML env = " ^ EnvOps.EtoString E)
          else ()
      
        (* Optionally dump the SML datatype environment to the log *)
        val _ = 
          if Controls.isOn "showSMLTypes" 
          then Debug.print ("\nSML types = " ^ SMLTy.DEtoString strDE)
          else ()

        (* Optionally dump the SML realisation to the log *)
        val _ = 
          if Controls.isOn "showSMLTypes" 
          then Debug.print ("\nSML realisation = " ^ 
            SMLTy.realisationToString psi)
          else ()     

        (* Optionally dump the MIL type environment to the log *)
        val _ =
          if Controls.isOn "showMILTypes"
          then Debug.print ("\nMIL types = " ^ tynameTysToString tynameTys)
          else ()

        val EE = SMLTy.ExMap.unionWith #2 (EE, 
          TransExn.transEE entity tynameTys strEE)

        (* Translate the SML typed term into a MIL computation term *)
        val { term, cty, varsupply, tyvarsupply, errors = errors' } =
          PrintManager.process ("Translating", false)
          (fn () =>
            Trans.trans 
            { 
              entity = entity,
              strexp = e,
              SE = SE,
              EE = EE,
              tynameTys = tynameTys,
              supply = limit
            })

      in
        if List.exists Error.isSerious errors'
        then (NONE, errors @ errors')
        else
        let

          (* Translate the SML environment into a MIL tuple type *)
          val ty = TransType.transE tynameTys E

          (* Construct a type environment under which to transform the term *)
          val tyenv = foldr (fn ((strid, ty), tyenv') =>            
            case Symbol.OrdMap.find(strVars, strid) of
              NONE => tyenv' 
            | SOME v => 
              Var.Map.insert(tyenv', v, ty)) Var.Map.empty stridTys

          val (term,supply) = ApplyOpt.applyOpts (!opts) tyenv (term,varsupply)
        in
          (SOME (Str 
           { E = E, CE = strCE, EE = strEE, 
             strVars = strVars, supply = supply, 
             tynameTys = TyName.Map.unionWith #2 (tynameTysDE, tynameTysPsi), 
             term = term, ty = ty, limit = limit}),
          errors @ errors')
        end
      end)
    end

  | _ =>
    Debug.fail ("ElabManager.doelab: " ^ EntityOps.description entity ^
    " not bound")
in
  PrintManager.printErrors (sourcemap, errors);
  result
end

(*----------------------------------------------------------------------*)
(* Has the environment to which an entity elaborates changed?           *)
(* If so, then all dependent entities must be recompiled.               *)
(*----------------------------------------------------------------------*)
fun infoChanged (Sig (_,E1), Sig(_,E2)) = 
    not (EnvOps.eq (E1, E2))

  | infoChanged (Str { E = E1, ... }, Str { E = E2, ... }) =
    not (EnvOps.eq (E1, E2))

  | infoChanged _ = true

val primStr = (Entity.Str, Ids.symbol "Prim")
val primSig = (Entity.Sig, Ids.symbol "PRIM")

(*----------------------------------------------------------------------*)
(* Type check, translate and transform entities in dependency order	*)
(*----------------------------------------------------------------------*)
fun elab (prefix, []) changed = true

  | elab (prefix, entity::entities) changed =
    case Entity.Map.find(!cache, entity) of

  (* If it's in the cache then only elaborate if it's out of date or
     in the changed set *)
    SOME (oldinfo, oldref) =>
    if EntityOps.eq(entity, primStr)
    then elab (entity::prefix, entities) changed
    else 
    let
      val imports = 
        case Entity.Map.find(deps, entity) of
          SOME imports => imports
        | NONE => Debug.fail "SepComp.elab: entity disappeared"

      val mustelab = not 
        (Entity.Set.isEmpty (Entity.Set.intersection(changed, imports)))
    in
      case ParseManager.parse (entity, if mustelab then NONE else oldref) of
        ParseManager.Fail => 
        false

      | ParseManager.Success ((topbind,_,sourcemap), SOME newref) =>
        (case doelab (prefix, entity, topbind, sourcemap, newref) of
          NONE => 
          false

        | SOME result =>
          (cache := Entity.Map.insert(!cache, entity, (result, SOME newref));
          elab (entity::prefix, entities) 
               (if infoChanged (oldinfo, result)
                then Entity.Set.add(changed, entity) else changed))
        )

      | ParseManager.NotFound =>
        Debug.fail "SepComp.elab: missing file"

      | _ =>  
        elab (entity::prefix, entities) changed

    end

  | NONE =>
    if EntityOps.eq(entity, primStr)
    then
    let
      val SOME E = getE primSig
      val (imports,(B,CE,EE,strTys,tynameTys)) = makeEnv (prefix,primSig)
      val entry = SepCompPrim.makePrimEntry (tynameTys, E)
    in
      cache := Entity.Map.insert(!cache, entity, (entry, NONE));
      elab (entity::prefix, entities) changed
    end

    else
    (case ParseManager.parse (entity, NONE) of
      ParseManager.Success ((topbind,_,sourcemap), SOME fileref) =>
      (case doelab (prefix, entity, topbind, sourcemap, fileref) of
        NONE => false
      | SOME result => 
        (cache := Entity.Map.insert(!cache, entity, (result, SOME fileref));
        elab (entity::prefix, entities) (Entity.Set.add(changed, entity)))
      )

    | ParseManager.NotFound =>
      
      (print (EntityOps.description entity ^ " not found.\n"); false)

    | _ => 
      false)

in
  elab ([], rev order) Entity.Set.empty
end

fun freeze p = 
  cache := Entity.Map.filteri (fn (entity,_) => p entity) (!cache)

end (* of struct *)

