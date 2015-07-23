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
(* Dependency analysis of structures and signatures.			*)
(*======================================================================*)
structure SyntaxDep :> SYNTAXDEP =
struct

open SmallSyntax 

infixr 5 ++

val op++ = Entity.Set.union
val empty = Entity.Set.empty

fun singleSigid sigid = Entity.Set.singleton (Entity.Sig, sigid)

(*----------------------------------------------------------------------*)
(* Structure/functor environments: the `types' of structures            *)
(* and functors and the semantic analogue of signatures.	        *)
(*----------------------------------------------------------------------*)
datatype Env = 
  Env of Env Symbol.OrdMap.map		(* SML structure or Java package *)
| ClassStr of Syntax.longid * bool ref	(* Java class; true = seen *)

type Info = 
{
  order : Entity.Ref list,
  deps : Entity.Set.set Entity.Map.map,
  classes : Syntax.longid list
}

(*----------------------------------------------------------------------*)
(* The empty environment						*)
(*----------------------------------------------------------------------*)
val emptyEnv = Env Symbol.OrdMap.empty

(*----------------------------------------------------------------------*)
(* Add a binding to an environment					*)
(*----------------------------------------------------------------------*)
fun add (Env env, id, env') = Env(Symbol.OrdMap.insert(env, id, env'))
  | add (ClassStr _, id, env') = add(emptyEnv, id, env')

(*----------------------------------------------------------------------*)
(* Merge e1 and e2, with e2 over-riding e1.				*)
(*----------------------------------------------------------------------*)
fun extend (Env e1, Env e2) = Env (Symbol.OrdMap.unionWith #2 (e1, e2))
  | extend (ClassStr _, env) = env
  | extend (env, ClassStr _) = env

(*----------------------------------------------------------------------*)
(* Do a dependency analysis starting at the root structures specified.  *)
(*----------------------------------------------------------------------*)
fun analyse ids =
let

(*----------------------------------------------------------------------*)
(* Convert a package map into an environment				*)
(*----------------------------------------------------------------------*)
fun convert longid (PackageManager.Package { classes, packages }) =
    let
      val E1 = Env (Symbol.OrdMap.mapi
        (fn (id, ref p) => convert (longid @ [id]) p) packages)
      val E2 = Env (Symbol.OrdMap.mapi
        (fn (id, _) => ClassStr (longid @ [id], ref false)) classes)
    in
      extend (E1, E2)
    end

(*----------------------------------------------------------------------*)
(* Initial environment for checking structures and functors.		*)
(*----------------------------------------------------------------------*)
val initialEnv = convert [] (PackageManager.getTop ())

(*----------------------------------------------------------------------*)
(* The initial environment for checking signatures includes Int, Real,  *)
(* etc									*)
(*----------------------------------------------------------------------*)
val initialEnvForSig = 
  extend (initialEnv,
  Env 
  (
    Symbol.OrdMap.foldli 
    (fn (strid, _, env) => Symbol.OrdMap.insert(env, strid, emptyEnv))
    Symbol.OrdMap.empty
    TopEnv.initialSE
  ))

val order = ref ([] : Entity.Ref list)
val deps = ref (Entity.Map.empty : (Env * Entity.Set.set) Entity.Map.map)
val classes = ref ([] : Syntax.longid list)
val badParse = ref false

fun parseError () = badParse := true

(*----------------------------------------------------------------------*)
(* Look up a structure/signature identifier in an environment.		*)
(*----------------------------------------------------------------------*)
fun lookup (Env e, id) = Symbol.OrdMap.find(e, id)
  | lookup (ClassStr _, id) = NONE

(*----------------------------------------------------------------------*)
(* Look up an entity reference in the top-level environment.		*)
(* Note that structures and packages share the same entities.           *)
(*----------------------------------------------------------------------*)
fun lookupTop entity = Entity.Map.find(!deps, entity)

(*----------------------------------------------------------------------*)
(* Add a top-level entity to the accumulated dependency info.		*)
(*----------------------------------------------------------------------*)
fun addTop (entity, v) =
    (order := entity :: !order;
     deps := Entity.Map.insert(!deps, entity, v))

fun testCycle (loc, entity, pending) =
let
  fun find acc [] = ()
    | find acc (entity'::rest) =
      if EntityOps.eq(entity,entity')
      then 
        Debug.fail (
        EntityOps.description entity ^ " is in a circular definition with " ^
        Pretty.vec ("itself", "", "", "", "", " and ") EntityOps.description
        acc)
      else find (entity'::acc) rest
in
  find [] pending
end

(*----------------------------------------------------------------------*)
(* Analyse a single declaration.					*)
(*----------------------------------------------------------------------*)
fun analyseDecItem (env, pending) (decitem : DecItem) = 
case decitem of
  Local(dec1, dec2) =>
  let
    val (env1, refs1) = analyseDec (env, pending) dec1
    val (env2, refs2) = analyseDec (extend(env, env1), pending) dec2
  in
    (env2, refs1 ++ refs2)
  end

| Structure bindings =>
  foldl (fn ((strid, strexp), (finalenv, refs)) =>
    let 
      val (strenv, strrefs) = analyseStrExp (env, pending) strexp
    in
      (add(finalenv, strid, strenv), refs ++ strrefs)
    end) (emptyEnv, empty) bindings

| Open (loc, longids) =>
  let val (_,env,refs) = foldl
    (fn (longid, (env, envresult, refs)) =>
    let val (idenv, idrefs) = analyseLongid (env, pending) (loc, longid)
    in
      (extend (env, idenv), extend (envresult, idenv), refs ++ idrefs)
    end) (env, emptyEnv, empty) longids
  in
    (env, refs)  
  end

| Mention mention =>
  let val (_, refs) = analyseLongid (env, pending) mention
  in (emptyEnv, refs) end

| Class class =>
  (emptyEnv, empty)

(*----------------------------------------------------------------------*)
(* Analyse a sequence of declarations.					*)
(*----------------------------------------------------------------------*)
and analyseDec (env, pending) [] = (emptyEnv, empty)
  | analyseDec (env, pending) (decitem::dec) = 
    let
      val (env1, refs1) = analyseDecItem (env, pending) decitem
      val (env2, refs2) = analyseDec (extend (env, env1), pending) dec
    in
      (extend(env1, env2), refs1 ++ refs2)
    end

(*----------------------------------------------------------------------*)
(* Analyse a structure expression.					*)
(*----------------------------------------------------------------------*)
and analyseStrExp (args as (env, pending)) strexp =
case strexp of
  Struct dec =>
  analyseDec args dec

| Strid strid =>
  analyseLongid args strid

| StrConstraint(strexp, sigexp) =>
  let val (strenv, strrefs) = analyseStrExp args strexp
      val (sigenv, sigrefs) = analyseSigExp args sigexp
  in
    (sigenv, strrefs ++ sigrefs)
  end

(*
| FunApp(loc, funid, strexp) =>
  let
    val (argenv, argrefs) = analyseStrExp args strexp
    val entity = (Entity.Fun, funid)
    val x = lookupTop entity
  in
    case x of
      SOME (resultenv, refs) => 
      (resultenv, Entity.Set.add(argrefs, entity))

    | NONE =>
      (testCycle (loc, entity, pending);
        case ParseManager.parse (entity, NONE) of
          ParseManager.Success((_, [Functor []], _),_) => 
          let 
            val (resultenv, resultrefs) = 
              analyseFunExp (initialEnv, entity::pending) funexp
          in
            addTop(entity, (resultenv, resultrefs));
            (resultenv, Entity.Set.add(argrefs, entity))
          end

        | _ => 
          (print "1"; parseError ();
           addTop(entity, (emptyEnv, Entity.Set.empty));
           (emptyEnv, Entity.Set.add(argrefs, entity)))
      )
  end
*)

| StrLet(dec, strexp) =>
  let
    val (env1, refs1) = analyseDec args dec
    val (env2, refs2) = analyseStrExp (extend(env, env1), pending) strexp
  in
    (env2, refs1 ++ refs2)
  end

(*----------------------------------------------------------------------*)
(* Analyse a signature expression.					*)
(*----------------------------------------------------------------------*)
and analyseSigExp (env, pending) sigexp =
case sigexp of
  Sigid(loc, sigid) =>
  let 
    val entity = (Entity.Sig, sigid)
    val x = lookupTop entity
  in
    case x of
      SOME (env, imports) => 
      (env, Entity.Set.singleton entity)

    | NONE =>
      (testCycle (loc, entity, pending);
        case ParseManager.parse (entity, NONE) of
          ParseManager.Success((_,[Signature [(_,sigexp)]], _),_) => 
          let 
            val (env, refs) = 
              analyseSigExp (initialEnvForSig, entity::pending) sigexp
          in
            addTop(entity, (env, refs));
            (env, Entity.Set.singleton entity)
          end

        | _ => 
          (print "2"; parseError (); 
          addTop(entity, (emptyEnv, Entity.Set.empty));
          (emptyEnv, Entity.Set.singleton entity))
      )
  end

| SigSpec spec =>
  analyseSpec (env, pending) spec

| Where(sigexp, mentions) =>
  let
    val (sigenv, refs) = analyseSigExp (env, pending) sigexp
    val refs = foldl 
    (fn (mention, refs) =>
      let val (_, refs') = analyseLongid (env, pending) mention
      in refs ++ refs' end) refs mentions
  in
    (sigenv, refs)
  end

(*----------------------------------------------------------------------*)
(* Analyse a functor expression						*)
(*----------------------------------------------------------------------*)
and analyseFunExp (env, pending) (formal, sigexp, strexp) =
  let
    val (env1, refs1) = analyseSigExp (env, pending) sigexp
    val (env2, refs2) = analyseStrExp (add(env, formal, env1), pending) strexp
  in
    (env2, refs1 ++ refs2)
  end

(*----------------------------------------------------------------------*)
(* Analyse a sequence of specifications.				*)
(*----------------------------------------------------------------------*)
and analyseSpec (env, pending) [] = 
    (emptyEnv, empty)

  | analyseSpec (env, pending) (specitem::spec) = 
    let
      val (env1, refs1) = analyseSpecItem (env, pending) specitem
      val (env2, refs2) = analyseSpec (extend (env, env1), pending) spec
    in
      (extend(env1, env2), refs1 ++ refs2)
    end

(*----------------------------------------------------------------------*)
(* Analyse a single specification.					*)
(*----------------------------------------------------------------------*)
and analyseSpecItem (args as (env, pending)) specitem =
case specitem of
  StructureDesc bindings =>
  foldl (fn ((strid, sigexp), (finalenv, refs)) =>
    let val (sigenv, sigrefs) = analyseSigExp (env, pending) sigexp
    in
      (add(finalenv, strid, sigenv), refs ++ sigrefs)
    end) (emptyEnv, empty) bindings 

| Include sigexp =>
  analyseSigExp args sigexp

| SpecMention mention =>
  let
    val (_, refs) = analyseLongid args mention
  in
    (emptyEnv, refs)
  end
  
(*----------------------------------------------------------------------*)
(* Analyse a single long identifier.					*)
(* Resolution is as follows:                                            *)
(*   (1) Local -- already in the environment;                           *)
(*   (2) Global structure/package: for an identifier strid.longid       *)
(*       EITHER a top-level structure strid must exist;                 *)
(*       OR     a top-level package strid must exist.                   *)
(*----------------------------------------------------------------------*)
and analyseLongid (env, pending) (loc, longstrid as id::ids) = 
let
  (* Analyse a longid that starts with a non-top-level identifier *)
  fun analyseLocal (env, refs) [] = (env, refs)
    | analyseLocal (env, refs) (id::ids) =
      case lookup(env, id) of
        (* This could be flagged as a missing substructure error *)
        NONE => 
        (emptyEnv, refs)

      | SOME env => 
        analyseLocal (env, refs) ids

  val entity = (Entity.Str, id)

  val result as (env, refs) = 
  case lookup(env, id) of
    SOME env => 
    analyseLocal (env, empty) ids

  | NONE => 
    case lookupTop entity of
      SOME (env, refs) =>
      analyseLocal (env, Entity.Set.singleton entity) ids

    | NONE =>      
      (testCycle (loc, entity, pending);
        case ParseManager.parse (entity, NONE) of
          ParseManager.Success((_,[Structure [(_,strexp)]], _),_) => 
          let val (env, refs) = 
            analyseStrExp (initialEnv, entity::pending) strexp
          in
            addTop (entity, (env, refs));
            analyseLocal (env, Entity.Set.singleton entity) ids
          end

        | ParseManager.NotFound =>
          (PrintManager.print 
            ("structure " ^ Pretty.idToString id ^ " not found");
          parseError (); 
          addTop(entity, (emptyEnv, Entity.Set.empty));
          (emptyEnv, Entity.Set.singleton entity))

        | _ => 
          (print "3"; parseError (); 
          addTop(entity, (emptyEnv, Entity.Set.empty));
          (emptyEnv, Entity.Set.singleton entity))
     )
in
  (case env of
    ClassStr (longid, r as ref false) => 
    (r := true; classes := longid :: !classes)
  | _ => ());
  result
end

in
  map (fn id => analyseLongid (initialEnv, []) 
    ({left=0,right=0}, [Ids.symbol id])) ids;
  if !badParse 
  then NONE 
  else SOME 
  { 
    deps = Entity.Map.map #2 (!deps), 
    order = !order, 
    classes = !classes
  }
end
        
end

