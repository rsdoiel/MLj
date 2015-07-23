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
(* Process the export command.						*)
(*======================================================================*)
structure ProcessExports :> PROCESSEXPORTS =
struct

structure Map = Symbol.OrdMap

fun lookupTyCon (E, [tycon]) = 
    Map.find (EnvOps.TEofE E, tycon)

  | lookupTyCon (E, strid :: longtycon) =
    (case Map.find(EnvOps.SEofE E, strid) of
      NONE => NONE

    | SOME E => lookupTyCon (E, longtycon))

(*----------------------------------------------------------------------*)
(* Process a list of export declarations.				*)
(*----------------------------------------------------------------------*)
fun process exports =
let

  val names = ref ([] : (TyName.TyName * string) list)
  val mainClass = ref (NONE : string option)

  (* Fake entity used for type names and class types for structures *)
  val entity = (Entity.Str, Ids.symbol "_export")

  (* Type name and variable supplies *)
  val tynamesupply = ref (TyName.initial entity)
  val varsupply = ref Var.initial

  fun freshVar () = 
  let
    val (s, v) = Var.fresh (!varsupply)
  in
    varsupply := s; v
  end

  fun freshTyName strid =
  let
    val (n, s) = TyName.fresh ([strid], TySort.classSort) (!tynamesupply)
  in
    tynamesupply := s; n
  end

  open MILTerm 

  fun exportStructures (limit,isExternal) [] (strVars, term) =
    (SepComp.cache := Entity.Map.insert(!SepComp.cache, entity,
      (SepCompTypes.Str 
      {
        E = EnvOps.emptyE,    (* should be ignored anyway *)
        CE = TyName.Map.empty,
        EE = IMap.empty,
        strVars = strVars,
        limit = limit,
        supply = !varsupply,
        term = term,
        tynameTys = TyName.Map.empty,
        ty = MILTy.prod []
      }, NONE)); SOME (!names, !mainClass))

  | exportStructures (limit,isExternal) ((strid,classname,strvar, tyname, E)::rest) 
    (strVars, term) =
let
  val TE = EnvOps.TEofE E
  val SE = EnvOps.SEofE E
  val VE = EnvOps.VEofE E

  val SOME (SepCompTypes.Str { ty, ... },_) = Entity.Map.find(!SepComp.cache,
    (Entity.Str, strid))
  val SOME miltys = MILTy.fromProd ty

  val strVars = Symbol.OrdMap.insert(strVars, strid, strvar)

  val canExportArg = SMLJavaOps.isExportable isExternal
  val canExportRes = SMLJavaOps.isJava isExternal
  fun fromProd ty = 
    case SMLTy.fromProd ty of
      NONE => [ty]
    | SOME tys => tys

  fun processValBind (i, [], _, fields, methods, clinit) =
      let
        val methdef =   
        (
          JavaString.fromString "<clinit>",
          [Method.STATIC, Method.PUBLIC],
          [], NONE,
          SOME (freshVar (), ([], clinit))
        )
      in
        exportStructures (limit,isExternal) rest
        (strVars, LetClass(MILTy.tyname tyname, ([Class.PUBLIC], NONE, []),
        fields, methdef::methods, term))
      end

    | processValBind (i, (name, bind)::rest, milty::miltys, 
        fields, methods, clinit) =
      case bind of
        ValBind.VarSch(SMLSch.TypeScheme([], ty)) =>
        (case SMLTy.fromFunType ty of
          NONE => 
          if canExportRes ty
          then
          let
            val flddef = 
              (
                Symbol.toJavaString name, 
                [Field.STATIC, Field.FINAL, Field.PUBLIC], 
                milty,
                NONE
              )
            val initvar = freshVar ()
            val clinit = LetVal(initvar, Proj(i, Var strvar), 
              Let(Java((Java.PutField, SOME (MILTy.tyname tyname),
                SOME (Symbol.toJavaString name)), [Var initvar],
                MILTy.cmp (Effect.writes, [])), ([], clinit)))
          in
            processValBind(i+1, rest, miltys, flddef::fields, methods, clinit)
          end
          else (print ("\nStructure " ^ Pretty.idToString strid ^ 
            " has val binding for " ^ Pretty.idToString name ^ 
            " whose type cannot be exported"); NONE)
            
        | SOME (argty, resty) =>
          let
            val argtys = fromProd argty                   
            val void = null (fromProd resty)
          in
            if List.all canExportArg argtys andalso 
              (canExportRes resty orelse void)
            then
            let
              val SOME ([argty], cty) = MILTy.fromArrow milty
              val argtys = 
                case MILTy.fromProd argty of
                  SOME tys => tys
                | NONE => [argty]
                  
              val (_,[ty]) = MILTy.fromCmp cty
              val restyopt = 
                case MILTy.fromProd ty of
                  SOME [] => NONE
                | _ => SOME ty

              val argvars = map (fn ty => freshVar ()) argtys
              val funvar = freshVar ()
              val term =
                case argvars of
                  [] =>
                  App(Var funvar, [Tuple []])

                | [argvar] =>
                  App(Var funvar, [Var argvar])

                | _ => 
                  let
                    val prodvar = freshVar ()
                  in
                    LetVal(prodvar, Tuple (map Var argvars),
                      App(Var funvar, [Var prodvar]))
                  end
          
              val term = 
                if isSome restyopt then term 
                else Let(term, ([(Var.dummy, MILTy.prod [])], Triv []))
                
              val methdef =   
                (
                  Symbol.toJavaString name,
                  [Method.STATIC, Method.PUBLIC],
                  argtys, 
                  restyopt,
                  SOME (
                    freshVar (), 
                    (argvars, LetVal(funvar, Proj(i, Var strvar), term)))
                )

              val _ = if Symbol.equal(name, Ids.symbol "main")
                andalso not (isSome restyopt)
                andalso 
                  case argtys of
                    [ty] => MILTy.eq(ty, MILTys.option (MILTy.array 
                       (MILTys.option (MILTy.java
                             (Types.CLASS ClassHandle.string)))))
                  | _ => false

              then mainClass := SOME classname
              else ()
            in
              processValBind(i+1, rest, miltys,fields,methdef::methods,clinit)
            end
            else 
              (print ("\nStructure " ^ Pretty.idToString strid ^ 
                 " has val binding for " ^ Pretty.idToString name ^ 
                 " whose type cannot be exported"); NONE)
          end
        )

      | _ => 
       (print ("\nStructure " ^ Pretty.idToString strid ^ 
          " has binding for " ^ Pretty.idToString name ^ 
          " that cannot be exported"); NONE)
in
  if Symbol.OrdMap.numItems SE > 0 orelse Symbol.OrdMap.numItems TE > 0
  then 
  (print ("\nStructure " ^ Pretty.idToString strid ^ 
    " contains type or structure bindings; cannot export"); NONE)
  else processValBind (0, Symbol.OrdMap.listItemsi VE, 
    miltys, [], [], Triv [])
end


  (*..................................................................*)
  (* Process the declarations					      *)
  (*..................................................................*)
  fun process' ([], tynames, classdefs, strdefs) = 
      let
        fun isExternal tn = 
          List.exists (fn (tn',_) => TyName.eq(tn,tn')) tynames

        val errors = List.mapPartial 
          (SMLJavaOps.isExportableClassDef isExternal) classdefs

        val limit = !varsupply

        val _ = freshVar ()       
      in
        case errors of
          [] =>
          (names := tynames; 
           exportStructures (limit, isExternal) strdefs
           (Symbol.OrdMap.empty, Triv [Tuple []]))

        | messages => 
          (app (fn message => print ("\n" ^ message)) messages; NONE)
      end

    | process' ((s,name)::rest, tynames, classdefs, strdefs) =
      let
        val longid as (strid::longtycon) = map Ids.symbol s
      in
    (*..................................................................*)
    (* Look up the structure					        *)
    (*..................................................................*)
      case SepComp.getE (Entity.Str, strid) of
        NONE => 
        (print ("\nNo such structure: " ^ Pretty.idToString strid); 
         NONE)

      | SOME E =>
    (*..................................................................*)
    (* Either it's a top-level structure (longtycon=[]) or it's a       *)
    (* fully-qualified class type.				        *)
    (*..................................................................*)
        if null longtycon         
        then 
        let
          val tyname = freshTyName strid
          val strvar = freshVar ()
        in
          process'(rest, (tyname,name)::tynames, 
            classdefs, (strid,name,strvar,tyname,E)::strdefs)
        end
        else
          case lookupTyCon (E, longtycon) of
          NONE => 
          (print ("\nNo such type: " ^ Pretty.longidToString longid); 
           NONE)

        | SOME tystr =>
          case TyStr.fromClassType tystr of
            NONE =>
            (print ("\nType is not a class type: " ^ 
              Pretty.longidToString longid); NONE)

          | SOME (tyname, classdef) => 
            process' (rest, (tyname, name)::tynames, 
               (SMLTy.baseType tyname,classdef)::classdefs, strdefs)
      end

in
  process' (exports, [], [], [])
end

end

