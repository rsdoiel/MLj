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
(* Pretty-print MIL types and terms					*)
(*======================================================================*)
structure MILPretty :> MILPRETTY =
struct

val printDepth = ref 30

local 
  open MILTerm Pretty 
in

(*----------------------------------------------------------------------*)
(* Can a value expression be displayed without parentheses?		*)
(*----------------------------------------------------------------------*)
fun isAtomVal (Inj(_,_,_::_)) = false
  | isAtomVal (ExCon _) = false
  | isAtomVal (Proj _) = false
  | isAtomVal (SCon _) = false
  | isAtomVal (Fold _) = false
  | isAtomVal _        = true

(*----------------------------------------------------------------------*)
(* Can a computation expression be displayed without parentheses?	*)
(*----------------------------------------------------------------------*)
fun isAtomCmp (Case _) = true
  | isAtomCmp (CaseSCon _) = true
  | isAtomCmp (CaseExCon _) = true
  | isAtomCmp (Let _) = true
  | isAtomCmp _ = false

  fun seq (sep, ts) = 
  let 
    fun seq' [] = ""
      | seq' [t] = t
      | seq' (t::ts) = t ^ sep ^ seq' ts
  in
    seq' ts
  end

(*----------------------------------------------------------------------*)
(* Pretty print various lists						*)
(*   Types are written between square brackets				*)
(*   Tuple values are written between round brackets			*)
(*   Multiple arguments are written between angle brackets		*)
(*----------------------------------------------------------------------*)
fun tyvec f = vec (" {}", " {", "}", " {", "}", ",") f
fun argvec f = vec (" <>", " ", "", " <", ">", ",") f
fun bindvec f = vec ("", " ", "", " <", ">", ",") f
fun conargvec f = vec ("", " ", "", "<", ">", ",") f
fun tuplevec f = vec ("()", "(", ")", "(", ")", ",") f
fun javavec f = vec ("()", "(", ")", "(", ")", ",") f

(*----------------------------------------------------------------------*)
(* Convert a back-end `test' into standard SML notation			*)
(*----------------------------------------------------------------------*)
fun testToString t =
case t of
  MLEq     => "="
| JavaTest Tests.eq => "=="
| JavaTest Tests.ne => "<>"
| JavaTest Tests.lt => "<"
| JavaTest Tests.gt => ">"
| JavaTest Tests.le => "<="
| JavaTest Tests.ge => ">="

fun kindToString AnyFun = ""
  | kindToString LocalFun = "block "
  | kindToString KnownFun = "known "


fun displayFunDef (args as (indent,closures)) (kind,def) = 
  case def of
    Fun(f, tabs) => 
    kindToString kind ^ Var.toString f ^ " = fn" ^ 
      displayTAbstr " =>" args tabs

  | RecFun defs =>
    simpleVec (newline (indent+1) ^ "and ")
      (fn (f, g, tabs, cty) =>
    kindToString kind ^ Var.toString f ^ " as " ^ Var.toString g 
        ^ " : " ^ MILTy.cmpToString cty ^ " = fn" ^ 
        displayTAbstr " =>" args tabs) defs

and displayClass (args as (indent,closures)) 
  (classname, (flags,super,implements), fields, methods) =
    let
      val nl = newline indent
      fun fieldsToString [] = ""
        | fieldsToString ((name, mods, ty, eopt)::rest) =
          JavaString.toMLString name ^ " : " ^ MILTy.toString ty
          ^ (case eopt of NONE => "" | SOME e => " = " ^ 
             Constants.constant_toString e)
          ^ nl ^ fieldsToString rest
      fun methodsToString [] = ""
        | methodsToString ((name, mods, tys, tyopt, absopt)::rest) =
          let
            val argtys = 
              if List.exists (fn Method.STATIC => true | _ => false) mods
              then tys
              else classname::tys
            val bodystr = 
              case absopt of 
                NONE => "" 
              | SOME (f,(vs,e)) => 
                " as " ^ Var.toString f ^ " = " ^ nl ^ 
                "fn" ^ displayTAbstr " => " args (ListPair.zip(vs,argtys),e)
          in
            JavaString.toMLString name ^ bodystr ^ nl ^ methodsToString rest
          end
    in                 
      "class " ^ MILTy.toString classname ^  
      nl ^ "{" ^ nl ^
      fieldsToString fields ^ methodsToString methods ^ "}"
    end

(*----------------------------------------------------------------------*)
(* Pretty-print a value expression					*)
(* Indent is the current indentation level.                             *)
(*----------------------------------------------------------------------*)
and displayVal (args as (indent,closures)) ve =
let
  val dv = displayVal args
in
  if indent > !printDepth then "..."
  else
  case ve of
    SCon (ty, jcon) =>
    (Constants.constant_toString jcon ^ ":" ^ MILTy.toString ty)

  | Var v => 
    Var.toString v

  | TApp(ve, tys) =>
    displayAtomVal args ve ^ tyvec MILTy.toString tys

  | TAbs(tyvars, ve) =>
    vec("Fn ", "Fn ", "=>", "Fn ", "=>", ",") 
    MILTy.boundTyVarToString tyvars ^ dv ve

  | Inj(ty, i, args) => 
    "in_" ^ Int.toString i ^ "{" ^ MILTy.toString ty ^ "}" ^ conargvec dv args

  | ExCon(exty, args) =>
    MILTy.toString exty ^ conargvec dv args

  | Tuple velist => 
    tuplevec dv velist

  | Proj(i, ve) => 
    "#" ^ Int.toString i ^ " " ^ displayAtomVal args ve

  | Coerce (ve, ty) =>
    "(" ^ displayAtomVal args ve ^ ":>:" ^ MILTy.toString ty ^ ")"

  | Fold (ve, ty) =>
    "fold{" ^ MILTy.toString ty ^ "} " ^ displayAtomVal args ve

  | Unfold ve =>
    "unfold " ^ displayAtomVal args ve

  | Null ty =>
    "null"

  | Closure(i, vs) =>
    "closure_" ^ Int.toString i ^ conargvec dv vs
 
end

and displayAtomVal args ve = 
  if isAtomVal ve then displayVal args ve
  else "(" ^ displayVal args ve ^ ")"

(*----------------------------------------------------------------------*)
(* Pretty-print a computation expression				*)
(*----------------------------------------------------------------------*)
and displayCmp (args as (indent,closures)) ce = 
let
  val dv = displayVal args
  val dc = displayCmp args
  val nl = newline indent

  fun displayBoundTyVars tyvars =
    vec ("", "(", ") ", "(", ") ", ",") 
    MILTy.boundTyVarToString tyvars

  fun displayCont ce =  
      case ce of
        Let(e,(typedvars,e')) => 
        if Controls.isOn "showTypedLet"
        then displayTypedLet (e, (typedvars, e'))
        else displayLet (e, (map #1 typedvars, e'))
      | LetVal a => displayBind a
      | LetFun a => displayLetFun a
      | Init a => displayInit a
      | _ =>
        nl ^ "in" ^ 
        newline (indent+1) ^ displayCmp (indent+1,closures) ce ^
        nl ^ "end"

  and displayLet (ce1, (vs, ce2)) =
    newline (indent+1) ^
      vec ("<>", "", "", "<", ">", ",") Var.toString vs ^ 
      " <= " ^ 
      displayCmp (indent+1,closures) ce1 ^ 
      displayCont ce2

  and displayTypedLet (ce1, (vs, ce2)) =
    newline (indent+1) ^
      vec ("<>", "", "", "<", ">", ",") (fn (v, ty) => 
        Var.toString v ^ ":" ^ MILTy.toString ty) vs ^ 
      " <= " ^ 
      displayCmp (indent+1,closures) ce1 ^ 
      displayCont ce2

  and displayBind (v, ve, ce) =
    newline (indent+1) ^
      Var.toString v ^ " = " ^ displayVal (indent+1,closures) ve ^
      displayCont ce

  and displayInit (x, i, ve, ce) =
    newline (indent+1) ^
      "#" ^ Int.toString i ^ " " ^ Var.toString x ^ " = " ^ 
        displayVal (indent+1,closures) ve ^
      displayCont ce

  and displayLetFun (tyvars, kind, def, ce) =
      newline (indent+1) ^ displayBoundTyVars tyvars ^
      displayFunDef args (kind,def) ^ displayCont ce

  and displayClosBind (funvar, funvar', vars, tabs, cty) =
    Var.toString funvar ^  
    (if Var.isDummy funvar' then "" else " as " ^ 
      Var.toString funvar' ^ " : " ^ MILTy.cmpToString cty) ^
    (if closures then " = clos(" ^
    Pretty.simpleVec "," Var.toString vars ^ ") fn" ^ 
    displayTAbstr " =>" args tabs else "= #")

  fun displayAbstr middle (vs, body) =
    bindvec Var.toString vs ^ middle ^ " " ^ 
    displayCmp (indent+1,closures) body

  fun displayCases f (ve, bindargs, cases, ceopt) =
    "case " ^ dv ve ^ " of " ^ nl ^ "  " ^ 
       seq("| ", map (fn (i,abs) => f i ^ displayAbstr " => " abs ^ nl) cases)
       ^ (case ceopt of NONE => "" | SOME ce => "| _ => " ^ dc ce ^ nl) ^ "end"

in
  if indent > !printDepth then "..."
  else
  case ce of
    App(ve, velist) => 
    displayAtomVal args ve ^ argvec (displayAtomVal args) velist

  | Java(j as (optype, tyopt, nameopt), velist, cty) => 
    JavaOps.opTypeToString optype ^ 
    (case tyopt of NONE => "" | SOME ty => " " ^ MILTy.toString ty)
    ^ (case nameopt of NONE => "" | SOME name => " \"" ^ 
      JavaString.toMLString name ^ "\"")
    ^ javavec dv velist
    ^ ":" ^ MILTy.cmpToString cty

  | Triv velist => 
    vec ("val <>", "val ", "", "val <", ">", ",") dv velist

  | Case cases =>
    displayCases (fn i => "in_" ^ Int.toString i) cases

  | CaseSCon cases =>
    displayCases Constants.constant_toString cases

  | CaseExCon cases =>
    displayCases MILTy.toString cases

  | Cond(t, ve1, ve2, ce1, ce2) =>
    "(if " ^ dv ve1 ^ " " ^ testToString t ^ " " ^ dv ve2 ^ " then " ^
    dc ce1 ^ " else " ^ dc ce2 ^ ")"

  | Throw(ve, tys, _) => 
    "raise " ^ displayAtomVal args ve ^ " : " ^ tyvec MILTy.toString tys 

  | TryLet(try, tabss, tabs) =>
    nl ^ "try" ^ newline (indent+1) ^ displayCmp (indent+1,closures) try ^ 
    nl ^ "handle" ^ newline (indent+1) ^ 
    seq("| ", map (fn tabs => displayTAbstr " => " args tabs ^ nl) tabss) ^ 
    nl ^ "else" ^ newline (indent+1) ^ displayTAbstr " => " args tabs ^
    nl ^ "end" ^ nl

  | Let(e,(typedvars,e')) =>
    nl ^ "let" ^ 
      (if Controls.isOn "showTypedLet"
      then displayTypedLet (e, (typedvars, e'))
      else displayLet (e, (map #1 typedvars, e')))

  | LetVal a =>
    nl ^ "let" ^ displayBind a

  | LetFun a =>
    nl ^ "let" ^ displayLetFun a

  | Init a =>
    nl ^ "let" ^ displayInit a

  | LetClass(classname, info, fields, methods, ce) =>
    "let" ^ displayClass args (classname, info, fields, methods) ^ 
    " in " ^ nl ^ dc ce

  | Alloc (AnyRef,ve) =>
    "ref " ^ displayAtomVal args ve 

  | Alloc (GlobalRef,ve) =>
    "varref " ^ displayAtomVal args ve 

  | Deref ve =>
    "!" ^ displayAtomVal args ve 
    
  | Assign(v1,v2) =>
    "(" ^ dv v1 ^ " := " ^ dv v2 ^ ")"

  | Encap e =>
    "_pure(" ^ displayCmp args e ^ ")"

end

and displayTAbstr sep (indent,closures) (vs, body) =
  bindvec (fn (v,ty) => Var.toString v ^ ":" ^ MILTy.toString ty) vs
  ^ sep ^ displayCmp (indent+1,closures) body


end

fun valToString ve = displayVal (0,true) ve
fun cmpToString ce = displayCmp (0,true) ce
fun tabstrToString tabs = displayTAbstr " =>" (0,false) tabs
fun fundefToString a = displayFunDef (0,true) a
fun classdefToString a = displayClass (0,true) a

(*----------------------------------------------------------------------*)
(* Failure error messages dumped to the log.                            *)
(*----------------------------------------------------------------------*)
fun failVal ve message = 
  (Debug.print ("Fail: " ^ message ^ " in:\n" ^ valToString ve);
  raise Fail message)

fun failCmp ce message = 
  (Debug.print ("Fail: " ^ message ^ " in:\n" ^ cmpToString ce);
  raise Fail message)


end
