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
(* General Java stuff       						*)
(*======================================================================*)
structure JavaOps :> JAVAOPS =
struct

local open Java in

fun lookup match [] = "?"
  | lookup match ((x,y)::rest) = if match x then y else lookup match rest

(*----------------------------------------------------------------------*)
(* Legal class modifiers						*)
(*----------------------------------------------------------------------*)
val classModifiersList =
[
  (Class.ABSTRACT, "abstract"),
  (Class.FINAL, "final"),
  (Class.PUBLIC, "public")
]

val classModifiers = 
  foldr (fn ((x,s),m) => Map.insert(m, s, x)) Map.empty classModifiersList

fun classModToString m = "_" ^ lookup (fn m' => m=m') classModifiersList

(*----------------------------------------------------------------------*)
(* Legal field modifiers						*)
(*----------------------------------------------------------------------*)
val fieldModifiersList =
[
  (Field.FINAL, "final"),
  (Field.PRIVATE, "private"),
  (Field.PROTECTED, "protected"),
  (Field.PUBLIC, "public"),
  (Field.STATIC, "static"),
  (Field.TRANSIENT, "transient"),
  (Field.VOLATILE, "volatile")
]

val fieldModifiers =
  foldr (fn ((x,s),m) => Map.insert(m, s, x)) Map.empty fieldModifiersList

fun fieldModToString m = "_" ^ lookup (fn m' => m=m') fieldModifiersList

(*----------------------------------------------------------------------*)
(* Legal method modifiers						*)
(*----------------------------------------------------------------------*)
val methodModifiersList =
[
  (Method.ABSTRACT, "abstract"),
  (Method.FINAL, "final"),
  (Method.PRIVATE, "private"),
  (Method.PROTECTED, "protected"),
  (Method.PUBLIC, "public"),
  (Method.STATIC, "static"),
  (Method.SYNCHRONIZED, "synchronized")
]

val methodModifiers =
  foldr (fn ((x,s),m) => Map.insert(m, s, x)) Map.empty methodModifiersList
fun methodModToString Method.NATIVE = "_native"
  | methodModToString m = "_" ^ lookup (fn m' => m=m') methodModifiersList

(*----------------------------------------------------------------------*)
(* Legal constructor modifiers						*)
(*----------------------------------------------------------------------*)
val constructorModifiers =
foldr (fn ((x,s),m) => Map.insert(m, s, x)) Map.empty
[
  (Method.PRIVATE, "private"),
  (Method.PROTECTED, "protected"),
  (Method.PUBLIC, "public")
]

val coreops = 
foldr (fn ((x,s),m) => Map.insert(m, s, x)) Map.empty
[
  (Cast, "cast"),
  (GetField, "getfield"),
  (InstanceOf, "instanceof"),
  (Invoke, "invoke"),
  (New, "new"),
  (PutField, "putfield"),
  (Super, "super"),
  (This, "this"),
  (Synchronize, "synchronize")
]

val optops = 
foldr (fn ((x,s),m) => Map.insert(m, s, x)) Map.empty
[
  (Add, "add"),
  (And, "and"),
  (ArrayLength, "arraylength"),
  (ArrayLoad, "arrayload"),
  (ArrayStore, "arraystore"),
  (CmpG, "cmpg"),
  (CmpL, "cmpl"),
  (Div, "div"),
  (ExnName, "exnname"),
  (ExnMessage, "exnmessage"),
  (IsMLExn, "isML"),
(*
  (MonitorEnter, "monitorenter"),
  (MonitorExit, "monitorexit"),
*)
  (Mul, "mul"),
  (Neg, "neg"),
  (NewArray, "newarray"),
  (Or, "or"),
  (Rem, "rem"),
  (Shl, "shl"),
  (Shr, "shr"),
  (Sub, "sub"),
  (Test Tests.eq, "eq"),
  (Test Tests.ne, "ne"),
  (Test Tests.lt, "lt"),
  (Test Tests.gt, "gt"),
  (Test Tests.le, "le"),
  (Test Tests.ge, "ge"),
  (Ushr, "ushr"),
  (Xor, "xor"),
  (Pure, "pure")
]

val ops = Map.unionWith #1 (coreops, optops)

fun opTypeToString InvokeSpecial = "_invokespecial"
  | opTypeToString InvokeInterface = "_invokeinterface"
  | opTypeToString NopCast = "_nopcast"
  | opTypeToString SignExtend = "_signextend"
  | opTypeToString MonitorEnter = "_monitorenter"
  | opTypeToString MonitorExit = "_monitorexit"
  | opTypeToString optype =
    let val SOME s = Map.foldri 
      (fn (s,optype',r) => if optype = optype' then SOME ("_" ^ s) else r) 
    NONE 
    ops
in s end

fun exconToString (ExCon longid) = Pretty.longidToString longid
  | exconToString (ExJava e) = "\"" ^ e ^ "\""

fun intsToString [] = ""
  | intsToString (s::ss) = s ^ " " ^ intsToString ss

fun classInfoToString (modifiers, name, super, implements) =
  "_classtype " ^ name ^ 
  (case super of NONE => "" | SOME s => " _extends " ^ s) ^
  (case implements of [] => "" | ss => " _implements " ^ intsToString ss)

end

(*----------------------------------------------------------------------*)
(* Is c a character that can begin a Java identifier?			*)
(* Not sure whether we should allow "$".                                *)
(*----------------------------------------------------------------------*)
fun isJavaLetter c =
  (* c = #"$" orelse *) c = #"_" orelse Char.isAlpha c 

(*----------------------------------------------------------------------*)
(* Can c occur in a Java identifier after the first character?		*)
(*----------------------------------------------------------------------*)
fun isJavaLetterOrDigit c =
  isJavaLetter c orelse Char.isDigit c

(*----------------------------------------------------------------------*)
(* Is this a legal Java field or method name?				*)
(*----------------------------------------------------------------------*)
fun isLegalJava s = 
  size s <> 0
  andalso isJavaLetter (String.sub(s, 0))
  andalso CharVector.foldli 
    (fn (i,c,b) => isJavaLetterOrDigit c andalso b) true (s, 1, NONE)

(*----------------------------------------------------------------------*)
(* Is this a legal Java class name?					*)
(*----------------------------------------------------------------------*)
fun isLegalJavaClass s =
let
  val components as (h::t) = String.fields (fn c => c = #".") s
in
  List.all isLegalJava components
end
  
(*----------------------------------------------------------------------*)
(* Compare two Java constants.						*)
(* Note: only implemented for integral types and strings                *)
(*----------------------------------------------------------------------*)
fun compareConsts (c1,c2) =
  case (c1,c2) of
    (Constants.BYTE n1, Constants.BYTE n2) => JavaInt.numops.compare(n1,n2)
  | (Constants.INT n1, Constants.INT n2) => JavaInt.numops.compare(n1,n2)
  | (Constants.SHORT n1, Constants.SHORT n2) => JavaInt.numops.compare(n1,n2)
  | (Constants.CHAR n1, Constants.CHAR n2) => JavaInt.numops.compare(n1,n2)
  | (Constants.STRING s1, Constants.STRING s2) => JavaString.compare(s1,s2)
  | (Constants.LONG n1, Constants.LONG n2) => JavaLong.numops.compare(n1,n2)
  | (Constants.FLOAT _, Constants.FLOAT _) => LESS
  | (Constants.DOUBLE _, Constants.DOUBLE _) => LESS (* Hack *)
  | _ => Debug.fail ("JavaOps.compareConsts: " ^ 
    Constants.constant_toString c1 ^ " and " ^ Constants.constant_toString c2)
    
end
