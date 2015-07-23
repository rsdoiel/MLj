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
(* Operations on Java class definitions                               	*)
(*======================================================================*)
structure SMLClassDefOps :> SMLCLASSDEFOPS =
struct

exception ClassNotFound of string

open TransJava

(*----------------------------------------------------------------------*)
(* Pretty-print class, field and method modifiers.			*)
(*----------------------------------------------------------------------*)
local 
  open Class
in
  fun classFlagToString flag =
    case flag of
      PUBLIC => "public"
    | FINAL => "final"
    | INTERFACE => "interface"
    | ABSTRACT => "abstract"
end
  
local 
  open Field
in
  fun fieldFlagToString flag =
    case flag of
      PUBLIC => "public"
    | PRIVATE => "private"
    | PROTECTED => "protected"
    | STATIC => "static"
    | FINAL => "final"
    | VOLATILE => "volatile"
    | TRANSIENT => "transient"
end
  
local 
  open Method
in
  fun methodFlagToString flag =
    case flag of
      PUBLIC => "public"
    | PRIVATE => "private"
    | PROTECTED => "protected"
    | STATIC => "static"
    | FINAL => "final"
    | SYNCHRONIZED => "synchronized"
    | NATIVE => "native"
    | ABSTRACT => "abstract"
end
  

(*----------------------------------------------------------------------*)
(* Cache of external class defs						*)
(*----------------------------------------------------------------------*)
type Cache = SMLTy.ClassDef Symbol.OrdMap.map 
val cache = ref (Symbol.OrdMap.empty : Cache)

(*----------------------------------------------------------------------*)
(* Convert backend method info into our own representation		*)
(*----------------------------------------------------------------------*)
fun methodInfo 
  (Method.simple { name, flags, method_type = Descriptors.M(tyopt, tys),... })=
  (name, flags, map javaToML tys, Option.map javaToML tyopt)

(*----------------------------------------------------------------------*)
(* Convert backend field info into our own representation		*)
(*----------------------------------------------------------------------*)
fun fieldInfo 
  (Field.simple { name, flags, field_type, attributes}) =
  (name, flags, javaToML field_type, Attribute.getConstantValue attributes)
             
(*----------------------------------------------------------------------*)
(* Decode a class.                                                      *)
(*----------------------------------------------------------------------*)
fun doDecode 
  (longid,Class.middling { this,flags,super,interfaces,fields,methods,... }) =

let
  val _ = 
    if Controls.isOn "showClassImport"
    then PrintManager.print ("\n[Loading class " ^ 
      Pretty.longidToString longid ^ "] ")
    else ()
in
  (
    (flags,Option.map classToML super, map classToML interfaces), 
    map fieldInfo fields, 
    map methodInfo methods
  )
end

fun longidToClass longid =
let
  val jid::jlongid = map Symbol.toJavaString longid
  val jstrs = 
    jid :: foldr (fn (jid,jstrs) => JavaString.fromString "/" :: jid :: jstrs)
    [] jlongid
  val jclassname = JavaString.concat jstrs
in
  jclassname
end

fun classToLongid js =
let
  val s = JavaString.toMLString js
  val ss = String.tokens (fn c => c = #"/" orelse c = #".") s
in
  map Ids.symbol ss
end


fun decode longid =
let
  val filename = longidToClass longid
  val sym = Symbol.symbol filename 
in
  case Symbol.OrdMap.find(!cache, sym) of
    SOME result => 
    SOME result
(*
    (case PackageManager.getClass longid of
      NONE => NONE
    | SOME data =>
      let 
        val result = doDecode (longid, data)
      in
        cache := Symbol.OrdMap.insert(!cache, sym, result); 
        SOME result
      end)
*)

  | NONE => 
    case PackageManager.getClass longid of
      SOME data =>
      let val result = doDecode (longid, data)
      in
        cache := Symbol.OrdMap.insert(!cache, sym, result); 
        SOME result
      end

    | NONE => 
      NONE
end

(*----------------------------------------------------------------------*)
(* Return the class definition for a given class if it can be found.    *)
(*----------------------------------------------------------------------*)
fun tyToClassDef CE ty =
  case SMLTy.fromConsType ty of
    SOME ([], tyname) =>
    (case TyName.fromExternalClass tyname of
      SOME longid =>
      (case decode longid of
        NONE => 
        raise ClassNotFound (SMLTy.toString ty)

      | SOME classdef => 
        classdef)

    | NONE =>
      (case TyName.Map.find(CE, tyname) of
        SOME classdef =>
        classdef
      | NONE =>
        raise ClassNotFound (SMLTy.toString ty))
    )

  | _ =>
    Debug.fail ("SMLClassDefOps.tyToClassDef: non-class type: " ^ 
      SMLTy.toString ty)

fun checkExplicit longid = isSome (decode longid)

(*----------------------------------------------------------------------*)
(* Return true if ty11 is a subclass of ty2 or if ty1                   *)
(* implements the interface defined by ty2.       			*)
(*----------------------------------------------------------------------*)
fun subClass E (ty1, ty2) =
let
  fun search root =
    SMLTy.eq(root, ty2)
    orelse
    (let
      val ((_,superopt,ints),_,_) = tyToClassDef E root
    in
      (case superopt of
        NONE => 
        false

      | SOME super => search super)
      orelse List.exists search ints
    end)
in
  search ty1
end

(*----------------------------------------------------------------------*)
(* Return true if this classdef has a main method of appropriate type.	*)
(*----------------------------------------------------------------------*)
fun hasMain ((classflags,_,_), _, methods) = 
let
  fun isMain (name, flags, [argty], NONE) = 
    JavaString.equal(name, JavaString.fromString "main") andalso
    List.exists (fn Method.PUBLIC => true | _ => false) flags
  andalso List.exists (fn Method.STATIC => true | _ => false) flags
    | isMain _ = false
in
  List.exists (fn Class.PUBLIC => true | _ => false) classflags
  andalso List.exists isMain methods
end




end

