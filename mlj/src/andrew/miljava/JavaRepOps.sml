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
(* Auxiliary functions on Java representations. 			*)
(*======================================================================*)
structure JavaRepOps :> JAVAREPOPS =
struct

local 
  open JavaRep JavaNames
in
  
fun normalise r =
case r of
  RepRef (ref (SOME r)) => normalise r
| _ => r

(*----------------------------------------------------------------------*)
(* Do two representations have the same Java signature?                 *)
(* Distinct (normalised) representations product distinct signatures,   *)
(*----------------------------------------------------------------------*)
fun eq (r1,r2) =
case (normalise r1, normalise r2) of
  (Object, Object) => true
| (Java base1, Java base2) => Types.BaseKey.compare(base1, base2) = EQUAL
| (Prod i1, Prod i2) => i1=i2
| (Array rep1, Array rep2) => eq (rep1, rep2)
| (Con i1, Con i2) => i1=i2
| (Exn i1, Exn i2) => i1=i2
| (Closure i1, Closure i2) => i1=i2
| (Class i1, Class i2) => i1=i2
| (RepRef r1, RepRef r2) => r1=r2
| _ => false

(*----------------------------------------------------------------------*)
(* Pretty-print a Java representation					*)
(*----------------------------------------------------------------------*)
fun toString rep =
case normalise rep of
  Object => "Object"
| Java (Types.CLASS c) => ClassHandle.class_handle_toString c
| Java base => Types.base_type_toString base
| Prod i => JavaNames.prodClassName i
| Array rep => "(" ^ toString rep ^ " array" ^ ")"
| Con iopt => JavaNames.conClassName iopt
| Exn iopt => JavaNames.exnClassName iopt
| Closure iopt => JavaNames.closClassName iopt
| RepRef (ref NONE) => "^"
| Class i => JavaNames.classClassName i
| _ => 
  Debug.fail "JavaRepOps.toString: non-normalised representation"

(*----------------------------------------------------------------------*)
(* Convert a Java representation into a back-end type		        *)
(*----------------------------------------------------------------------*)
fun toJava rep =
case normalise rep of
  Object => 
  Types.F(0, Types.CLASS (ClassHandle.object))

| Java base =>
  Types.F(0, base)

| Prod i =>
  Types.F(0, Types.CLASS (prodClass i))

| Con iopt =>
  Types.F(0, Types.CLASS (conClass iopt))

| Closure iopt =>
  Types.F(0, Types.CLASS (closClass iopt))

| Exn iopt =>
  Types.F(0, Types.CLASS (exnClass iopt))

| Class i =>
  Types.F(0, Types.CLASS (classClass i))

| Array rep =>
  let  
    val Types.F(n, b) = toJava rep
  in 
    Types.F(n+1, b) 
  end  

| _ =>
  Debug.fail 
  ("JavaRepOps.repToJava: non-normalised representation " ^ toString rep)


fun toClass rep =
case toJava rep of
  Types.F(0, Types.CLASS class) => class
| _ => Debug.fail 
  ("JavaRepOps.toClass: non-class representation " ^ toString rep)

fun toBase rep =
case toJava rep of
  Types.F(0, base) => base
| _ => Debug.fail 
  ("JavaRepOps.toBase: non-base type representation " ^ toString rep)

fun isPointer rep =
case normalise rep of
  Java (Types.CLASS _) => true
| Java _ => false
| _ => true

fun isInt rep =
case normalise rep of
  Java Types.INT => true
| Java Types.CHAR => true
| Java Types.SHORT => true
| Java Types.BOOLEAN => true
| Java Types.BYTE => true
| _ => false
  
fun nullValue rep =
case normalise rep of
  Java Types.BOOLEAN => Constants.BOOLEAN (JavaInt.fromInt 0)
| Java Types.BYTE => Constants.BYTE (JavaInt.fromInt 0)
| Java Types.CHAR => Constants.CHAR (JavaInt.fromInt 0)
| Java Types.DOUBLE => Constants.DOUBLE (JavaDouble.fromReal 0.0)
| Java Types.FLOAT => Constants.FLOAT (JavaFloat.fromReal 0.0)
| Java Types.INT => Constants.INT (JavaInt.fromInt 0)
| Java Types.LONG => Constants.LONG (JavaLong.fromInt 0)
| Java Types.SHORT => Constants.SHORT (JavaInt.fromInt 0)
| _ => Constants.NULL
  
fun class c = 
  Java (Types.CLASS (ClassHandle.unknown (JavaString.fromString c)))
val int = Java Types.INT
val bool = Java Types.BOOLEAN
val exn = Java (Types.CLASS (ClassHandle.Exception))
val object = Object

end (* of local *)
end (* of structure *)
