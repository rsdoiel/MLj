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
(* Various names for labels, methods and classes                        *)
(*======================================================================*)
structure JavaNames :> JAVANAMES =
struct

open Names

(*----------------------------------------------------------------------*)
(* Label used for i'th component of a product, sum constructor,         *)
(* exception constructor, closure environment or list of globals.       *)
(*----------------------------------------------------------------------*)
fun argLabel i = indexToAlpha i

fun class s = ClassHandle.unknown (JavaString.fromString s)

(*----------------------------------------------------------------------*)
(* The class holding all global variables				*)
(*----------------------------------------------------------------------*)
val globalClassName = "G"
val globalClass = class globalClassName

(*----------------------------------------------------------------------*)
(* Class of i'th product type						*)
(*----------------------------------------------------------------------*)
fun prodClassName i = "R" ^ indexToLowerNum i
val prodClass = class o prodClassName

(*----------------------------------------------------------------------*)
(* Class of i'th class type						*)
(*----------------------------------------------------------------------*)
fun classClassName i = "C" ^ indexToLowerNum i
val classClass = class o classClassName

(*----------------------------------------------------------------------*)
(* Class of i'th sum constructor 					*)
(*----------------------------------------------------------------------*)
fun conClassName i = 
  "S" ^ (case i of NONE => "" | SOME i => indexToLowerNum i)
val conClass = class o conClassName

(*----------------------------------------------------------------------*)
(* Class of i'th exception constructor   				*)
(*----------------------------------------------------------------------*)
fun exnClassName i = 
  "E" ^ (case i of NONE => "" | SOME i => indexToLowerNum i)
val exnClass = class o exnClassName

(*----------------------------------------------------------------------*)
(* Class of i'th closure                             			*)
(*----------------------------------------------------------------------*)
fun closClassName i = 
  "F" ^ (case i of NONE => "" | SOME i => indexToLowerNum i)
val closClass = class o closClassName

(*----------------------------------------------------------------------*)
(* Name used for a particular apply method 				*)
(* Namespaces of methods and fields are separate so we can share names. *)
(*----------------------------------------------------------------------*)
fun appMethod i = indexToAlpha i                  

(*----------------------------------------------------------------------*)
(* Name used for the integer tag in a sum class				*)
(*----------------------------------------------------------------------*)
val sumTag = "$"

(*----------------------------------------------------------------------*)
(* Name used for the integer count in a generative exception class      *)
(*----------------------------------------------------------------------*)
val exnClassCount = "$"

(*----------------------------------------------------------------------*)
(* Name of the i'th global method			              	*)
(* Namespaces of methods and fields are separate so we can share names. *)
(*----------------------------------------------------------------------*)
fun globalMethod i = indexToAlpha i

(*----------------------------------------------------------------------*)
(* Name of the i'th NONE value						*)
(*----------------------------------------------------------------------*)
fun noneVal i = "_" ^ indexToAlpha i

val exnLocMessage = "$l"
val exnNameMethod = "$n"
val exnMessageMethod = "$m"
val allocCount = "$c"

end

