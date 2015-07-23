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
signature JAVAREPOPS =
sig

(*----------------------------------------------------------------------*)
(* Do two Java representations stand for the same Java type (that is,	*)
(* would they be given the same descriptor) ?                           *)
(*----------------------------------------------------------------------*)
val eq        : JavaRep.Rep * JavaRep.Rep -> bool

(*----------------------------------------------------------------------*)
(* Translate a Java representation into a Java type, Java class or      *)
(* Java primitive type.                                                 *)
(*----------------------------------------------------------------------*)
val toJava    : JavaRep.Rep -> Types.java_type
val toClass   : JavaRep.Rep -> ClassHandle.Handle
val toBase    : JavaRep.Rep -> Types.base_type

(*----------------------------------------------------------------------*)
(* Pretty-print a Java representation					*)
(*----------------------------------------------------------------------*)
val toString  : JavaRep.Rep -> string

(*----------------------------------------------------------------------*)
(* Is a representation a pointer type?					*)
(*----------------------------------------------------------------------*)
val isPointer : JavaRep.Rep -> bool

(*----------------------------------------------------------------------*)
(* Can a representation be treated as an integer? (true of "int",       *)
(* "char", "bool", "byte" and "short")                                 	*)
(*----------------------------------------------------------------------*)
val isInt     : JavaRep.Rep -> bool

(*----------------------------------------------------------------------*)
(* Null value for this type         					*)
(*----------------------------------------------------------------------*)
val nullValue : JavaRep.Rep -> Constants.constant

(*----------------------------------------------------------------------*)
(* Normalise a Java representation so that it is not RepRef		*)
(*----------------------------------------------------------------------*)
val normalise : JavaRep.Rep -> JavaRep.Rep

(*----------------------------------------------------------------------*)
(* `Unknown' Java class representation					*)
(*----------------------------------------------------------------------*)
val class     : string -> JavaRep.Rep
val int       : JavaRep.Rep
val bool      : JavaRep.Rep
val object    : JavaRep.Rep
val exn       : JavaRep.Rep

end