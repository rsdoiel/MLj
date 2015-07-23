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
(* Primitive operations visible only to Basis writers.			*)
(*======================================================================*)
signature MLj =
sig

type boolean = bool
type byte   (* stamp 1 *)
type char   (* stamp 2 *)
type double (* stamp 3 *)
type float  (* stamp 4 *)
type int    (* stamp 5 *)
type long   (* stamp 6 *)
type short  (* stamp 7 *)

(*----------------------------------------------------------------------*)
(* JVM bytecodes							*)
(*----------------------------------------------------------------------*)
val add : int * int -> int 
and add : byte * byte -> byte
and add : short * short -> short
and add : double * double -> double
and add : long * long -> long
and add : float * float -> float

val sub : int * int -> int 
and sub : byte * byte -> byte
and sub : short * short -> short
and sub : double * double -> double
and sub : long * long -> long
and sub : float * float -> float

val mul : int * int -> int 
and mul : byte * byte -> byte
and mul : short * short -> short
and mul : double * double -> double
and mul : long * long -> long
and mul : float * float -> float

val div : int * int -> int 
and div : double * double -> double
and div : long * long -> long
and div : float * float -> float

val rem : int * int -> int 
and rem : double * double -> double
and rem : long * long -> long
and rem : float * float -> float

val neg : int -> int 
and neg : double -> double
and neg : long -> long
and neg : float -> float

val lt : int * int -> boolean
and lt : byte * byte -> bool
and lt : short * short -> boolean
and lt : long * long -> boolean

val gt : int * int -> boolean
and gt : byte * byte -> boolean
and gt : short * short -> boolean
and gt : long * long -> boolean

val le : int * int -> boolean
and le : byte * byte -> boolean
and le : short * short -> boolean
and le : long * long -> boolean

val ge : int * int -> boolean
and ge : byte * byte -> boolean
and ge : short * short -> boolean
and ge : long * long -> boolean

val eq : double * double -> boolean
and eq : float * float -> boolean

val And : int * int -> int
and And : long * long -> long

val or : int * int -> int
and or : long * long -> long

val xor : int * int -> int
and xor : long * long -> long

val shl : int * int -> int
and shl : long * int -> long

val shr : int * int -> int
and shr : long * int -> long

val ushr : int * int -> int
and ushr : long * int -> long

val arraylength : 'a array -> int
val arrayload : 'a array * int -> 'a
val arraystore : 'a array * int * 'a -> unit
val newarray : int -> 'a array

(*----------------------------------------------------------------------*)
(* Some additional operations required for implementing the Basis       *)
(*----------------------------------------------------------------------*)
val exnMessage : exn -> string
val exnName : exn -> string
val isMLExn : exn -> bool

(*----------------------------------------------------------------------*)
(* Some unsafe coercions						*)
(*----------------------------------------------------------------------*)
val fromVector : 'a vector -> 'a array
val toVector : 'a array -> 'a vector
val unsafeValOf : 'a option -> 'a

(*----------------------------------------------------------------------*)
(* Primitive pure ML operations not implementable directly.             *)
(*----------------------------------------------------------------------*)
val ref : 'a ref -> 'a
val := : 'a ref * 'a -> unit
val ! : 'a ref -> 'a
val = : ''a * ''a -> bool
val <> : ''a * ''a -> bool

end
