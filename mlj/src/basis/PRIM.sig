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
(* Primitive types and operations visible only to Basis writers.	*)
(*======================================================================*)
signature PRIM =
sig

(*----------------------------------------------------------------------*)
(* Pervasive ML types							*)
(*----------------------------------------------------------------------*)
eqtype 'a vector				(* Stamp 1 *)

(*----------------------------------------------------------------------*)
(* Base ML/Java types						        *)
(* Note: array and ref are missing because they have special equality   *)
(* status.                                                              *)
(*----------------------------------------------------------------------*)
type boolean = Datatypes.bool
eqtype byte					(* Stamp 2 *)
eqtype char   					(* Stamp 3 *)
type double 					(* Stamp 4 *)
type float  					(* Stamp 5 *)
eqtype int    					(* Stamp 6 *)
eqtype long   					(* Stamp 7 *)
eqtype short  					(* Stamp 8 *)

eqtype word					(* Stamp 9 *)
eqtype word8                                    (* Stamp 10 *)
eqtype word64                                   (* Stamp 11 *)

type string = java.lang.String
type exn = java.lang.Exception
type unit = {}

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
and div : byte * byte -> byte
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
and neg : byte -> byte
and neg : short -> short

val lt : int * int -> boolean
and lt : byte * byte -> boolean
and lt : short * short -> boolean
and lt : long * long -> boolean
and lt : char * char -> boolean
and lt : float * float -> boolean
and lt : double * double -> boolean

val gt : int * int -> boolean
and gt : byte * byte -> boolean
and gt : short * short -> boolean
and gt : long * long -> boolean
and gt : char * char -> boolean
and gt : float * float -> boolean
and gt : double * double -> boolean

val le : int * int -> boolean
and le : byte * byte -> boolean
and le : short * short -> boolean
and le : long * long -> boolean
and le : char * char -> boolean
and le : float * float -> boolean
and le : double * double -> boolean

val ge : int * int -> boolean
and ge : byte * byte -> boolean
and ge : short * short -> boolean
and ge : long * long -> boolean
and ge : char * char -> boolean
and ge : float * float -> boolean
and ge : double * double -> boolean

val eq : double * double -> boolean
and eq : float * float -> boolean

val And : int * int -> int
and And : long * long -> long
and And : byte * byte -> byte

val or : int * int -> int
and or : long * long -> long
and or : byte * byte -> byte

val xor : int * int -> int
and xor : long * long -> long
and xor : byte * byte -> byte

val shl : int * int -> int
and shl : long * int -> long

val shr : int * int -> int
and shr : long * int -> long

val ushr : int * int -> int
and ushr : long * int -> long

val cmpl : float * float -> int
and cmpl : double * double -> int

val cmpg : float * float -> int
and cmpg : double * double -> int

val cmp : long * long -> int

val arraylength : 'a array -> int
val arrayload : 'a array * int -> 'a
val arraystore : 'a array * int * 'a -> unit
val newarray : int -> 'a array

val d2f : double -> float
val d2i : double -> int
val d2l : double -> long

val f2d : float -> double
val f2i : float -> int
val f2l : float -> long

val i2b : int -> byte
val i2c : int -> char
val i2d : int -> double
val i2f : int -> float
val i2l : int -> long
val i2s : int -> short

val l2d : long -> double
val l2f : long -> float
val l2i : long -> int

(*----------------------------------------------------------------------*)
(* Some additional operations required for implementing the Basis       *)
(*----------------------------------------------------------------------*)
val exnMessage : exn -> string
val exnName : exn -> string
val isMLExn : exn -> bool

(*----------------------------------------------------------------------*)
(* Some no-op coercions 						*)
(*----------------------------------------------------------------------*)
val fromVector : 'a vector -> 'a array
val toVector : 'a array -> 'a vector
val unsafeValOf : 'a option -> 'a

val fromWord : word -> int
val toWord : int -> word
val fromWord64 : word64 -> long
val toWord64 : long -> word64
val fromWord8 : word8 -> byte
val toWord8 : byte -> word8

val c2i : char -> int
val b2i : byte -> int
val s2i : short -> int

(*----------------------------------------------------------------------*)
(* Primitive pure ML operations not implementable directly.             *)
(*----------------------------------------------------------------------*)
val ref : 'a -> 'a ref
val := : 'a ref * 'a -> unit
val ! : 'a ref -> 'a
val = : ''a * ''a -> bool
val <> : ''a * ''a -> bool

end
