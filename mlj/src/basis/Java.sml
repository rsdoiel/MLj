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
(* Java structure provided for backwards compatability with MLJ 0.1	*)
(* NOTE: future releases of MLJ will not include this structure.	*)
(*======================================================================*)
structure Java = 
struct

type boolean = Prim.boolean
type byte = Prim.byte
type char = Prim.char
type double = Prim.double
type float = Prim.float
type int = Prim.int
type long = Prim.long
type short = Prim.short
type 'a array = 'a array

type BigInteger = java.math.BigInteger
type Boolean = java.lang.Boolean
type Character = java.lang.Character
type Double = java.lang.Double
type Exception = java.lang.Exception
type Float = java.lang.Float
type Integer = java.lang.Integer
type Long = java.lang.Long
type Object = java.lang.Object
type String = java.lang.String
type Throwable = java.lang.Throwable

fun fromArray (x : 'a array) = x
val toArray = fromArray

fun fromBool (x : bool) = x
val toBool = fromBool

fun fromChar (c : char) = c
val toChar = fromChar

fun fromExn (e : exn) = e
val toExn = fromExn

fun fromInt (i : int) = i
val toInt = fromInt

fun fromInt64 (i : long) = i
val toInt64 = fromInt64

fun fromIntInf (i : BigInteger) = i
val toIntinf = fromIntInf

fun fromReal (r : real) = r
val toReal = fromReal

fun fromString (s : string) = s
val toString = fromString

val fromVector = Prim.fromVector
val toVector = Prim.toVector

val fromWord = Prim.fromWord
val toWord = Prim.toWord

val fromWord8 = Prim.fromWord8
val toWord8 = Prim.toWord8

val fromWord64 = Prim.fromWord64
val toWord64 = Prim.toWord64

val unsafeValOf = Option.valOf

end
