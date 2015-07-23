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
(* Implementation of 8-bit integers (i.e. Java bytes)          		*)
(* In the JVM, these are represented by sign-extended 32-bit integers.  *)
(*======================================================================*)
structure Int8 : INTEGER =
struct

local 
  open General Bool Option 
  val op= = Prim.=
in

type int = Prim.byte

val precision = SOME (8 : int)
val maxInt    = SOME (127 : int)
val minInt    = SOME (~128 : int)

fun toInt   i = Prim.b2i(i)
fun fromInt i = Prim.i2b(i)

fun x < y  = Prim.lt(Prim.b2i x,Prim.b2i y)
fun x > y  = Prim.gt(Prim.b2i x,Prim.b2i y)
fun x <= y = Prim.le(Prim.b2i x,Prim.b2i y)
fun x >= y = Prim.ge(Prim.b2i x,Prim.b2i y)

fun x + y = Prim.i2b (Prim.add(Prim.b2i x, Prim.b2i y))
fun x - y = Prim.i2b (Prim.sub(Prim.b2i x, Prim.b2i y))
fun ~x    = Prim.neg(x : int)
fun x * y = Prim.i2b (Prim.mul(Prim.b2i x, Prim.b2i y))

fun quot (x,y) = Prim.i2b (Prim.div(Prim.b2i x, Prim.b2i y))
fun rem  (x,y) = Prim.i2b (Prim.rem(Prim.b2i x, Prim.b2i y))

fun sign (i:int) = if i < 0 then ~1 else if i > 0 then 1 else 0
fun compare (x, y) = 
  if x < y then LESS
  else if x > y then GREATER 
  else EQUAL

fun fromLarge x = Prim.i2b (MLJIntInfUtils.toInt x)

fun toLarge x = MLJIntInfUtils.fromInt(Prim.b2i x)

(* Thanks to George for these *)
fun (x:int) div (y:int) =
   let
      val signx=if x<0 then ~1 else 1:int
      val signy=if y<0 then ~1 else 1:int
   in
      if signx=signy then quot(x,y)
      else 
         quot(x-y+signy,y)
   end

fun (x:int) mod (y:int) =
   let
      val signx=if x<0 then ~1 else 1:int
      val signy=if y<0 then ~1 else 1:int
   in
      if signx=signy then rem(x,y)
      else
         y-signy + rem(x+signy,y)
   end

fun abs x = Prim.i2b (java.lang.Math.abs(Prim.b2i x))
fun min (x, y) = Prim.i2b (java.lang.Math.min(Prim.b2i x, Prim.b2i y))
fun max (x, y) = Prim.i2b (java.lang.Math.max(Prim.b2i x, Prim.b2i y))

fun sameSign (i, j) = sign i = sign j

fun fmt radix i = Int.fmt radix (Prim.b2i i)

fun scan radix getc src =
case Int.scan radix getc src of
  NONE => NONE
| SOME (i, src) => 
  if Prim.lt(i, ~128) orelse Prim.gt(i,127)
  then raise Overflow
  else SOME(Prim.i2b i, src)
    
fun toString i = fmt StringCvt.DEC i

val fromString = MLJUtils.String.scanString (scan StringCvt.DEC)

end (* of local open General Bool Option *)

end (* of struct *)
