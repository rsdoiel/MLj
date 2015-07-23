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
(* Implementation of 16-bit integers (i.e. Java shorts)        		*)
(* In the JVM, these are represented by sign-extended 32-bit integers.  *)
(*======================================================================*)
structure Int16 : INTEGER =
struct

local 
  open General Bool Option 
  val op= = Prim.=
in

type int = Prim.short

val precision = SOME (16 : int)
val maxInt    = SOME (32767 : int)
val minInt    = SOME (~32768 : int)

fun toInt   i = Prim.s2i(i)
fun fromInt i = Prim.i2s(i)

fun x < y  = Prim.lt(Prim.s2i x,Prim.s2i y)
fun x > y  = Prim.gt(Prim.s2i x,Prim.s2i y)
fun x <= y = Prim.le(Prim.s2i x,Prim.s2i y)
fun x >= y = Prim.ge(Prim.s2i x,Prim.s2i y)

fun x + y = Prim.i2s (Prim.add(Prim.s2i x, Prim.s2i y))
fun x - y = Prim.i2s (Prim.sub(Prim.s2i x, Prim.s2i y))
fun ~x    = Prim.neg(x : int)
fun x * y = Prim.i2s (Prim.mul(Prim.s2i x, Prim.s2i y))

fun quot (x,y) = Prim.i2s (Prim.div(Prim.s2i x, Prim.s2i y))
fun rem  (x,y) = Prim.i2s (Prim.rem(Prim.s2i x, Prim.s2i y))

fun sign (i:int) = if i < 0 then ~1 else if i > 0 then 1 else 0
fun compare (x, y) = 
  if x < y then LESS
  else if x > y then GREATER 
  else EQUAL

fun fromLarge x = Prim.i2s (MLJIntInfUtils.toInt x)

fun toLarge x = MLJIntInfUtils.fromInt(Prim.s2i x)

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

fun abs x = Prim.i2s (java.lang.Math.abs(Prim.s2i x))
fun min (x, y) = Prim.i2s (java.lang.Math.min(Prim.s2i x, Prim.s2i y))
fun max (x, y) = Prim.i2s (java.lang.Math.max(Prim.s2i x, Prim.s2i y))

fun sameSign (i, j) = sign i = sign j

fun fmt radix i = Int.fmt radix (Prim.s2i i)

fun scan radix getc src =
case Int.scan radix getc src of
  NONE => NONE
| SOME (i, src) => 
  if Prim.lt(i, ~32768) orelse Prim.gt(i,32767)
  then raise Overflow
  else SOME(Prim.i2s i, src)
    
fun toString i = fmt StringCvt.DEC i

val fromString = MLJUtils.String.scanString (scan StringCvt.DEC)

end (* of local open General Bool Option *)

end (* of struct *)
