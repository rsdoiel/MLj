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
(* Implementation of 32-bit integers (the default integer type)		*)
(* AJK, 17/10/97                                                        *)
(* We depart from the standard in not raising Overflow.                 *)
(*======================================================================*)
structure Int : INTEGER =
struct

local 
  open General Bool Option 
  val op= = Prim.=
in

type int = int

val precision = SOME 32
val maxInt    = SOME 2147483647
val minInt    = SOME ~2147483648

fun toInt   i = i : int
fun fromInt i = i : int

open MLJUtils.Int

fun sign i = if i < 0 then ~1 else if i > 0 then 1 else 0
fun compare (x, y) = 
  if x < y then LESS
  else if x > y then GREATER 
  else EQUAL

val fromLarge = MLJIntInfUtils.toInt

fun toLarge x = MLJIntInfUtils.fromFixedInt(Prim.i2l x)

(* Thanks to George for these *)
fun x div y=
   let
      val signx=if x<0 then ~1 else 1
      val signy=if y<0 then ~1 else 1
   in
      if signx=signy then quot(x,y)
      else 
         quot(x-y+signy,y)
   end

fun x mod y=
   let
      val signx=if x<0 then ~1 else 1
      val signy=if y<0 then ~1 else 1
   in
      if signx=signy then rem(x,y)
      else
         y-signy + rem(x+signy,y)
   end

fun abs (x:int) = java.lang.Math.abs(x)
fun min (x:int, y) = java.lang.Math.min(x, y)
fun max (x:int, y) = java.lang.Math.max(x, y)

fun sameSign (i, j) = sign i = sign j

fun fmt radix i =
let
  val s = Prim.unsafeValOf 
    (java.lang.Integer.toString(i, MLJBaseUtils.radixToBase radix))

  val s = if radix=StringCvt.HEX 
          then Prim.unsafeValOf(s.#toUpperCase ())
          else s
in
  Prim.unsafeValOf(s.#replace(#"-", #"~"))
end

fun scan radix getc src =
let
  val base = MLJBaseUtils.radixToBase radix

  fun posScan (n, src) =
  let
    val limit = quot(valOf maxInt, base)

    fun done n = SOME (if n < 0 then raise Overflow else n, src)

    fun loop (n, src) =
    case getc src of
      NONE => 
      done n

    | SOME (c, src') =>
      let
        val i = MLJBaseUtils.fromDigit(c, base)
      in
        if i < 0
        then done n
        else 
        if n > limit then raise Overflow
        else loop (n*base + i, src')
      end
  in
    loop (n, src)
  end

  fun negScan (n, src) =
  let
    val limit = quot(valOf minInt, base)

    fun done n = SOME (if n > 0 then raise Overflow else n, src)

    fun loop (n,src) =
    case getc src of
      NONE => 
      done n

    | SOME (c, src') =>
      let
        val i = MLJBaseUtils.fromDigit(c, base)
      in
        if i < 0
        then done n
        else 
        if n < limit then raise Overflow
        else loop (n*base - i, src')
      end
  in
    loop (n, src)
  end
     
  val (neg, src) = MLJUtils.String.trimSign getc src
  val src = 
    if radix=StringCvt.HEX then MLJUtils.String.trim0x getc src else src
in
  case getc src of
    NONE => NONE
  | SOME (c,src) =>
    let
      val i = MLJBaseUtils.fromDigit(c, base)
    in
      if i < 0 then NONE
      else
      if neg 
      then negScan (~i, src)
      else posScan (i, src)
    end
end
    
fun toString i = fmt StringCvt.DEC i

val fromString = MLJUtils.String.scanString (scan StringCvt.DEC)

end (* of local open General Bool Option *)

end (* of struct *)
