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
(* Implementation of 64-bit integers                           		*)
(* AJK, 17/10/97                                                        *)
(* We depart from the standard in not raising Overflow.                 *)
(*======================================================================*)
structure FixedInt : INTEGER =
struct

local 
  open General Bool Option 
  val op= = Prim.=
in

type int = Prim.long

val precision = SOME (64 : int)
val maxInt    = SOME ( 9223372036854775807 : int)
val minInt    = SOME (~9223372036854775808 : int)

val toLarge = MLJIntInfUtils.fromFixedInt
val fromLarge = MLJIntInfUtils.toFixedInt

fun toInt i = 
  if MLJUtils.FixedInt.<(i, Prim.i2l (valOf Int32.minInt))
  then raise Overflow
  else if MLJUtils.FixedInt.>(i, Prim.i2l (valOf Int32.maxInt))
  then raise Overflow
  else Prim.l2i i

fun fromInt i = Prim.i2l i

open MLJUtils.FixedInt

fun sign i = Prim.cmp(i, 0)

fun compare (x, y) = 
  case Prim.cmp(x, y) of
    ~1 => LESS
   | 0 => EQUAL
   | _ => GREATER

fun x div y =
   let
      val signx=if x<0 then ~1 else 1:int
      val signy=if y<0 then ~1 else 1:int
   in
      if signx=signy then quot(x,y)
      else 
         quot(x-y+signy,y)
   end

fun x mod y =
   let
      val signx=if x<0 then ~1 else 1:int
      val signy=if y<0 then ~1 else 1:int
   in
      if signx=signy then rem(x,y)
      else
         y-signy + rem(x+signy,y)
   end


fun abs x = java.lang.Math.abs(x:int)
fun min (x, y) = java.lang.Math.min(x:int, y:int)
fun max (x, y) = java.lang.Math.max(x:int, y:int)

fun sameSign (i, j) = sign i = sign j

fun fmt radix (i:int) =
let
  val s = 
  Prim.unsafeValOf (
    java.lang.Long.toString(i, MLJBaseUtils.radixToBase radix))

  val s = 
      if radix=StringCvt.HEX 
      then Prim.unsafeValOf(s.#toUpperCase())
      else s

  val s = Prim.unsafeValOf(s.#replace(#"-", #"~"))
in
  s
end

fun scan radix getc src =
let
  val base = MLJBaseUtils.radixToBase radix

  fun posScan (n, src) =
  let
    val limit = quot(valOf maxInt, fromInt base)

    fun done n = SOME (if n < 0 then raise Overflow else n, src)

    fun loop (n, src) =
    case getc src of
      NONE => 
      done n

    | SOME (c, src') =>
      let
        val i = MLJBaseUtils.fromDigit(c, base)
      in
        if Int.<(i, 0)
        then done n
        else 
        if n > limit then raise Overflow
        else loop (n*fromInt base + fromInt i, src')
      end
  in
    loop (n, src)
  end

  fun negScan (n, src) =
  let
    val limit = quot(valOf minInt, fromInt base)

    fun done n = SOME (if n > 0 then raise Overflow else n, src)

    fun loop (n,src) =
    case getc src of
      NONE => 
      done n

    | SOME (c, src') =>
      let
        val i = MLJBaseUtils.fromDigit(c, base)
      in
        if Int.<(i, 0)
        then done n
        else 
        if n < limit then raise Overflow
        else loop (n*fromInt base - fromInt i, src')
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
      if Int.<(i, 0) then NONE
      else
      if neg 
      then negScan (~(fromInt i), src)
      else posScan (fromInt i, src)
    end
end
    
fun toString i = fmt StringCvt.DEC i
val fromString = MLJUtils.String.scanString (scan StringCvt.DEC)

end (* of local open General Bool Option *)

end (* of struct *)
