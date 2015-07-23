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
(* Implementation of 32-bit words (the default word type)		*)
(* AJK, 17/10/97                                                        *)
(*======================================================================*)
structure Word : WORD =
struct

local 
  open General Bool Option 
  val op= = Prim.=
in

type word = Prim.word

val wordSize = 32

fun toLargeWordX w = Prim.toWord64(Prim.i2l (Prim.fromWord w))
fun toLargeWord w = Prim.toWord64(Prim.And(Prim.fromWord64 (toLargeWordX w), 
                Prim.fromWord64(0wxffffffff)))

fun fromLargeWord w = Prim.toWord(Prim.l2i (Prim.fromWord64 w))

fun fromInt i = Prim.toWord(i)

val fromLargeInt = MLJIntInfUtils.toWord

fun toLargeIntX x =
  MLJIntInfUtils.fromFixedInt(Prim.i2l (Prim.fromWord x))

fun toLargeInt w = 
  MLJIntInfUtils.fromFixedInt(Prim.fromWord64 (toLargeWord w))

fun toIntX w = Prim.fromWord w
fun toInt w = 
  if Prim.<>(Prim.And(Prim.fromWord w, Prim.fromWord (0wx80000000)), 0)
  then raise Overflow
  else Prim.fromWord w

fun << (i, n) = 
  if Prim.<>(Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffe0), 0)
  then 0w0
  else Prim.toWord(Prim.shl(Prim.fromWord i, Prim.fromWord n))

fun >> (i, n) = 
  if Prim.<>(Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffe0), 0)
  then 0w0
  else Prim.toWord(Prim.ushr(Prim.fromWord i, Prim.fromWord n))

fun ~>> (i, n) = 
  Prim.toWord(Prim.shr(Prim.fromWord i, 
  Prim.fromWord(
    if Prim.<>(Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffe0), 0)
    then 0wx1f else 0w0)))

fun compare (x, y) = 
  if x=y then EQUAL
  else 
  let
    val x' = Prim.fromWord64(toLargeWord x)
    val y' = Prim.fromWord64(toLargeWord y)
  in
    if Prim.lt (x',y') then LESS
    else GREATER
  end

fun x <= y = 
  Prim.le (Prim.fromWord64(toLargeWord x), Prim.fromWord64(toLargeWord y))
  
fun x >= y = 
  Prim.ge (Prim.fromWord64(toLargeWord x), Prim.fromWord64(toLargeWord y))
  
fun x < y = 
  Prim.lt (Prim.fromWord64(toLargeWord x), Prim.fromWord64(toLargeWord y))
  
fun x > y = 
  Prim.gt (Prim.fromWord64(toLargeWord x), Prim.fromWord64(toLargeWord y))

open MLJUtils.Word

fun x div y = Prim.toWord 
  (Prim.l2i
  (Prim.div(Prim.fromWord64 (toLargeWord x), Prim.fromWord64 (toLargeWord y))))
fun x mod y = Prim.toWord
  (Prim.l2i
  (Prim.rem(Prim.fromWord64 (toLargeWord x), Prim.fromWord64 (toLargeWord y))))

fun min (x, y) = if x < y then x else y
fun max (x, y) = if x < y then y else x

fun fmt radix i =
let
  val u = Prim.fromWord64 (toLargeWord i)
  val s = 
    Prim.unsafeValOf (java.lang.Long.toString(u,
      MLJBaseUtils.radixToBase radix))

  val s = 
      if radix=StringCvt.HEX 
      then Prim.unsafeValOf(s.#toUpperCase())
      else s
in
  s
end

fun scan radix getc src =
let
  val base = MLJBaseUtils.radixToBase radix

  val src = MLJUtils.String.trim0wx base getc src

  fun unsignedscan (n, src) =
  let
    fun done () = SOME (n, src)
  in
    case getc src of
      NONE => 
      done ()

    | SOME (c, src') =>
      let
        val i = MLJBaseUtils.fromDigit(c, base)
      in
        if Prim.lt (i, 0)
        then done ()
        else 
        let
          val next = fromInt i + n*fromInt base
        in
          unsignedscan (next, src')
        end
      end
  end
in
  case getc src of
    NONE => 
    NONE

  | SOME (c, src') =>
    let
      val i = MLJBaseUtils.fromDigit(c, base)
    in
      if Prim.lt (i, 0)
      then NONE
      else unsignedscan (fromInt i, src')
    end
end
    
val fromString = MLJUtils.String.scanString (scan StringCvt.HEX)

fun toString i = fmt StringCvt.HEX i

end (* of local open General Bool Option *)

end (* of struct *)
