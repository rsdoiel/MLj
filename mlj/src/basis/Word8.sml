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
(* Word8.word is represented as Java.byte, with the high-order bits 	*)
(* containing arbitrary junk. Thus various functions have to            *)
(* normalise their arguments.                                           *)
(*======================================================================*)
structure Word8 : WORD = 
struct 

local 
  open General Bool Option 
  val op= = Prim.=
in

type word = Prim.word8

val wordSize = 8

(*----------------------------------------------------------------------*)
(* Convert a Java.byte into a Java.int in the range 0-255 by zeroing    *)
(* the high-order bits.	                                                *)
(*----------------------------------------------------------------------*)
fun normalise x = Prim.And(Prim.b2i (Prim.fromWord8 x), Prim.fromWord 0wxff)

(*----------------------------------------------------------------------*)
(* All these operations preserve equivalence of representation		*)
(*----------------------------------------------------------------------*)
fun orb (x : word, y : word) = 
  Prim.toWord8(Prim.or(Prim.fromWord8 x, Prim.fromWord8 y))
fun xorb (x : word, y : word) = 
  Prim.toWord8(Prim.xor(Prim.fromWord8 x, Prim.fromWord8 y))
fun andb (x : word, y : word) = 
  Prim.toWord8(Prim.And(Prim.fromWord8 x, Prim.fromWord8 y))

(*----------------------------------------------------------------------*)
(* XOR with 0wxffffffff; there's a short bytecode for -1.        	*)
(*----------------------------------------------------------------------*)
fun notb (x : word) = 
  Prim.toWord8(Prim.xor(Prim.fromWord8 x, Prim.i2b(Prim.fromWord 0wxffffffff)))

(*----------------------------------------------------------------------*)
(* Sign extension done by bytecode i2l					*)
(*----------------------------------------------------------------------*)
fun toLargeWordX w = 
  Prim.toWord64(Prim.i2l (Prim.b2i (Prim.fromWord8 w)))

(*----------------------------------------------------------------------*)
(* Normalise first; then sign extension just fills in zeros.		*)
(*----------------------------------------------------------------------*)
fun toLargeWord (w : word) = 
  Prim.toWord64(Prim.i2l (normalise w))

fun fromLargeWord w = 
  Prim.toWord8(Prim.i2b (Prim.l2i (Prim.fromWord64 w)))

fun fromInt i = 
  Prim.toWord8(Prim.i2b i)

val fromLargeInt = MLJIntInfUtils.toWord8

val toLargeInt = MLJIntInfUtils.fromWord8

fun toLargeIntX x = 
  MLJIntInfUtils.fromFixedInt(Prim.fromWord64 (toLargeWordX x))

fun toIntX w = Prim.b2i (Prim.fromWord8 w)

fun toInt w = normalise w

fun is_large_shift n= Prim.<>(Prim.shr(Prim.fromWord n,Prim.fromWord 0wx3), 0)
(* is_large_shift n returns true if n>=8 or n<0 (considering it as a signed
   quantity).  This means that shifting a Word8 by n (as a word) will result
   in 0 or (for right sign-extending shifts of a word8 with top bit set) 
   sign extensions (the case n<0 means that as a word, n is very large). *)

fun << (w : word, n) = 
  if is_large_shift n then 0w0
  else Prim.toWord8(Prim.i2b 
    (Prim.shl(Prim.b2i (Prim.fromWord8 w), Prim.fromWord n)))

fun >> (w : word, n) =
  if is_large_shift n then 0w0
  else Prim.toWord8(Prim.i2b (Prim.ushr(normalise w, Prim.fromWord n)))

fun ~>> (w : word, n) =
  Prim.toWord8(Prim.i2b (Prim.shr(toIntX w,
    Prim.fromWord(if is_large_shift n then 0w7 else n))))

(*----------------------------------------------------------------------*)
(* Must normalise before doing comparisons.				*)
(*----------------------------------------------------------------------*)
fun compare (x, y) = 
  let
    val x' = normalise x
    val y' = normalise y
  in
    if Prim.=(x', y') then EQUAL
    else 
    if Prim.lt(x', y')
    then LESS
    else GREATER
  end

fun x <= y = Prim.le (normalise x, normalise y)
fun x >= y = Prim.ge (normalise x, normalise y)
fun x < y  = Prim.lt (normalise x, normalise y)
fun x > y  = Prim.gt (normalise x, normalise y)

(*----------------------------------------------------------------------*)
(* Addition, subtraction and multiplication preserve equivalence...     *)
(*----------------------------------------------------------------------*)
fun (x : word) + (y : word) = 
  Prim.toWord8 (Prim.add(Prim.fromWord8 x, Prim.fromWord8 y))
fun (x : word) - (y : word) = 
  Prim.toWord8 (Prim.sub(Prim.fromWord8 x, Prim.fromWord8 y))
fun (x : word) * (y : word) = 
  Prim.toWord8 (Prim.mul(Prim.fromWord8 x, Prim.fromWord8 y))

(*----------------------------------------------------------------------*)
(* ...but div and mod don't, so normalise first.			*)
(*----------------------------------------------------------------------*)
fun x div y = Prim.toWord8 (Prim.i2b (Prim.div(normalise x, normalise y)))
fun x mod y = Prim.toWord8 (Prim.i2b (Prim.rem(normalise x, normalise y)))

(*----------------------------------------------------------------------*)
(* As comparisons normalise first it might be tidier to return a 	*)
(* normal form but as we don't elide normalisation it's not worth it.   *)
(*----------------------------------------------------------------------*)
fun min (x, y) = if x < y then x else y
fun max (x, y) = if x < y then y else x

(*----------------------------------------------------------------------*)
(* Normalise then use Java's own function on 32-bit ints.           	*)
(*----------------------------------------------------------------------*)
fun fmt radix (w : word) =
  let
  val u = normalise w
  val s = 
    Prim.unsafeValOf (java.lang.Integer.toString(u,
      MLJBaseUtils.radixToBase radix))

  val s = 
      if radix=StringCvt.HEX 
      then Prim.unsafeValOf(s.#toUpperCase ())
      else s
in
  s
end

fun scan getc a b = 
  case Word32.scan getc a b of
    NONE => NONE
  | SOME (w, a) => 
    if Word32.>=(w, 0wx100) then raise Overflow 
    else SOME (Prim.toWord8(Prim.i2b (Prim.fromWord w)), a)

fun fromString s = 
  case Word32.fromString s of
    NONE => NONE
  | SOME w => 
    if Word32.>=(w, 0wx100) then raise Overflow
    else SOME (Prim.toWord8(Prim.i2b (Prim.fromWord w)))

fun toString (w : word) = fmt StringCvt.HEX w

end (* of local open General Bool Option *)
end (* of struct *)
