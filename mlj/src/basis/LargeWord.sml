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

structure LargeWord : WORD =
struct

local 
  open General Bool Option 
  val op= = Prim.=
in

type word = Prim.word64

val wordSize = 64

fun toLargeWordX w = w
fun toLargeWord w = w
fun fromLargeWord w = w

fun fromInt n = Prim.toWord64(Prim.i2l n)

val fromLargeInt = MLJIntInfUtils.toLargeWord

fun toIntX w = Prim.l2i (Prim.fromWord64 w)

fun toLargeIntX x = 
  MLJIntInfUtils.fromFixedInt(Prim.fromWord64 x)

fun toLargeInt w = 
  MLJIntInfUtils.fromLargeWord w

fun toInt w =
  if Prim.<>(Prim.And(Prim.fromWord64 w, 
    Prim.fromWord64 (0wxffffffff80000000)), Prim.fromWord64 0w0)
  then raise Overflow
  else Prim.l2i (Prim.fromWord64 w)

fun << (i, n) = 
  if Prim.<>(Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffc0), 0)
  then 0w0
  else Prim.toWord64(Prim.shl(Prim.fromWord64 i, Prim.fromWord n))

fun >> (i, n) = 
  if Prim.<>(Prim.And (Prim.fromWord n, Prim.fromWord 0wxffffffc0), Prim.fromWord 0w0)
  then 0w0
  else Prim.toWord64(Prim.ushr(Prim.fromWord64 i, Prim.fromWord n))

fun ~>> (i, n) = 
  Prim.toWord64(Prim.shr(Prim.fromWord64 i, 
  Prim.fromWord(
    if Prim.<>(Prim.And(Prim.fromWord n, Prim.fromWord 0wxffffffc0), Prim.fromWord 0w0)
    then 0wx3f else n)))

fun isPos x = Prim.ge(Prim.fromWord64 x, Prim.fromWord64 0w0)
fun compare (x, y) = 
  if x=y then EQUAL
  else 
    if isPos x
    then
      if isPos y
      then 
        if Prim.lt (Prim.fromWord64 x, Prim.fromWord64 y) 
        then LESS 
        else GREATER
      else LESS
    else 
      if isPos y
      then GREATER
      else 
        if Prim.lt (Prim.fromWord64 x, Prim.fromWord64 y)
        then LESS
        else GREATER
    
fun x <= y = 
  case compare(x,y) of
    GREATER => false
  | _ => true
  
fun x >= y = 
  case compare(x,y) of
    LESS => false
  | _ => true
  
fun x < y = 
  case compare(x,y) of
    LESS => true
  | _ => false
  
fun x > y = 
  case compare(x,y) of
    GREATER => true
  | _ => false

open MLJUtils.LargeWord

fun a mod b =
  if Prim.lt(Prim.fromWord64 b, Prim.fromWord64 0w0)
  then
    if Prim.lt(Prim.fromWord64 a, Prim.fromWord64 0w0) 
    andalso Prim.le(Prim.fromWord64 b, Prim.fromWord64 a)
    then
      a-b
    else
      a
  else
    let
      val k = Prim.rem(Prim.ushr(Prim.fromWord64 a, 1), Prim.fromWord64 b)
      val r = Prim.add(Prim.shl(k, 1), 
        Prim.And(Prim.fromWord64 a, Prim.fromWord64 0w1))
   in
      if Prim.ge(r,Prim.fromWord64 b) orelse Prim.lt(r, Prim.fromWord64 0w0)
      then
         Prim.toWord64 r - b
      else
         Prim.toWord64 r
   end

fun a div b =
  if Prim.lt(Prim.fromWord64 b, Prim.fromWord64 0w0)
  then
    if Prim.lt(Prim.fromWord64 a, Prim.fromWord64 0w0) 
    andalso Prim.le(Prim.fromWord64 b, Prim.fromWord64 a)
    then
      0w1
    else
      0w0
  else
    if Prim.ge(Prim.fromWord64 a, Prim.fromWord64 0w0) 
    (* this is a common enough case to be worth optimising *)
    then Prim.toWord64 (Prim.div(Prim.fromWord64 a, Prim.fromWord64 b))
    else
    let
       val aa = Prim.ushr(Prim.fromWord64 a, 1)
       val g = Prim.div(aa, Prim.fromWord64 b)
       val guess = Prim.toWord64(Prim.shl(g, 1))
       val r = a-guess*b
    in
      if Prim.ge(Prim.fromWord64 r, Prim.fromWord64 b) 
      then guess+0w1 else guess
    end

fun min (x, y) = if x < y then x else y
fun max (x, y) = if x < y then y else x

fun fmt radix i = raise General.NotImplemented "Word64.fmt"

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
