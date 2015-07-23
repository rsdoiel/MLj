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

(* MulCarry:MULCARRY implements multiplication by 10 and by 16 of Word32.words
   interpreted as an unsigned quantity with carry.
   *)
structure MulCarry:>MULCARRY=
struct
   (* In each case, the carry is given first *)

   fun mul10 (w:Word32.word)=
   let
      val mul=Word32.*(w,0w10)
      (* mul is the remainder.  We need to compute the carry.  We know
         that (for real integers) mul=10*w-2^32*carry.  Hence
         *)
      val carry=Word32.mod(Word32.<<(
         Word32.+(Word32.mod(mul,0w11),Word32.mod(w,0w11)),
         0w3)
         ,0w11)
      (* (3 remainders & an extra addition seem excessive but I can't be
         bothered to think up a better way right now) *)
   in
      (carry,mul)
   end

   fun mul16 w=(Word32.>>(w,0w28),Word32.<<(w,0w4))
end
