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

(* JavaLong:JAVALONG is the structure for storing Java longs.  Currently
   these are stored as a tuple of two 32 bit words; however the only
   constructor takes an ML int so it isn't possible to set any but the
   bottom word. *)

signature JAVALONG=
sig
   eqtype t
   structure pack:PACKABLE
   sharing type t=pack.t
   val fromInt:int->t
   val fromString:IntConvFlags.Base->IntConvFlags.Kind->string->t option
   (* fromString converts the string (of digits) to a t; see IntConv
      for further documentation. *)
   val toInt:t->int option
   (* SOME i if t can be represented by i, NONE otherwise *)

   structure numops:NUMOPS where type num=t where type shiftnum=JavaInt.t
   (* This allows us to do various operations on JavaInts; see NUMOPS.sig *)
  
   val getlong:BinIO.instream->t
   (* getlong reads a Java long stored in Java format *)

   val log2  :t->int option
   (* If there exists int s such that t=2^s, returns s;
      otherwise it returns NONE. *)


   val toJavaInt:t->JavaInt.t
   (* Returns the int obtained from the bottom 32 bits of the argument.
      Thus this function never fails; it is designed to be equivalent
      to the Java VM's l2i instruction *)

   val fromJavaInt:JavaInt.t->t
   (* Returns the JavaLong derived from the argument by sign-extending
      it to 64 bits.  This function never fails; it is designed to
      be equivalent to the Java VM's i2l instruction *)
end

