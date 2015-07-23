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

(* JavaInt:JAVAINT is the structure for representing Java integers.
   *)

signature JAVAINT=
sig
   eqtype t (* Like int except guaranteed to be 32 bit *)
   exception JavaInt_Overflow (* Raised when integer which is too big
                                 is passed to fromInt *)
   structure pack:PACKABLE (* this is how Java integers are output in
                              4 bytes *)
   sharing type t=pack.t

   val fromInt:int->t
   val toInt:t->int option
   (* SOME i if t can be represented by i, NONE otherwise *)

   structure numops:NUMOPS where type num=t where type shiftnum=t
   (* This allows us to do various operations on JavaInts; see NUMOPS.sig *)
  
   val getint:BinIO.instream->t
   (* getint reads a Java integer stored in Java format *)

   val fromString:IntConvFlags.Base->IntConvFlags.Kind->string->t option
   (* fromString converts the string (of digits) to a t; see IntConv
      for further documentation. *)

   val toString:t->string
   (* toString converts t to a string *)

   val isju2:t->bool
   val isju1:t->bool
   val isji1:t->bool
   val isji2:t->bool
   (* true if t can be represented as 2-byte unsigned/1 byte unsigned/
      1 byte signed/2-byte signed. *)

   (* ju[i](value) represents value, unsigned, i bytes, or raises an
      exception if this isn't possible. *)

   val ju1   :t->Word8.word
   val ju2   :t->Word8Vector.vector
   val ju4   :t->Word8Vector.vector

   val ji1   :t->Word8.word
   val ji2   :t->Word8Vector.vector
   val ji4   :t->Word8Vector.vector

(*
   val -     :t*t->t
   val +     :t*t->t
   (* - and + raise Overflow if the result cannot be represented in a
      JavaInt *)
   val <     :t*t->bool
   (* - and < are used in CompileCode.check_pass . .  *)
*)
   val log2  :t->int option
   (* If there exists int s such that t=2^s, returns s;
      otherwise it returns NONE. *)
end





