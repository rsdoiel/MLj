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

(* JavaIntInf:JAVAINTINF is the structure for representing Java big integers.
   *)
signature JAVAINTINF=
sig
   type t

   exception JavaIntInf_UnImp 
   (* This is raised for the logical functions in NumOps to indicate
      that they haven't been implemented yet. *)

  
   datatype packing=
      INT of JavaInt.t
   |  LONG of JavaLong.t
   |  BYTES of Word8Vector.vector
 
   val pack:t->packing
   (* This is used for packing JavaIntInfs so that they can be put into
      the classfile.  The BYTES format is used if the IntInf is not in
      the range [-2^63,2^63-1], and is in the same format as is used for
      the java.math.BigInteger constructor from a bytes array.
      This is the 2s complement representation, truncated to sufficiently
      many bytes to include the sign bit, with the MS byte first.
      (So the highest bit of byte 0 is the sign bit).
      *)

   structure numops:NUMOPS where type num=t where type shiftnum=JavaInt.t
   (* This allows us to do various operations on JavaInts; see NUMOPS.sig *)

   val fromString:IntConvFlags.Base->IntConvFlags.Kind->string->t option
   (* fromString converts the string (of digits) to a t; see IntConv
      for further documentation. *)

   val toString:t->string
   (* toString converts t to a string *)
end





