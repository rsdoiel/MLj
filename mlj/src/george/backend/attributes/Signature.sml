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

(* The Signature attribute (at present completely uninterpreted) is
   used in GJ.
   *)
structure Signature:>SIGNATURE=
struct
   type t=JavaString.t

   val name=JavaString.fromString "Signature"

   type hndl=unit->int
   type t2=hndl

   fun pool_pass(A,js)=AllPools.r_Utf8(A,js)

   fun bytecode_pass h=W8.vv1(Numbers.h2(h))

   fun decode_attr((A,is),nbytes)=
   let
      val _ = Assert.assert(nbytes=2,"Bad Signature attribute")
      val index=ReadInts.i2 is
   in
      ReadPool.get_utf8(A,index)
   end
end
