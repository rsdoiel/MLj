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

(* BytePackML:>BYTEPACKML contains the code for converting a
   Word8Vector into a JavaString.t *)
structure BytePackML:>BYTEPACKML=
struct
   (* This code is inverse to basis/BytePackJava; both the
      same structure BytePack. *)
   fun pack vec=
   let
      (* construct a reader for the vector *)
      type state=int
      fun reader i=
         (SOME(Word8Vector.sub(vec,i),i+1))
         handle Subscript => NONE

      val vec=BytePack.pack {start=0,reader=reader}
      val contents=BytePack.makeList vec
   in
      JavaString.fromUnicode(List.rev contents)
   end
end
