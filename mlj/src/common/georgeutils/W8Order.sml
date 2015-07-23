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

(* W8Order:ORD_KEY orders Word8Vectors. *)
structure W8Order:>ORD_KEY where type ord_key=Word8Vector.vector =
struct
   type ord_key=Word8Vector.vector
   fun compare(k,l)=
   let
      fun ci i=
      let
         val ki=Word8Vector.sub(k,i)
         val li=Word8Vector.sub(l,i)
      in
         (case Word8.compare(ki,li) of
            LESS => LESS
         |  GREATER => GREATER
         |  EQUAL => ci(i+1)
         )
      end 
         handle Subscript =>
            Int.compare(Word8Vector.length k,Word8Vector.length l)
   in
      ci 0
   end
end