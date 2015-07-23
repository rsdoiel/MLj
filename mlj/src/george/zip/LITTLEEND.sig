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

(* LittleEnd:>LITTLEEND packs and unpacks numbers in little Endian order (because
   Zip files like it that way), unlike Numbers which packs them into
   big endian (because Java files like them that way).  *)
signature LITTLEEND=
sig
   val toLittle_W2:Word32.word->Word8Vector.vector
   val toLittle_W4:Word32.word->Word8Vector.vector
   val fromLittle:Word8Vector.vector->Word32.word
   (* the number of bytes is taken to be the length of the vector *)
   val fromLittleSlice:Word8Vector.vector*int*int->Word32.word
   (* fromLittleSlice(vec,i,len) returns the word encoded in a littleendian
      way in the len bytes starting at index i.  len is assumed>=1. *)
end
