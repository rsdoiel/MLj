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

(* LittleEnd:>LITTLEEND packs numbers in little Endian order (because
   Zip files like it that way), unlike Numbers which packs them into
   big endian (because Java files like them that way).  *)
structure LittleEnd:>LITTLEEND=
struct
   fun w2w8 w=Word8.fromLargeWord(Word.toLargeWord w)
   fun W2w8 w=Word8.fromLargeWord(Word32.toLargeWord w)

   fun w2b(w,i)=w2w8(Word.>>(w,0w8*Word.fromInt i))
   fun W2b(w,i)=W2w8(Word32.>>(w,0w8*Word.fromInt i))

   fun toLittle_W2 w=
      Word8Vector.fromList 
        [W2b(w,0),
         W2b(w,1)]
   fun toLittle_W4 w=
      Word8Vector.fromList
         [W2b(w,0),
          W2b(w,1),
          W2b(w,2),
          W2b(w,3)]

   fun fromLittleSlice(vec,i,len)=
   let
      fun w82W w=Word32.fromLargeWord(Word8.toLargeWord w)
      fun do_one(_,toadd,toshift)=Word32.<<(toshift,0w8) +(w82W toadd)
      val len' = len-1
      val final=w82W(Word8Vector.sub(vec,i+len'))
   in
      Word8Vector.foldri
         do_one
         final
         (vec,i,SOME len') 
   end

   fun fromLittle vec=fromLittleSlice(vec,0,Word8Vector.length vec)
end
