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

structure Word8Key:>HASH_KEY where type hash_key=Word8Vector.vector=
struct
(* We define hashing for Word8Vector's; IE byte streams. *)
   type hash_key=Word8Vector.vector
   fun sameKey(x,y)=(x=y)
   fun hashVal(x:hash_key)=
   let
      (* hashpjw from section 7.6 of the Dragon Book.  Only the
         bottom 28 bits are valid. *)
      fun hashword8(w,w8)=
      let
         val next=Word32.fromLargeWord(Word8.toLargeWord w8)
         val w=Word32.+(Word32.<<(w,0w4),next)
         val g=Word32.andb(w,0wxf0000000)
      in 
         Word32.xorb(w,Word32.>>(g,0w24))
      end

      fun tohash w=
      let
         val w=Word32.andb(w,0wxfffffff)
      in
         Word.fromLargeWord(Word32.toLargeWord w)
      end

      fun seqhash(n,hsofar)=
      if n=0 then 
         tohash hsofar
      else 
         seqhash(n-1,hashword8(hsofar,Word8Vector.sub(x,n-1)))
   in seqhash(Word8Vector.length(x),0w0)
   end
end (* struct *)
