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

(* BytePackJava:>BYTEPACKJAVA contains the Java-specific code for
   unpacking strings into bytes.
   *)
structure BytePackJava:>BYTEPACKJAVA=
struct
   (* This code is inverse to common/values/BytePackML; both the
      same structure BytePack. *)

   open General
   open Option
   open List

   fun unpack s=
   let
      (* Construct a reader *)
      type state=int
      val start=0
      fun reader i=
         (SOME(Word.fromInt(Char.ord(String.sub(s,i))),Int.+(i,1)))
            handle Subscript => NONE

      val vec=BytePack.unpack{start=start,reader=reader}
      (* Time is not critical, simplicity is.  So we unroll this into
         a list and then lay it out.  The list is in reverse order. 
         All the allocations could be avoided by using a dynamic
         array instead. *)
      val contents=BytePack.makeList vec
      val length=List.length contents
      val arr : Prim.byte array = Prim.newarray length
      fun unwrap(l as h::t,i)=
      let
         val i=Int.-(i,1)
         val ()= Prim.arraystore(arr,i, Prim.i2b (Word8.toInt h))
      in
         unwrap(t,i)
      end
      |  unwrap([],_) = ()
      val ()=unwrap(contents,length)
   in
      arr
   end
end
