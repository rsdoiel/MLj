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

structure PackBig:>PACK_WORD=
struct
(* The core Java API does not seem to provide any easy way of turning
   a word into its constituent bytes, as is demonstrated by the fact
   that DataInputStream does it naively. *)

   open MLJUtils.LargeWord (* get + and * *)
   open Bool

   val bytesPerElem = 8 

   val isBigEndian = true
   val w82W=Word8.toLargeWord
   val W2w8=Word8.fromLargeWord

   fun subVec(vec,i) =
   let
      (* we assume multiplications by powers of 2 will get optimised
         into shifts and additions of 0 to nothing. *)
      val si=Int.*(i,bytesPerElem) (* should be turned into a shift *)
      fun gw j=w82W(Word8Vector.sub(vec,Int.+(si,j)))
   in
      gw 7+0wx100*
     (gw 6+0wx100*
     (gw 5+0wx100*
     (gw 4+0wx100*
     (gw 3+0wx100*
     (gw 2+0wx100*
     (gw 1+0wx100*
     (gw 0 )))))))
   end

   val subVecX=subVec
   (* only different when bytesPerElem doesn't equal the wordsize *)

   fun subArr(arr,i)=
   let
      val si=Int.*(i,bytesPerElem) (* should be turned into a shift *)
      fun gw j=w82W(Word8Array.sub(arr,Int.+(si,j)))
   in
      gw 7+0wx100*
     (gw 6+0wx100*
     (gw 5+0wx100*
     (gw 4+0wx100*
     (gw 3+0wx100*
     (gw 2+0wx100*
     (gw 1+0wx100*
     (gw 0 )))))))
   end

   val subArrX=subArr (* see subVecX *)

   fun update(arr,i,w)=
   let
      val si=Int.*(i,bytesPerElem) (* should be turned into a shift *)
      fun uw(i,x)=Word8Array.update(arr,Int.+(si,i),W2w8 x)
      val _ = uw(7,w)
      val w = LargeWord.>>(w,0w8)
      val _ = uw(6,w)
      val w = LargeWord.>>(w,0w8)
      val _ = uw(5,w)
      val w = LargeWord.>>(w,0w8)
      val _ = uw(4,w)
      val w = LargeWord.>>(w,0w8)
      val _ = uw(3,w)
      val w = LargeWord.>>(w,0w8)
      val _ = uw(2,w)
      val w = LargeWord.>>(w,0w8)
      val _ = uw(1,w)
      val w = LargeWord.>>(w,0w8)
      val _ = uw(0,w)
   in 
      {}
   end
end




