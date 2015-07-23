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

structure PackRealBig:PACK_REAL=
struct
   type real=Real.real
   val bytesPerElem = 8
   val isBigEndian = Bool.true

   fun real2word(x:real):LargeWord.word= 
     _pure(Prim.toWord64(java.lang.Double.doubleToLongBits (x)))
   fun word2real(x:LargeWord.word):real= 
      _pure(java.lang.Double.longBitsToDouble(Prim.fromWord64 x))

   fun toBytes r=
   let
      val arr=Word8Array.array(bytesPerElem,0w0)
      val contents=real2word r
      val _ = PackBig.update(arr,0,contents)
   in
      Word8Array.extract(arr,0,Option.NONE)
   end


   fun subVec(vec,i)=
      word2real(PackBig.subVec(vec,i))

   fun subArr(arr,i)=
      word2real(PackBig.subArr(arr,i))
  
   fun fromBytes vec=subVec(vec,0)
   (* raises Subscript if vec isn't big enough, which seems about the
      best we can do (Otherwise not specified) *)

   fun update(arr,i,r)=PackBig.update(arr,i,real2word r)
end
