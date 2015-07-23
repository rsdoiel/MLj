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

structure JavaFloat:>JAVAFLOAT=
struct
   (* Packing reals is timeconsuming and may be done lots of times
      as it is used for comparisons in the hash table and
      elsewhere.  So we cache the packed
      real and make sure we only compute it once.  (Arguably
      we shouldn't keep the packed real around; it is only
      needed in toInt. )
      *)

   type t=real* Word8Vector.vector option ref

   fun packFloat x=PackFloat.pack {exponent_size=8,mantissa_size=23,value=x}
   
   fun getPacked((x,r):t)=
      (case !r of
         NONE =>
            let 
               val packing=packFloat x
               val _ = r:=SOME packing
            in
               packing
            end
      |  SOME packing => packing
      )

   structure pack:>PACKABLE where type t=t =
   struct
      type t=t
      fun pack value=getPacked value
      fun equal(x,y)=(pack x = pack y)
   end

   fun order(x,y)=W8Order.compare(getPacked x,getPacked y)

   fun fromReal x=(x,ref NONE):t
   fun toInt(value as (x,_))=
   let
      val i=Real.round x
      val approx=Real.fromInt(i)
   in if (getPacked value=packFloat approx) then SOME i else NONE
   end handle Overflow=>NONE | Domain=>NONE

   fun getfloat is=
   let
      val packed=ReadInts.inputN(is,4)
   in
      (UnPackFloat.unpack{data=packed,exponent_size=8,
         mantissa_size=23},ref(SOME packed)):t
   end

   fun toString(x,_)=Real.toString x
end




