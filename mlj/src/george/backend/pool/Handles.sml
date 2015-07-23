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

(* Handles:>HANDLES implements handles to items in the constant pool.  They can be
      hashed at any time using the Handle and TwoHandle structures.
      Once the position of the item has been set, they can be used
      in the HandlePack and TwoHandlePack structures, which
      pack a handle or a pair of handles into a Word8Vector as they are
      stored in the constant pool, or via lookup.
      *)
structure Handles:>HANDLES=
struct
   (* We use HashableRefs *)
   structure HR=HashableRefs(type t=int) 
   (* Negative numbers are used for unset handles.
      We don't check this kind of thing because the backend has been working for several
      months now and it seems unlikely there that this structure will get used in
      incorrect ways.  Also judging by the first profiler results it is actually one
      of the more expensive parts of the program. *)
   type pool_handle=HR.href
   fun new_handle ()= HR.new(~1)
   fun set_handle(h,i)=(HR.access h):=i
   fun lookup(h)= !(HR.access h)

   structure HandleKey=HR.S
   structure HandlePack=
   struct 
      type t=pool_handle
      fun pack h=Numbers.u2(lookup h)
      val equal=HR.S.sameKey
   end

   structure TwoHandlePS=PairPackSearch(
      structure LeftS=HandleKey and LeftP=HandlePack and
                RightS=HandleKey and RightP=HandlePack)

   type pair=TwoHandlePS.pair
   val make_pair=TwoHandlePS.make_pair
   structure TwoHandleKey=TwoHandlePS.S
   structure TwoHandlePack=TwoHandlePS.P
end



