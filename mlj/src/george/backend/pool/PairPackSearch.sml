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

functor PairPackSearch(structure LeftP:PACKABLE and LeftS:HASH_KEY
                       sharing type LeftP.t=LeftS.hash_key
                       structure RightP:PACKABLE and RightS:HASH_KEY
                       sharing type RightP.t=RightS.hash_key
                       ):>PAIRPACKSEARCH
                           where type left=LeftP.t
                           where type right=RightP.t =
struct
   type left=LeftP.t
   type right=RightP.t
   datatype pair=make_pair of left*right

   structure P:>PACKABLE where type t=pair =
   struct
      type t=pair
      fun pack(make_pair(l,r))=
         Word8Vector.concat [LeftP.pack(l),RightP.pack(r)]
      fun equal(make_pair(l1,r1),make_pair(l2,r2))=
         LeftP.equal(l1,l2) andalso RightP.equal(r1,r2)
   end

   structure S:>HASH_KEY where type hash_key=pair =
   struct
      type hash_key=pair
      fun sameKey(make_pair(l1,r1),make_pair(l2,r2))=
         LeftS.sameKey(l1,l2) andalso RightS.sameKey(r1,r2)
      fun hashVal(make_pair(l,r))=
         LeftS.hashVal l + 0w201 * RightS.hashVal r
   end
end
