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

(* Intersects is a functor which takes an ORD_KEY to
   INTERSECTS, a structure containing the function intersects which tests if
   two lists contain a common element. *)
functor Intersects(A:ORD_KEY):>INTERSECTS where type t=A.ord_key =
struct
   type t=A.ord_key
   structure ASort=Sort(A)

   fun intersects_sorted(l1 as hd1::tl1,l2 as hd2::tl2)=
   (* finds if two sorted lists intersect *)
      (case A.compare(hd1,hd2) of
          LESS => intersects_sorted(tl1,l2)
      |GREATER => intersects_sorted(l1,tl2)
      |EQUAL   => true
      )
   |  intersects_sorted(_,_)=false (* one of the lists is empty *)

   fun intersects(l1,l2)=intersects_sorted(ASort.sort l1,ASort.sort l2)

   fun find_intersects_sorted(l1,l2)=
   let
      fun inner_find_intersects_sorted(sofar,l1 as (t1,a)::tl1,l2 as (t2,b)::tl2)=
         (case A.compare(t1,t2) of
            LESS => inner_find_intersects_sorted(sofar,tl1,l2)
         |  GREATER => inner_find_intersects_sorted(sofar,l1,tl2)
         |  EQUAL => inner_find_intersects_sorted((t1,a,b)::sofar,tl1,tl2)
         )
      |  inner_find_intersects_sorted(sofar,_,_)=sofar (* one of the lists is empty *)
   in
      inner_find_intersects_sorted([],l1,l2)
   end

   fun find_intersects(l1,l2)=find_intersects_sorted(ASort.sort_pairs l1,ASort.sort_pairs l2)

end
