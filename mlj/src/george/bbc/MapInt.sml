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

(* MapInt is a functor taking an ORD_KEY K to a MAPINT structure which finds the intersections of lists.
   (the implementation should get changed when we can use the New Jersey map intersection functions). *)
functor MapInt(structure K:ORD_KEY):>MAPINT where type key=K.ord_key =
struct
   type key=K.ord_key
   structure SortKey=Sort(K)

   fun multi_intersect(kll,eqfun)=
   let
      (* sort all the lists *)
      val kll_sorted=List.map (SortKey.sort) kll
      (* We find the intersection of 2 lists first and then proceed. *)
      fun intersect2(sofar,l1,l2)=
      (case (l1,l2) of
         (hd1::tl1,hd2::tl2)=>
             (case K.compare(hd1,hd2) of
                EQUAL => if eqfun(hd1,hd2)
                         then intersect2(hd1::sofar,tl1,tl2)
                         else intersect2(sofar,tl1,tl2)
             |  GREATER => intersect2(sofar,l1,tl2)
             |  LESS => intersect2(sofar,tl1,l2)
             )
      | (_,_) => List.rev sofar (* one list is empty *)
      )
      fun mint(l1::l2::rest)=mint(intersect2([],l1,l2)::rest)
      |   mint([l1])=l1
      |   mint []=[]
      fun difference2(sofar,l1,l2)=
      (* l1 and l2 should be sorted and l2 should contain the elements of l1.  This returns
         the union, in unspecified order, of l2\l1 @ sofar *)
      (case (l1,l2) of
         (hd1::tl1,hd2::tl2)=>
            (case K.compare(hd1,hd2) of
               EQUAL => difference2(sofar,tl1,tl2)
            |  GREATER => difference2(hd2::sofar,l1,tl2)
            )
      |  ([],_)=> List.revAppend(l2,sofar)
      )
      val intersection=mint(kll_sorted)
      val remains=
         List.map
            (fn l => difference2([],intersection,l))
            kll_sorted
   in
      (intersection,remains)
   end
end
