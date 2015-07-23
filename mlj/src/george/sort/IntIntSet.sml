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

(* IntIntSet is an implementation of INTSET for integers *)
structure IntIntSet:>INTSET where type intitem=int =
struct
   structure IntKey=
   struct
      type ord_key=int
      val compare=Int.compare
   end
   structure IntIntersects=Intersects(IntKey)


   type intitem=int
   type intset=int list (* which we will sort *)
   val empty=[]
   val is_empty=List.null

   fun singleton n=[n]
   fun intersects(l1,l2)=IntIntersects.intersects_sorted(l1,l2)

   fun intersects_table{contents,eval,cost}=
   let
      (* Right now we use the naive algorithm for this *)
      val contents_vec=Vector.fromList contents
      val vec=Vector.map eval contents_vec
      
      fun c i=
      let
         val (ci,cset)=Vector.sub(vec,i)
     
         val fiddle=if is_empty(cset) then 0 else ~(cost(ci,ci))
      in
         Vector.foldl
            (fn ((di,dset),sf) =>
               if intersects(cset,dset)
               then
                  sf+cost(ci,di)
               else
                  sf
               )
            fiddle
            vec
      end
   in
      List.tabulate(Vector.length contents_vec,
         fn i => (c i,Vector.sub(contents_vec,i))
         )
   end 
      
      
   fun union(l1,l2)=
   let
      fun inner_union(sofar,l1 as hd1::tl1,l2 as hd2::tl2)=
         (case Int.compare(hd1,hd2) of
            LESS => inner_union(hd1::sofar,tl1,l2)
         |  GREATER => inner_union(hd2::sofar,l1,tl2)
         |  EQUAL => inner_union(hd1::sofar,tl1,tl2)
         )
      |  inner_union(sofar,[],rest)=(sofar,rest)
      |  inner_union(sofar,rest,[])=(sofar,rest)
   in
      List.revAppend(inner_union([],l1,l2))
   end

   type 'a intset'=(int*'a) list
   val empty'=[]
   val is_empty'=List.null

   fun singleton'(i,a)=[(i,a)]
   fun intersects'(l1,l2)=IntIntersects.find_intersects_sorted(l1,l2)
   fun union'(l1,l2,f)=
   let
      fun inner_union(sofar,l1 as (hd1 as (t1,a1))::tl1,l2 as (hd2 as (t2,a2))::tl2)=
         (case Int.compare(t1,t2) of
            LESS => inner_union(hd1::sofar,tl1,l2)
         |  GREATER => inner_union(hd2::sofar,l1,tl2)
         |  EQUAL => inner_union((t1,f(a1,a2))::sofar,tl1,tl2)
         )
      |   inner_union(sofar,[],rest)=(sofar,rest)
      |   inner_union(sofar,rest,[])=(sofar,rest)
   in
      List.revAppend(inner_union([],l1,l2))
   end
end


