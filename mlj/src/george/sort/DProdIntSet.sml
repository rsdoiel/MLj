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

(* DProdIntSet is a functor taking an INTSET and a PARTIALINTSET structure to a PARTIALINTSET
   structure (there is additional code in this file for taking an  INTSET*INTSET to INTSET but
   it is commented out). *)
functor DProdIntSet(structure A:INTSET and B:PARTIALINTSET):>
  PARTIALINTSET where type intitem=A.intitem*B.intitem
=
struct
   type intitem=A.intitem*B.intitem
   type intset=B.intset A.intset'
   (* invariant - all the B.intsets should be nonempty. *)

   val empty=A.empty'
   val is_empty=A.is_empty'

   fun singleton(a,b)=A.singleton'(a,B.singleton(b))
   fun intersects(aset1,aset2)=
   let
      val aalist=A.intersects'(aset1,aset2)
   in
      List.exists
         (fn (_,bset1,bset2)=>B.intersects(bset1,bset2))
         aalist
   end

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
      
   fun union(aset1,aset2)=
      A.union'(aset1,aset2,fn (bset1,bset2)=>B.union(bset1,bset2))

(* remaining code only necessary and possible if we want INTSET and B is an INTSET *)
(*
   type 'a intset'='a B.intset' A.intset'
   fun singleton'((a,b),lab)=A.singleton'(a,B.singleton'(b,lab))
   fun intersects'(aset1,aset2)=
   let
      val aalist=A.intersects'(aset1,aset2)
   in
      List.concat
         (List.map
            (fn (a,bset1,bset2)=>
               List.map
                  (fn (b,lab1,lab2)=>((a,b),lab1,lab2))
                  (B.intersects'(bset1,bset2))
               )
            aalist
            )
   end

   fun union'(aset1,aset2,f)=
      A.union'(aset1,aset2,fn(bset1,bset2)=>B.union'(bset1,bset2,f))
*)
end
