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

(* The pool is implemented via a Dictionary, keyed by I
   (which contains the packing function) and S (which contains the
   hashing function). *)

functor Pool(I:POOL_ITEM):>POOL where type t=I.t =
struct
   type t=I.t

   structure Hashing=I.Hashing

   structure PoolDict=HashTableFn(I.Hashing)

   datatype pool=P of bool ref (* true if the pool has been resolved *)
                    * Handles.pool_handle PoolDict.hash_table

   exception NotFound 
   (* raised by PoolDict.lookup when it can't find an item.  Actually this should never
      be raised, since we don't use lookup. *)

   fun create(n)=P(ref false,PoolDict.mkTable(n,NotFound))

   exception already_resolved

   fun add(P(b,dict),value)=
      if !b then raise already_resolved
      else case PoolDict.find dict value of
         SOME pv=>pv
      |  NONE   =>let

            val hand=Handles.new_handle()
            val _=PoolDict.insert dict (value,hand)
         in
            hand
         end

   exception twice_resolved

   val tag_vector=Word8Vector.fromList[I.tag]

   structure Word8VectorLex= (* Lexical key on Word8Vectors *)
   struct
      type ord_key=Word8Vector.vector
      fun compare(v1,v2)=
      let
         fun sd i=
         (case Word8.compare(Word8Vector.sub(v1,i),Word8Vector.sub(v2,i)) of
             EQUAL => sd (i+1)
         |   x => x
         )
      in
         (sd 0) handle Subscript => 
            Int.compare(Word8Vector.length v1,Word8Vector.length v2)
      end
   end

   structure SortWord8VectorLex=Sort(Word8VectorLex)
    
   fun resolve(P(b,dict),iref)=
      if !b then raise twice_resolved
      else 
      let
         val _= (b:=true)
         (* transform list_pool into a list of byte vectors, and resolve
            all the references *)
         val kh_list= 
            List.map
               (fn (key,hand) => (I.Packing.pack key,hand))
               (PoolDict.listItemsi dict)

         val kh_list_sorted=SortWord8VectorLex.sort_pairs kh_list

         val list_list_vectors=List.map
            (* We rely on map working from left to right *)
            (fn (bytes,hand)=>let
                val _= Handles.set_handle(hand,!iref)
                val _= (iref:= !iref+I.skip)
             in
                [tag_vector,bytes]
                (* this is the syntax of a single entry in the constant pool
                   *)
             end)
             kh_list_sorted

         val list_vectors=List.concat list_list_vectors;
         val vector=W8.fromvList list_vectors
      in vector
      end

   fun pool_size(P(_,dict))=I.skip*PoolDict.numItems(dict)
end




