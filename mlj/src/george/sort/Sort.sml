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

(* Sort is a functor which takes ORD_KEY structures into sorting functions on
   those keys.  ORD_KEY must have the following two members:

   type ord_key
   val compare : (ord_key * ord_key) -> order

   Thus this corresponds to the current definition of ORD_KEY in the
   SML/NJ extensions to the standard basis.  The implementation of the
   functor also uses these extensions, but the purpose of writing these
   functions is that this can be changed later.

   The result of the functor is :SORT rather than :>SORT, but I shall
   change this when SML/NJ implements "where type" correctly.
    *)
functor Sort (K:ORD_KEY) :> SORT where type key=K.ord_key =
struct
   type key=K.ord_key
   val sort=ListMergeSort.sort (fn (x,y)=>(K.compare(x,y)=GREATER))
   fun sort_pairs list=ListMergeSort.sort (fn ((x,_),(y,_))=>(K.compare(x,y)=GREATER)) list
   (* We need to eta expand to avoid a value restriction problem. *)

   val reverse_sort=ListMergeSort.sort (fn (x,y)=>(K.compare(x,y)=LESS))
   fun reverse_sort_pairs list=ListMergeSort.sort (fn ((x,_),(y,_))=>
      (K.compare(x,y)=LESS)) list

end
