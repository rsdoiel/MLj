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
signature SORT=sig
   type key
   val sort:key list->key list
   (* The list is sorted into increasing order.  The sort is not
      guaranteed to be stable. *)
   val sort_pairs:(key*'a) list->(key*'a) list
   (* Like sort, but extra data is carried along as well *)

   val reverse_sort:key list->key list
   val reverse_sort_pairs:(key*'a) list->(key*'a) list
   (* reverse_sort and reverse_sort_pairs are like sort and sort_pairs
      except that they sort in reverse order *)
end
