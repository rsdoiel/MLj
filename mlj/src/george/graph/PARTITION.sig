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

(* Partition:PARTITION does not use the Graph structure but is in this
   directory because it can be used for many graph algorithms.  It
   is a functor which takes an ORD_MAP indexing things of type elem to
   a structure which manipulates partitions on things of type elem *)
signature PARTITION=
sig
   type elem
   type part (* this corresponds to sets in the partition *)
   type partition (* this corresponds to the partition itself *)
   val new_partition:elem list->partition
      (* create a new partition on the set of elements in the elem list
         where each element is in its own set. *)
   val find_part:partition*elem->part
      (* finds the part containing an element *)
   val union:partition*part*part->partition
      (* returns the partition in which the two parts given (which should
         be parts of this partition) have been replaced by their union
         if they are different; if they are the same nothing is changed. *)
   val union_list:partition*part list->partition
   (* Obvious generalisation of union:

      union_list(partit,p1::p2::rest)=
      let
         val new_partit=union(partit,p1,p2)
      in
         union_list(new_partit,find_part(new_partit,p1)::rest)
      end
      |  union_list(partit,_)=partit
      *)

   val list_part:partition*part->elem list
      (* returns the elements in the part *)
   val list_parts:partition->part list
      (* returns the parts in the partition *)

   structure OrdPart:ORD_KEY
   (* this provides an ordering on parts of the same set *)
   sharing type part=OrdPart.ord_key
end
