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

(* Priority:PRIORITY is a structure that implements priority queues for
   pairs int*int.  In a pair (k,n), k is used to key the priority queue.
   n is an identification number.  Identification numbers are allocated from
   0 upwards as items as added in the priority queue.  The queue itself is
   updated by side-effects. *)
signature PRIORITY=
sig
   type queue
   val empty:unit->queue (* allocate a new empty queue *)
   val insert:queue*{key:int}->{id:int}
   (* insert(q,key=k) adds (k,n) to the queue and returns n, where n is
      the number of times insert has previously been called on this queue. *)
   val bump:queue*{id:int,newkey:int}->unit
   (* bump changes the key attached to id to newkey. *)
   exception NotFound
   val lookup:queue*{id:int}->{key:int}
   (* lookup finds the key currently attached to the id, raising
      NotFound if it's not there. *)
   val lowest:queue*int->{key:int,id:int} list
   (* Let the number of items in the queue be N.
      lowest(q,n) returns [{key=k1,id=i1},...,{key=km,id=im}] where
      m=min(n,N); the (ki,mi) are distinct elements in the queue; and
      there is no (k,i) not in the queue such that k is not >= some ki. *)
   val remove:queue*{id:int}->unit
   (* remove removes the item with given id from the queue, or raises NotFound if it isn't there.*)
end
