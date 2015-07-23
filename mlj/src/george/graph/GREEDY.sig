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

(* Greedy:GREEDY greedy-colours a graph given a list of its nodes.  Thus,
   if N_i is the i^th node in the list, N_i gets coloured with the least
   positive integer which isn't colouring any node in {N_j | j<i &
   N_j is adjacent to N_i}.  At the moment, no checks are made to verify
   that either (1) the graph has no loops or (2) the graph is symmetric
   (if A is adjacent to B then B is adjacent to A) or (3) the node list
   is a list of the nodes of the graph, without repetitions; the effect of
   breaking any of these rules is undefined.
   *)
signature GREEDY=
sig
   val greedy:(('n,'a) Graph.graph*(Graph.node list))->(Graph.node*int) list
   (* the colouring is returned as a list of pairs (node,its colour) *)
   val mindeg_greedy:(('n,'a) Graph.graph)->(Graph.node*int) list
   (* this colours using the following algorithm: take a copy of the graph.
      Repeatedly remove some node of minimum degree from this graph until
      there is nothing left.  Reverse the order of nodes so obtained, and
      then pass this to greedy. *)
end
