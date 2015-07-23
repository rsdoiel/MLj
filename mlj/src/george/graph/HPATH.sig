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

(* HPath:HPATH finds an approximate solution to the following NP-complete problem:
   given a complete digraph where the arcs are weighted by non-negative reals, find a sequence of
   vertices beginning with some nominated vertex which has minimum total weight. *)
signature HPATH=
sig
   val prefind:('a,real) Graph.preGraph*Graph.node -> Graph.node list
   (* the node supplied is the nominated one.  Like OrthogonalPartition, it is actually more convenient
      to work with the preGraph than the graph.

      If multiple arcs are supplied with the same beginning and end, they are replaced by a single
      arc with weight their total weight.
      *)
end
