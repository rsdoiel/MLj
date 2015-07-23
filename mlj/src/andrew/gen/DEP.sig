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

(*======================================================================*)
(* Dependency analysis							*)
(*======================================================================*)
signature DEP =
sig
 
(*----------------------------------------------------------------------*)
(* Both functions in this module take two arguments: graph and eq.	*)
(*									*)
(* If 'b is the type of node identifiers and 'a is the type of node	*)
(* data (which also includes enough information to identify the node),	*)
(* then:								*)
(*									*)
(*   graph : ('a * 'b list) list					*)
(*     lists the data for each node paired with its neighbours		*)
(*   eq : ('a * 'b) -> bool						*)
(*     returns true if the data 'a comes from the node identified by 'b *)
(*									*)
(* Typically a graph in this form is easy to generated (e.g. for 	*)
(* letrec and datatype dependency analysis).				*)
(*----------------------------------------------------------------------*)

(*----------------------------------------------------------------------*)
(* Topological sorting for dependency analysis				*)
(*									*)
(* The result is a pair of list of nodes (ns1, ns2)			*)
(* where ns1 are the acyclic nodes listed in order of dependence,	*)
(* and ns2 are nodes which occur in cycles.				*)
(*----------------------------------------------------------------------*)
val cyclesort : ('a * 'a list) list * ('a*'a -> bool) -> 'a list * 'a list

(*----------------------------------------------------------------------*)
(* Strongly-connected components wrapper.				*)
(* George's interface is rather unwieldy; here we provide a less	*)
(* general interface to it.						*)
(*									*)
(* Given (graph, eq) as described above,                                *)
(*   scc (graph, eq) : 'a Component list	                        *)
(* is a list of strongly-connected components, ordered such that the    *)
(* root comes last. Each component is either a non-recursive singleton  *)
(* (NonRec x) or a recursive non-empty list (Rec xs).                   *)
(*----------------------------------------------------------------------*)
datatype 'a Component =
  NonRec of 'a
| Rec of 'a NList.list

val scc : ('a * 'b list) list * ('a*'b -> bool) -> 'a Component list

end