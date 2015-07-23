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

(* GeneralGraph:GENERALGRAPH
   is intended to provide a general interface for graph
   operations.  Nodes and arcs are labelled.  A graph has a non-negative
   integer associated with it, which we call its Arity.
   Each node has a number of sets of arcs attached to it; these arcs
   are indexed by numbers 0,..,Arity-1.  Each arc points to a node and
   also contains an arc label.  Each set may
   contain at most one arc per node, so multiple arcs are not allowed,
   though loops are.  It is not possible to find out what arcs point to
   a node without checking all the arcs of all the nodes; therefore to
   implement directed graphs efficiently it might be advisable to use
   a graph of arity 2, with set 0 containing the arcs of the directed
   graph, and set 1 reversed arcs; then the arcs going from and to the
   node can be found. *)
signature GENERALGRAPH=
sig
   (* Convention: 'a is the type of arc-labels; 'n is the type of
      node-labels.  *)

   (* Graphs are immutable, though the compiler may have difficulty
      spotting this as they must perforce use refs. *)

   type ('n,'a) graph
   type ('n,'a) node (* nodes are actually pointed graphs, so encode not
                        just the node but the graph they belong to. *)

   val create: {
      arity:int,
      nodes:'n list
      } -> ('n,'a) graph * ('n,'a) node list
   (* create creates a graph with no arcs and returns a list of its nodes. *)

   exception ArcAlready

   val add_arc: {
      from:('n,'a) node,
      to:('n,'a) node,
      arc_data:'a,
      index:int
      } -> ('n,'a) graph
   (* add_arc adds an arc to "to in the "index'th set for "from, returning
      the resulting graph.  If there is already an arc for that node in
      the graph, nothing is done, and ArcAlready is raised.
      "from and "to should belong to the same
      graph, or Fail is raised with an appropriate argument.
      If index is out of range, subscript is raised. *)

   val mapNode: ('n1->'n2)->(('n1,'a) graph)->(('n2,'a) graph)
   (* mapNode transforms the graph, changing all the node labels *)

   val neighbours: ('n,'a) node * int-> (('n,'a) node * 'a) list
   (* neighbours returns a list of the i'th neighbours of the given node,
      with the arc-labels.  The order of the list is unspecified, where
      i is the integer argument.  If i is out of range, subscript is raised.
      *)
   val label: ('n,'a) node->'n
