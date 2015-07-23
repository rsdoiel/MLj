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

(* WSArcs:WSARCS takes a list of weighted arcs, removes all arcs with the same beginning and end and
   replaces them by an arc with weight their total weight, and sorts the arcs in descending order. *)
structure WSArcs:>WSARCS=
struct
   local
      open Graph
      structure OrdArcNode:>ORD_KEY 
         where type ord_key=({from:node,to:node}*real)=
      struct (* this is used for sorting the arcs.  Fortunately our
                arcs are monomorphic *)
         type ord_key=({from:node,to:node}*real)
         fun compare((_,r1),(_,r2))=Real.compare(r1,r2)
      end
      structure OrdArcSort=Sort(OrdArcNode)

      structure NodePairKey:ORD_KEY=
      struct (* this is used for removing duplicate arcs. *)
         type ord_key={from:node,to:node}
         fun compare({from=f1,to=t1},{from=f2,to=t2})=
            (case OrdNode.compare(f1,f2) of
                LESS=>LESS
            |   GREATER=>GREATER
            |   EQUAL=>OrdNode.compare(t1,t2))
      end
      structure NodePairMap=SplayMapFn(NodePairKey)
   in
      fun wsarcs arc_list=
      let
         val arc_map= (* construct a map from node pairs to reals *)
            List.foldl
            (fn ((nodepair,weight),map_so_far)=>
                (case NodePairMap.find(map_so_far,nodepair) of
                   SOME oldweight=>
                      NodePairMap.insert(map_so_far,nodepair,
                         oldweight+weight)
                |  NONE=>
                      NodePairMap.insert(map_so_far,nodepair,weight)
                ))
            NodePairMap.empty
            arc_list

         val arcs2=NodePairMap.listItemsi arc_map
      in
         (* Sort the arcs in descending order of weight. *)
         List.rev(OrdArcSort.sort arcs2)
      end
   end
end
