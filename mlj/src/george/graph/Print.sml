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

(* Print:>PRINT contains functions for prettyprinting an (int*'a) graph.
   Parse.parseD_string
   o PRINT.printD_string is the identity on (int,unit) graphs where
   the ints are distinct and consecutive from 1. Parse.parseU_string o
   Print.printU_string is the identity on (int,unit) symmetric graphs
   where the ints are distinct and consecutive from 1.  printU_string
   omits all arcs which go from a vertex label x to a vertex label y with
   x>y, thus saving space for symmetric graphs. *)
structure Print:>PRINT=
struct
   local
      open Graph
   in
      val i2s=Int.toString
      (* general_print_string generalises printD_string and printU_string;
         it prints out those arcs for which test_fun {from=int,to=int}
         returns true. *)
      fun general_print_string test_fun (G:(int,'a) Graph.graph)=let
         val node_list=nodeList(G)
         fun n2i n=valOf(nodeLabel(G,n))
         val n2s=i2s o n2i
      in
         String.concat(List.concat(
            [i2s(length(node_list))]::
            List.map
              (fn node=>"\n"::n2s node::
                  List.concat(
                     List.map
                       (fn nodeto=>let
                           val nfrom=n2i(node)
                           val nto=n2i(nodeto)
                        in
                           if test_fun {from=nfrom,to=nto}
                           then
                              ["-",i2s nto]
                           else
                              []
                        end)
                       (valOf(neighbours(G,node)))
                     ))
              node_list
              )
            @
            ["."])
      end

      fun printD_string g=general_print_string (fn _ => true) g
      fun printU_string g=general_print_string
        (fn {from:int,to:int} => from>=to) g
   end (* local *)
end (* struct *)
