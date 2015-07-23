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

(* Print:PRINT contains functions for prettyprinting an (int*'a) graph.
   Parse.parseD_string
   o PRINT.printD_string is the identity on (int,unit) graphs where
   the ints are distinct and consecutive from 1. Parse.parseU_string o
   Print.printU_string is the identity on (int,unit) symmetric graphs
   where the ints are distinct and consecutive from 1.  printU_string
   omits all arcs which go from a vertex label x to a vertex label y with
   x>y, thus saving space for symmetric graphs. *)
signature PRINT=
sig
   val printD_string:(int,'a) Graph.graph->string
   val printU_string:(int,'a) Graph.graph->string
end
