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

(* DFS:DFS is a functor which takes an ORD_SET to a structure which
   performs depth-first searches *)
signature DFS=
sig
   type key
   val simple_dfs:(key->key list)->(key*'a->'a)->'a->key->'a
   (* simple_dfs neighbours foldfun initial start
      performs a depth-first search on nodes of type key, starting at
      start.  Thus it visits a sequence of nodes start=n_1,...,n_r
      It returns foldfun(n_r,...,(foldfun(n_1,initial))...)
      *)

   val dfs_combined:(key*'a->(key list)*'a)->'a->key->'a
   (* dfs_combined neighbours initial start
      performs a depth-first search, calling neighbours each time it
      visits a node, and letting neighbours accumulate state in 'a,
      which is initially initial; the final state is returned. *)
end
