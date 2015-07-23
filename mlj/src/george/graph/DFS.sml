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
functor DFS(M:ORD_SET):>DFS where type key=M.Key.ord_key =
struct
   type key=M.Key.ord_key

   fun simple_dfs neighbours foldfun initial start=
   let
      fun do_fold(node,state as (visited_set,foldfun_state))=
         if M.member(visited_set,node)
         then state
         else
            List.foldl
               do_fold
               (M.add(visited_set,node),foldfun(node,foldfun_state))
               (neighbours node)
   in
      #2(do_fold(start,(M.empty,initial)))
   end

   fun dfs_combined neighbours initial start=
   let
      fun do_fold(node,state as (visited_set,neighbours_state))=
         if M.member(visited_set,node)
         then state
         else let
            val (nhbrs,new_state)=neighbours(node,neighbours_state)
         in
            List.foldl
               do_fold
               (M.add(visited_set,node),new_state)
               nhbrs
         end
   in
      #2(do_fold(start,(M.empty,initial)))
   end
end
