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
(* Naive detection of duplicates in a list				*)
(*======================================================================*)
structure Dups =
struct

(*----------------------------------------------------------------------*)
(* Given a list and an equality function, return a list of elements	*)
(* that are duplicated.                                                 *)
(*----------------------------------------------------------------------*)
fun duplicates eq xs =
let
  fun gather ([], dups) = dups
    | gather (x::xs, dups) = 
      if List.exists (fn x' => eq(x,x')) dups 
      then gather (xs, dups)
      else if List.exists (fn x' => eq(x,x')) xs
      then gather (xs, x::dups)
      else gather (xs, dups)
in
  gather (xs, [])
end

(*----------------------------------------------------------------------*)
(* Given a list of strings xs return a list of the duplicate elements.  *)
(*----------------------------------------------------------------------*)
fun duplicateStrings xs =
let
  fun check [] (set, dups) = dups
    | check (x::xs) (set, dups) =
      if Set.member(set, x)
      then check xs (set, Set.add(dups, x))
      else check xs (Set.add(set, x), dups)
in
  Set.listItems (check xs (Set.empty, Set.empty))
end

(*----------------------------------------------------------------------*)
(* Given a list of atoms xs return a list of the duplicate elements.    *)
(*----------------------------------------------------------------------*)
fun duplicateAtoms xs =
let
  fun check [] (set, dups) = dups
    | check (x::xs) (set, dups) =
      if Symbol.OrdSet.member(set, x)
      then check xs (set, Symbol.OrdSet.add(dups, x))
      else check xs (Symbol.OrdSet.add(set, x), dups)
in
  Symbol.OrdSet.listItems (check xs (Symbol.OrdSet.empty, Symbol.OrdSet.empty))
end

(*----------------------------------------------------------------------*)
(* Given a list of (atom,value) pairs return the duplicates as a list   *)
(* of (atom,value_list) pairs.                                          *)
(*----------------------------------------------------------------------*)
fun duplicateAtoms' xs =
let
  fun addValue (dups, a, v) =
  case Symbol.OrdMap.find(dups, a) of
    NONE => Symbol.OrdMap.insert(dups, a, [v])
  | SOME vs => Symbol.OrdMap.insert(dups, a, v::vs)
    
  fun check [] (set, dups) = dups
    | check ((a,v)::xs) (set, dups) =
      if Symbol.OrdSet.member(set, a)
      then check xs (set, addValue(dups, a, v))
      else check xs (Symbol.OrdSet.add(set, a), dups)
in
  Symbol.OrdMap.listItemsi 
  (check xs (Symbol.OrdSet.empty, Symbol.OrdMap.empty))
end

(*----------------------------------------------------------------------*)
(* Remove duplicates using given equality function			*)
(*----------------------------------------------------------------------*)
fun removeDups eq [] = []
  | removeDups eq (x::xs) =
    if List.exists (fn x' => eq(x,x')) xs then removeDups eq xs
    else x :: removeDups eq xs


end
