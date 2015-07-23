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

structure IntPartition :> INTPARTITION =
struct

type elem = int
type partition = int

datatype Entry = 
  Parent of int
| Weight of int

type partition = Entry IMap.map

fun root s i = 
  case IMap.find(s, i) of
    Parent j => root s j
  | Weight w => (i, w)

fun updateParent p s i = 
  case IMap.find(s, i) of
    Parent j => updateParent p (RandomAccessList.update s i (Parent p)) j
  | Weight w => s

fun eq (x,y) s =
let
  val (xroot, xweight) = root s x
  val (yroot, yweight) = root s y

  val s = updateParent xroot s x
  val s = updateParent yroot s y
in
  (s, xroot = yroot)
end  

fun union (x,y) s =
let
  val (xroot, xweight) = root s x
  val (yroot, yweight) = root s y

  val s = updateParent xroot s x
  val s = updateParent yroot s y
in
  if xroot = yroot 
  then s
  else 
  let 
    val weight = xweight + yweight + 1
  in
    if yweight > xweight 
    then RandomAccessList.update 
           (RandomAccessList.update s xroot (Parent yroot)) 
           yroot (Weight weight)
    else RandomAccessList.update 
           (RandomAccessList.update s yroot (Parent xroot)) 
           xroot (Weight weight)
  end
end

fun fresh s = 
  let val n = RandomAccessList.length s
  in
    (RandomAccessList.cons (Weight 0) s, n)
  end

val empty = RandomAccessList.empty

fun rep x s = let val (r, _) = root s x in r end

end
