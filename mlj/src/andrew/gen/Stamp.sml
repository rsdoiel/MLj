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
(* Stamps                                 				*)
(*======================================================================*)
functor Stamp(type Info) =
struct

datatype Stamp =
  Stamp of Entity.Ref * int * Info

type Supply = Entity.Ref * int

(*----------------------------------------------------------------------*)
(* Sets of type names and finite maps from type names.			*)
(*----------------------------------------------------------------------*)
structure StampOrd =
  struct
    type ord_key = Stamp
    fun compare (Stamp(e1,i1,_), Stamp(e2,i2,_)) = 
      (case String.compare(EntityOps.pickleRef e1, EntityOps.pickleRef e2) of
        EQUAL => Int.compare(i1, i2)
      | other => other)
  end

structure Set = SetFn(StampOrd)
structure Map = MapFn(StampOrd)

type Renaming = Stamp Map.map

fun rename r stamp =
case Map.find(r, stamp) of
  NONE => stamp
| SOME stamp' => stamp'

(*----------------------------------------------------------------------*)
(* Equality on stamps        						*)
(*----------------------------------------------------------------------*)
fun eq (Stamp(e1,i1,_), Stamp(e2,i2,_)) = e1=e2 andalso i1=i2

(*----------------------------------------------------------------------*)
(* Stamp supply functions						*)
(*----------------------------------------------------------------------*)
fun initial entity = (entity,1)
fun fresh info (entity,i) = (Stamp(entity,i,info), (entity,i+1))

fun replaceInfo (Stamp(entity,i,info)) info' = Stamp(entity,i,info')

fun entity (Stamp(entity,_,_)) = entity

(*----------------------------------------------------------------------*)
(* Was a tyname generated earlier than the point specified?     	*)
(*----------------------------------------------------------------------*)
fun earlier (Stamp(entity,i,_), (entity', i')) = 
  entity <> entity' orelse i<i'

(*----------------------------------------------------------------------*)
(* Display a stamp							*)
(*----------------------------------------------------------------------*)
fun toString (Stamp(entity,i,_)) = 
  if Controls.isOn "showStamps" 
  then "(" ^ EntityOps.pickleRef entity ^ Int.toString i ^ ")" 
  else ""

fun info (Stamp(_, _, info)) = info

end
