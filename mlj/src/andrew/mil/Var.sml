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
(* Variable names and environments for MIL types and terms		*)
(*======================================================================*)
structure Var :> VAR =
struct

structure Set = IntBinarySet

structure Map = IntBinaryMap

type Var = int
type Supply = int

val dummy = 0
fun isDummy v = v=0
val initial = 1
fun fresh supply = (supply+1, supply)

fun eq (v1:Var,v2:Var) = v1=v2

fun extend (env, vs) = 
  foldr (fn ((v,x),env) => Map.insert(env, v, x)) env vs

fun toString v = if isDummy v then "*" else Pretty.indexToString v

fun lookup (env, v) = 
  case Map.find(env, v) of
    NONE => raise Fail ("Var.lookup: variable " ^ toString v ^ " not found")
  | SOME x => x

end
