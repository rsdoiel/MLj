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
(* MIL exceptions. See signature for more info.                         *)
(*======================================================================*)
structure MILExn :> MILEXN =
struct

type Exn = Entity.Ref * int * Syntax.longid

fun toString (e,n,_) = EntityOps.toString e ^ "." ^ Int.toString n

fun eq ((entity1,i1,_),(entity2,i2,_)) = 
  Entity.Ord.compare(entity1,entity2) = EQUAL andalso i1=i2

fun compare ((entity1,i1,_),(entity2,i2,_)) =
  case Int.compare (i1,i2) of
    EQUAL => Entity.Ord.compare(entity1,entity2) 
  | other => other

fun info (entity,i,longid) = longid

fun exn p = p

(*----------------------------------------------------------------------*)
(* Return a hash code for an exception constructor			*)
(*----------------------------------------------------------------------*)
fun hash ((entity,i,_) : Exn) = 
  Symbol.HashKey.hashVal (#2 entity) + Word.fromInt i

end
