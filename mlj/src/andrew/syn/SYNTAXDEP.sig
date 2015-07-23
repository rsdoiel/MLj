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
(* Dependency analysis from a list of "root" structures			*)
(*======================================================================*)
signature SYNTAXDEP =
sig

(*----------------------------------------------------------------------*)
(* Dependency information: a record { order, deps, java } where		*)
(*   order is a list of topologically-sorted entities, root last;	*)
(*   deps maps each entity in L to a pair (E,s) where			*)
(*      E is the environment for that entity as described above;	*)
(*	s is the set of entities on which this entity depends.		*)
(*   classes are external classes used as structures.                   *)
(*----------------------------------------------------------------------*)
type Info = 
{
  order : Entity.Ref list,
  deps  : Entity.Set.set Entity.Map.map,
  classes : Syntax.longid list
}

(*----------------------------------------------------------------------*)
(* Given a list of top-level "root" structure identifiers, follow all	*)
(* dependencies and return information as described above.		*)
(* Return NONE in the case of failure:					*)
(*   if there's a parsing error						*) 
(*   if there's a circularity between entities				*)
(*   if there's a missing entity					*)
(* Error messages will always be displayed in this situation.		*)
(*----------------------------------------------------------------------*)
val analyse : string list -> Info option

end