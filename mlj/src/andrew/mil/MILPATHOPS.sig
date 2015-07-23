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
(* Path operations.                                                     *)
(*======================================================================*)
signature MILPATHOPS =
sig

val toString : MILPath.Path -> string
val eq       : MILPath.Path * MILPath.Path -> bool
val eqItem   : MILPath.Item * MILPath.Item -> bool
val join     : MILPath.Path * MILPath.Path -> MILPath.Path

structure Map : 
sig
  type 'a map
  val find : 'a map * MILPath.Path -> 'a list
  val insert : 'a map * MILPath.Path * 'a -> 'a map
  val empty : 'a map
  val toString : ('a -> string) -> ('a map -> string)
end

end

