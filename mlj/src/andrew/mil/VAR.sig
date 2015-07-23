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
signature VAR =
sig

(* Expose these to direct fiddling from outside *)
type Supply = int
type Var = int

(* Sets and maps for variables *)
structure Set : ORD_SET where type Key.ord_key = Var
structure Map : ORD_MAP where type Key.ord_key = Var

(* Use e.g. for unused arguments to functions *)
val dummy : Var
val isDummy : Var -> bool

(* Initial state for variable supply *)
val initial : Supply

(* Generate a fresh variable *)
val fresh : Supply -> Supply * Var

(* Pretty print a variable in alphabetic form *)
val toString : Var -> string

(* Insert multiple entries into a variable map *)
val extend : 'a Map.map * (Var * 'a) list -> 'a Map.map

(* Look up a variable in a map, forcing a fatal error if it's not there *)
val lookup : 'a Map.map * Var -> 'a

(* Equality on variables *)
val eq : Var * Var -> bool

end
