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
(* Process the export command.						*)
(*======================================================================*)
signature PROCESSEXPORTS =
sig

(*----------------------------------------------------------------------*)
(* Argument: a list of long identifiers paired with actual class names.	*)
(* Single identifiers are interpreted as structure identifiers,         *)
(* qualified identifiers are interpreted as class type identifiers.     *)
(*									*)
(* Result: 								*)
(*  NONE if an error occurred.						*)
(*  SOME (tynames, mainclassopt)					*)
(*   otherwise, where tynames maps type names to actual class names and *)
(*   mainclassopt is the name of a class containing a main method, if   *)
(*   one exists.							*)
(*----------------------------------------------------------------------*)
val process   : 
  (string list * string) list -> 
  ((TyName.TyName * string) list * string option) option

end

