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
(* Parse manager: manage a cache containing complete and reduced syntax	*)
(* trees.                                                               *)
(*======================================================================*)
signature PARSEMANAGER =
sig

(*----------------------------------------------------------------------*)
(* Result of calling parse.						*)
(*----------------------------------------------------------------------*)
datatype Result =
  Unchanged           (* File has not been updated so no info returned *)
| NotFound            (* File does not exist *)
| Fail                (* There was a parsing error *)
| Success of          (* Parsing was successful *)
  (Syntax.Dec * SmallSyntax.Dec * SourceMap.sourcemap) * Entity.FileRef option

(*----------------------------------------------------------------------*)
(* parse(entity, NONE) parses the file corresponding to the entity	*)
(*   given, returning Changed(r, time) if successful or Failed if not.  *)
(* parse(entity, SOME time) parses the file corresponding to the        *)
(*   entity given, if its timestamp is more recent than time. If it is  *)
(*   not more recent then Unchanged is returned.                        *)
(*----------------------------------------------------------------------*)
val parse : 
  Entity.Ref * Entity.FileRef option -> Result

(*----------------------------------------------------------------------*)
(* Keep only entities matching this predicate; even for these remove	*)
(* the cached syntax tree and keep only the reduced syntax.             *) 
(*----------------------------------------------------------------------*)
val freeze    : (Entity.Ref -> bool) -> unit
val freezeAll : unit -> unit

end
