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
(* Source file manager							*)
(*======================================================================*)
signature SOURCEMANAGER =
sig


(*----------------------------------------------------------------------*)
(* Conversion of signature, structure and functor identifiers to file   *)
(* names works as follows:                                              *)
(* 1. the identifier is looked up in !translation; this produces a      *)
(*    filename which is searched for first in the directories in        *)
(*    and then in !projPath.                                            *)
(* 2. if it's not in !translation, each directory in !basisPath and     *)
(*    !projPath is searched for a filename constructed from the         *)
(*    identifer with extension given by !sigExts, !strExts, or !funExts *)
(*    as appropriate.                                                   *)
(*----------------------------------------------------------------------*)
val translation : string Entity.Map.map ref  (* default is empty    *)

val sigExts     : string list ref       (* default is [".sig"] *)
val strExts     : string list ref       (* default is [".sml"] *)
val funExts     : string list ref       (* default is [".fun", ".sml"] *)

(*----------------------------------------------------------------------*)
(* Paths for project and basis files.					*)
(*----------------------------------------------------------------------*)
val projPath    : string list ref  (* default is ["examples"] *)
val basisPath   : string list ref  (* default is ["basis"] *)

(*----------------------------------------------------------------------*)
(* Cache timestamps from all directories in !basisPath and !projPath.   *)
(* Return true if successful.                               		*)
(*----------------------------------------------------------------------*)
val sync        : unit -> bool

val isFrozen    : unit -> bool
val freezeBasis : unit -> unit

(*----------------------------------------------------------------------*)
(* load(entity, NONE) reads in the file corresponding to the entity	*)
(*   given, returning Changed(s, time) if successful or Failed if not.  *)
(* load(entity, SOME fileref) reads in the file corresponding to the    *)
(*   entity given, if its ref is more recent than fileref. If it is     *)
(*   not more recent then Unchanged is returned.                        *)
(* The boolean indicates whether or not the file is part of the basis.  *)
(*----------------------------------------------------------------------*)
datatype Result =
  Changed of string * bool * Entity.FileRef option
| Failed
| Unchanged

val load : 
  Entity.Ref * Entity.FileRef option -> 
  Result

(*----------------------------------------------------------------------*)
(* Return the full file name associated with a particular entity	*)
(*----------------------------------------------------------------------*)
val fileFor : Entity.Ref -> string option


end
