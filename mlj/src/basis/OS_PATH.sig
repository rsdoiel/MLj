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

signature OS_PATH =
  sig
    exception Path
    val parentArc : string
    val currentArc : string
    val validVolume : {isAbs:bool, vol:string} -> bool
    val fromString : string -> {arcs:string list, isAbs:bool, vol:string}
    val toString : {arcs:string list, isAbs:bool, vol:string} -> string
    val getVolume : string -> string
    val getParent : string -> string
    val splitDirFile : string -> {dir:string, file:string}
    val joinDirFile : {dir:string, file:string} -> string
    val dir : string -> string
    val file : string -> string
    val splitBaseExt : string -> {base:string, ext:string option}
    val joinBaseExt : {base:string, ext:string option} -> string
    val base : string -> string
    val ext : string -> string option
    val mkCanonical : string -> string
    val isCanonical : string -> bool
    val mkAbsolute : string * string -> string
    val mkRelative : string * string -> string
    val isAbsolute : string -> bool
    val isRelative : string -> bool
    val isRoot : string -> bool
    val concat : string * string -> string
  end
