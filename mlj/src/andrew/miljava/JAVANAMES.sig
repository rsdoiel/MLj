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
(* Various names for labels, methods and classes                        *)
(* See the structure file for more info.                                *)
(*======================================================================*)
signature JAVANAMES =
sig

val globalClassName : string
val globalClass     : ClassHandle.Handle

val conClassName    : int option -> string
val conClass        : int option -> ClassHandle.Handle

val prodClassName   : int -> string
val prodClass       : int -> ClassHandle.Handle

val exnClassName    : int option -> string
val exnClass        : int option -> ClassHandle.Handle

val closClassName   : int option -> string
val closClass       : int option -> ClassHandle.Handle

val classClassName  : int -> string
val classClass      : int -> ClassHandle.Handle

val sumTag          : string
val globalMethod    : int -> string
val appMethod       : int -> string
val noneVal         : int -> string
val argLabel        : int -> string
val exnClassCount   : string
val exnLocMessage   : string
val allocCount      : string

val exnNameMethod   : string
val exnMessageMethod: string

end
