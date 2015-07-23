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
(* Java package manager. Purpose: to maintain class paths and a map	*)
(* of the packages and classes visible through the class paths.		*)
(*======================================================================*)
signature PACKAGEMANAGER =
sig

type ClassDir
datatype Package = Package of Env
withtype Env = 
  {
    classes : ClassDir Symbol.OrdMap.map,
    packages : Package ref Symbol.OrdMap.map
  }

(*----------------------------------------------------------------------*)
(* The shell command used to unzip a compressed archive.		*)
(*----------------------------------------------------------------------*)
val unzipCommand  : string ref

(*----------------------------------------------------------------------*)
(* Set the class paths							*)
(*----------------------------------------------------------------------*)
val setBootClassPath : string list -> bool
val setClassPath : string list -> bool

(*----------------------------------------------------------------------*)
(* Get the class paths (for display purposes only)			*)
(*----------------------------------------------------------------------*)
val getBootClassPath : unit -> string list
val getClassPath : unit -> string list

(*----------------------------------------------------------------------*)
(* Load a class, given its directory reference.				*)
(*----------------------------------------------------------------------*)
val getClass :
  Syntax.longid ->
  Class.class_data option

(*----------------------------------------------------------------------*)
(* Get at the whole environment						*)
(*----------------------------------------------------------------------*)
val getTop :
  unit -> Package

(*----------------------------------------------------------------------*)
(* Pretty-printing of the package datatype, used for diagnostics only.	*)
(*----------------------------------------------------------------------*)
val packageToString : Package -> string

end

