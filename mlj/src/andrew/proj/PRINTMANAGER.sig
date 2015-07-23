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
(* Printing within the interactive environment; also copied to the log.	*)
(*======================================================================*)
signature PRINTMANAGER =
sig

(*----------------------------------------------------------------------*)
(* Print directly the console and the log				*)
(*----------------------------------------------------------------------*)
val print : string -> unit

(*----------------------------------------------------------------------*)
(* Print a bunch of errors, indented correctly.  			*)
(*----------------------------------------------------------------------*)
val printErrors : SourceMap.sourcemap * Error.Error list -> unit

(*----------------------------------------------------------------------*)
(* process (message,oblig) f:                                           *)
(*   perform some process f with the descriptive message supplied.      *)
(*   don't print anything if oblig=false and "verbose" is off.          *)
(*   if f () = NONE then there were errors.                             *)
(*----------------------------------------------------------------------*)
val process : string*bool -> (unit -> 'a) -> 'a 

val printTime : string -> Timer.cpu_timer -> unit

val restart : unit -> unit

end