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
(* Make a whole project							*)
(*======================================================================*)
signature MAKE =
sig

  val exports : (string list * string) list ref
  val opts : string list ref

  (* Compile and link the current project *)
  val make : unit -> bool

  (* Compile (don't link) the basis project *)
  val makeBasis : string list -> bool

  (* Compile (don't link) the basis project; then freeze it *)
  (* First arg true for Java 1.1; second arg is a log file name *)
  val makeAndFreezeBasis : bool * string -> bool

  (* Roots for basis *)
  val basisStrs : string list ref

  (* Return name of class containing main, if a unique such class exists *)
  val getMainClass : unit -> string option

end