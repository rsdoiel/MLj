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
(* General pretty-printing routines					*)
(*======================================================================*)
signature PRETTY =
sig

(* The unit used for indentation of hierarchies. Default is 2. *)
val indentUnit : int ref

val indexToString : int -> string

(* SML identifiers and long identifiers. Expand out non-ASCII characters. *)
val idToString : Symbol.symbol -> string
val longidToString : Symbol.symbol list -> string

val vec : string * string * string * string * string * string
    -> ('a -> string) -> 'a list -> string
val simpleVec : string -> ('a -> string) -> 'a list -> string
val bigVec : int -> ('a -> string) -> 'a list -> string
val parens : bool -> string -> string
val newline : int -> string

end
