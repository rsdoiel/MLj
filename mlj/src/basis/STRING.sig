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

signature STRING =
sig

    eqtype string

    structure Char : CHAR

    val maxSize   : int
    val size      : string -> int
    val sub       : string * int -> Char.char
    val substring : string * int * int -> string
    val extract   : string * int * int option -> string
    val concat    : string list -> string
    val ^         : string * string -> string
    val str       : Char.char -> string
    val implode   : Char.char list -> string 
    val explode   : string -> Char.char list

    val translate : (Char.char -> string) -> string -> string
    val tokens    : (Char.char -> bool) -> string -> string list
    val fields    : (Char.char -> bool) -> string -> string list
    val isPrefix  : string -> string -> bool

    val compare   : string * string -> order
    val collate   : (Char.char * Char.char -> order) -> string*string -> order

    val fromString  : string -> string option     (* ML escape sequences *)
    val toString    : string -> string            (* ML escape sequences *)
    val fromCString : string -> string option     (* C escape sequences *)
    val toCString   : string -> string            (* C escape sequences *)

    val <  : string * string -> bool
    val <= : string * string -> bool
    val >  : string * string -> bool
    val >= : string * string -> bool

end

