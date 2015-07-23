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

signature STRING_CVT =
sig

datatype radix = 
  BIN 
| OCT 
| DEC 
| HEX

datatype realfmt = 
  SCI of int option	
| FIX of int option   
| GEN of int option 	
| EXACT

type cs
type ('a, 'b) reader = 'b -> ('a * 'b) option

val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option
val scanList   : ((char, char list) reader -> ('a, char list) reader) 
                 -> char list -> 'a option

val splitl     : (char -> bool) -> (char, 'a) reader -> 'a -> string * 'a
val takel      : (char -> bool) -> (char, 'a) reader -> 'a -> string 
val dropl      : (char -> bool) -> (char, 'a) reader -> 'a -> 'a 
val skipWS     : (char, 'a) reader -> 'a -> 'a 

val padLeft    : char -> int -> string -> string
val padRight   : char -> int -> string -> string

end

