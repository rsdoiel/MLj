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
(* Standard basis CHAR signature, copied directly from SML/NJ site.	*)
(* AJK, 21/10/97                                                        *)
(*======================================================================*)
signature CHAR =
sig

eqtype char
eqtype string

val minChar : char
val maxChar : char
val maxOrd  : int

val ord     : char -> int
val chr     : int  -> char
val succ    : char -> char
val pred    : char -> char

val <  : char * char -> bool
val <= : char * char -> bool
val >  : char * char -> bool
val >= : char * char -> bool
val compare : char * char -> order

val contains    : string -> char -> bool
val notContains : string -> char -> bool

val toLower     : char -> char
val toUpper     : char -> char

val isAlpha     : char -> bool
val isAlphaNum  : char -> bool
val isAscii     : char -> bool
val isCntrl     : char -> bool
val isDigit     : char -> bool
val isGraph     : char -> bool
val isHexDigit  : char -> bool
val isLower     : char -> bool
val isPrint     : char -> bool
val isPunct     : char -> bool
val isSpace     : char -> bool
val isUpper     : char -> bool

val fromString  : String.string -> char option
val toString    : char -> String.string       

val fromCString : String.string -> char option
val toCString   : char -> String.string       

val scan        : ('a -> (Char.char * 'a) option) -> 'a -> (char * 'a) option

end
