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

(* The IntConv functor converts an INTOPS structure into a fromString
   function. *)
signature INTCONV=
sig
   type t
   val fromString:IntConvFlags.Base->IntConvFlags.Kind->string->t option
   (* fromString converts a string of digits into a t according to the
      base and kind supplied, returning NONE if there is an overflow.
      See IntConvFlags for the meanings of the flags.

      Warning: fromString does not check that the string is indeed a string
      of digits since all that is assumed to have been checked by the lexer.
      *)
end
