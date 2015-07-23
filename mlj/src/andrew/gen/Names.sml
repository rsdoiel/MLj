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

structure Names :> NAMES =
struct

(*----------------------------------------------------------------------*)
(* Turn a non-negative integer into an alphabetic id.                   *)
(*----------------------------------------------------------------------*)
fun indexToAlpha m =
if m < 26 then Char.toString (Char.chr (Char.ord #"a" + m))
else if m < 52 then Char.toString (Char.chr (Char.ord #"A" + m-26))
else indexToAlpha (m div 52 - 1) ^ indexToAlphaNum (m mod 52)

and indexToAlphaNum m =
if m < 26 then Char.toString (Char.chr (Char.ord #"a" + m))
else if m < 52 then Char.toString (Char.chr (Char.ord #"A" + m-26))
else if m < 62 then Char.toString (Char.chr (Char.ord #"0" + m-52))
else indexToAlphaNum (m div 62 - 1) ^ indexToAlphaNum (m mod 62)

(*----------------------------------------------------------------------*)
(* Turn a non-negative integer into a lowercase alphabetic id.          *)
(*----------------------------------------------------------------------*)
fun indexToLower m =
if m < 26 then Char.toString (Char.chr (Char.ord #"a" + m))
else indexToLower (m div 26 - 1) ^ indexToLowerNum (m mod 26)

and indexToLowerNum m =
if m < 26 then Char.toString (Char.chr (Char.ord #"a" + m))
else if m < 36 then Char.toString (Char.chr (Char.ord #"0" + m-26))
else indexToLowerNum (m div 36 - 1) ^ indexToLowerNum (m mod 36)

end



