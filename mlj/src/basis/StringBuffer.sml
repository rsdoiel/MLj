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
(* Auxiliary structure that wraps up StringBuffer.			*)
(*======================================================================*)
structure StringBuffer :> STRINGBUFFER =
struct

type StringBuffer = java.lang.StringBuffer

fun fromString s = _new StringBuffer(s : string)
fun toString (sb : StringBuffer) = Prim.unsafeValOf(sb.#toString())

fun empty () = _new StringBuffer ()

fun emptyWith n = _new StringBuffer (n : int)

fun appendString (sb : StringBuffer, s : string) = 
  General.ignore (sb.#append (s))

fun appendChar (sb : StringBuffer, c : char) =
  General.ignore (sb.#append (c))

fun appendInt64 (sb : StringBuffer, l : Prim.long) =
  General.ignore (sb.#append (l))

fun appendInt32 (sb : StringBuffer, l : int) =
  General.ignore (sb.#append (l))
end

