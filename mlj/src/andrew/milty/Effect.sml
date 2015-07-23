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

structure Effect :> EFFECT
= struct

(*----------------------------------------------------------------------*)
(* Bit twiddling isn't very ML, but in the absence of a Pascal-like	*)
(* set types, it's hard to avoid...                                     *)
(*----------------------------------------------------------------------*)
type Effect = word

val partial = 0wx1
val throws  = 0wx2
val reads   = 0wx4
val writes  = 0wx8
val io      = 0wx10
val allocs  = 0wx20

val none    = 0wx0
val any     = 0wx3f

val union        = Word.orb
val intersection = Word.andb

fun sub (eff1, eff2) = Word.andb(eff1, Word.notb eff2) = 0w0

fun toString e =
  (if Word.andb(e, partial) <> 0w0 then "!" else "") ^
  (if Word.andb(e, throws) <> 0w0 then "t" else "") ^
  (if Word.andb(e, reads) <> 0w0 then "r" else "") ^
  (if Word.andb(e, writes) <> 0w0 then "w" else "") ^
  (if Word.andb(e, io) <> 0w0 then "i" else "") ^
  (if Word.andb(e, allocs) <> 0w0 then "a" else "")

end
