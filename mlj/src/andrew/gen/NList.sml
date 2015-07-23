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
(* Non-empty list type & standard operations.				*)
(* This is useful for static enforcement of non-empty-ness -- no need   *)
(* for nonexhaustive case analysis or "raise Bug" constructs.           *) 
(*======================================================================*)
structure NList :> NLIST =
struct

type 'a list = 'a * 'a List.list

val toList = op::

fun singleton x = (x,[])
fun hd (x,y) = x
fun tl (x,y) = y
fun last (x,y) = if null y then x else List.last y

fun nth (p,i) = List.nth(op:: p, i)

fun length (x,y) = 1 + List.length y

fun concat (x,y) = x @ List.concat y
fun app f (x,y) = (f x; List.app f y)
fun map f (x,y) = (f x, List.map f y)
fun find f (x,y) = if f x then SOME x else List.find f y

fun foldr f i (x,y) = List.foldr f i (x::y)
fun foldl f i (x,y) = List.foldl f i (x::y)

fun exists f (x,y) = f x orelse List.exists f y
fun all f (x,y) = f x andalso List.all f y

end
