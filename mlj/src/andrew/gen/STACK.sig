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
(* Stacks with constant-time size operation.				*)
(*======================================================================*)
signature STACK =
sig

type 'a stack

val empty      : 'a stack
val isEmpty    : 'a stack -> bool
val push       : 'a stack * 'a -> 'a stack
val pop        : 'a stack -> 'a stack * 'a
val numItems   : 'a stack -> int
val find       : ('a -> bool) -> 'a stack -> (int * 'a) option
val map        : ('a -> 'b) -> 'a stack -> 'b stack
val listItems  : 'a stack -> 'a list
val appi       : (int * 'a -> unit) -> 'a stack -> unit
val nth        : ('a stack * int) -> 'a

end