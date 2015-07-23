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

(* List -- SML Basis Library *)

signature LIST =
sig

datatype list = datatype Datatypes.list

exception Empty  (* Subscript and Size *)

val null       : 'a list -> bool
val hd         : 'a list -> 'a                          (* Empty     *)
val tl         : 'a list -> 'a list                     (* Empty     *)
val last       : 'a list -> 'a                          (* Empty     *)

val nth        : 'a list * int -> 'a                    (* Subscript *)
val take       : 'a list * int -> 'a list               (* Subscript *)
val drop       : 'a list * int -> 'a list               (* Subscript *)

val length     : 'a list -> int 

val rev        : 'a list -> 'a list 

val @          : 'a list * 'a list -> 'a list
val concat     : 'a list list -> 'a list
val revAppend  : 'a list * 'a list -> 'a list

val app        : ('a -> unit) -> 'a list -> unit
val map        : ('a -> 'b) -> 'a list -> 'b list
val mapPartial : ('a -> 'b option) -> 'a list -> 'b list

val find       : ('a -> bool) -> 'a list -> 'a option
val filter     : ('a -> bool) -> 'a list -> 'a list
val partition  : ('a -> bool ) -> 'a list -> ('a list * 'a list)

val foldr      : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val foldl      : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

val exists     : ('a -> bool) -> 'a list -> bool
val all        : ('a -> bool) -> 'a list -> bool

val tabulate   : int * (int -> 'a) -> 'a list           (* Size      *)

end
