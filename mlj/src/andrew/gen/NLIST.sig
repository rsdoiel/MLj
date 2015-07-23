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
signature NLIST =
sig

type 'a list = 'a * 'a List.list

val singleton  : 'a -> 'a list
val toList     : 'a list -> 'a List.list
val hd         : 'a list -> 'a                          
val tl         : 'a list -> 'a List.list                     
val last       : 'a list -> 'a                          

val nth        : 'a list * int -> 'a         

val length     : 'a list -> int 

val concat     : 'a List.list list -> 'a List.list
val app        : ('a -> unit) -> 'a list -> unit
val map        : ('a -> 'b) -> 'a list -> 'b list

val find       : ('a -> bool) -> 'a list -> 'a option

val foldr      : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val foldl      : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

val exists     : ('a -> bool) -> 'a list -> bool
val all        : ('a -> bool) -> 'a list -> bool

end
