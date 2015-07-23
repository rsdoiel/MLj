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

signature TIME =
sig
     eqtype time
     exception Time
     val zeroTime : time 
     val fromReal : LargeReal.real -> time 
     val toReal : time -> LargeReal.real 
     val toSeconds : time -> LargeInt.int 
     val toMilliseconds : time -> LargeInt.int 
     val toMicroseconds : time -> LargeInt.int 
     val fromSeconds : LargeInt.int -> time 
     val fromMilliseconds : LargeInt.int -> time 
     val fromMicroseconds : LargeInt.int -> time 
     val + : (time * time) -> time 
     val - : (time * time) -> time 
     val compare : (time * time) -> order 
     val < : (time * time) -> bool 
     val <= : (time * time) -> bool 
     val > : (time * time) -> bool 
     val >= : (time * time) -> bool 
     val now : unit -> time 
     val fmt : int -> time -> string 
     val toString : time -> string 
     val fromString : string -> time option 
     val scan : (char, 'a) StringCvt.reader -> 'a -> (time * 'a) option 
end