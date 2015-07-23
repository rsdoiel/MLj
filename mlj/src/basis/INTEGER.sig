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
(* Standard basis INTEGER signature, copied directly from SML/NJ site.	*)
(* AJK, 17/10/97                                                        *)
(*======================================================================*)
signature INTEGER =
sig

     eqtype int
     val toLarge : int -> LargeInt.int
     val fromLarge : LargeInt.int -> int
     val toInt : int -> Int.int 

     val fromInt : Int.int -> int 
     val precision : int option 
     val minInt : int option 
     val maxInt : int option 
     val ~ : int -> int 
     val * : (int * int) -> int 
     val div : (int * int) -> int 
     val mod : (int * int) -> int 
     val quot : (int * int) -> int 
     val rem : (int * int) -> int 
     val + : (int * int) -> int 
     val - : (int * int) -> int 
     val compare : (int * int) -> order 
     val > : (int * int) -> bool 
     val >= : (int * int) -> bool 
     val < : (int * int) -> bool 
     val <= : (int * int) -> bool 
     val abs : int -> int 
     val min : (int * int) -> int 
     val max : (int * int) -> int 
     val sign : int -> Int.int 
     val sameSign : (int * int) -> bool 
     val fmt : StringCvt.radix -> int -> string 
     val toString : int -> string 
     val fromString : string -> int option 
     val scan : StringCvt.radix -> ('a -> (char * 'a) option) -> 'a
     -> (int * 'a) option 

end