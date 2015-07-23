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
(* Standard basis WORD signature, copied directly from SML/NJ site.	*)
(* AJK, 17/10/97                                                        *)
(*======================================================================*)
signature WORD =
sig

     eqtype word
     val wordSize : int 
     val toLargeWord : word -> LargeWord.word
     val toLargeWordX : word -> LargeWord.word
     val fromLargeWord : LargeWord.word -> word 
     val toLargeInt : word -> LargeInt.int
     val toLargeIntX : word -> LargeInt.int
     val fromLargeInt : LargeInt.int -> word 
     val toInt : word -> Int.int 
     val toIntX : word -> Int.int 
     val fromInt : Int.int -> word 
     val orb : (word * word) -> word 
     val xorb : (word * word) -> word 
     val andb : (word * word) -> word 
     val notb : word -> word 
     val << : (word * Word.word) -> word 
     val >> : (word * Word.word) -> word 
     val ~>> : (word * Word.word) -> word 
     val + : (word * word) -> word 
     val - : (word * word) -> word 
     val * : (word * word) -> word 
     val div : (word * word) -> word 
     val mod : (word * word) -> word 
     val compare : (word * word) -> order 
     val > : (word * word) -> bool 
     val < : (word * word) -> bool 
     val >= : (word * word) -> bool 
     val <= : (word * word) -> bool 
     val min : (word * word) -> word 
     val max : (word * word) -> word 
     val fmt : StringCvt.radix -> word -> string 
     val toString : word -> string 
     val fromString : string -> word option 
     val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> 'a
     -> (word * 'a) option 

end