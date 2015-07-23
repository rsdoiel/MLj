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
(* Standard basis SUBSTRING signature, copied directly from SML/NJ site.*)
(* AJK, 21/10/97                                                        *)
(*======================================================================*)
signature SUBSTRING =
sig
     structure String : STRING
     type substring
     val base : substring -> (String.string * int * int) 
     val string : substring -> String.string 
     val extract : (String.string * int * int option) -> substring 
     val substring : (String.string * int * int) -> substring 
     val all : String.string -> substring 
     val isEmpty : substring -> bool 
     val getc : substring -> (String.Char.char * substring) option 
     val first : substring -> String.Char.char option 
     val triml : int -> substring -> substring 
     val trimr : int -> substring -> substring 
     val slice : (substring * int * int option) -> substring 
     val sub : (substring * int) -> char 
     val size : substring -> int 
     val concat : substring list -> String.string 
     val explode : substring -> String.Char.char list 
     val isPrefix : String.string -> substring -> bool 
     val compare : (substring * substring) -> order 
     val collate : ((String.Char.char * String.Char.char) -> order)
     -> (substring * substring) -> order 
     val splitl : (String.Char.char -> bool) -> substring ->
     (substring * substring) 
     val splitr : (String.Char.char -> bool) -> substring ->
     (substring * substring) 
     val splitAt : (substring * int) -> (substring * substring) 
     val dropl : (String.Char.char -> bool) -> substring ->
     substring 
     val dropr : (String.Char.char -> bool) -> substring ->
     substring 
     val takel : (String.Char.char -> bool) -> substring ->
     substring 
     val taker : (String.Char.char -> bool) -> substring ->
     substring 
     val position : String.string -> substring -> (substring *
     substring) 
     val translate : (String.Char.char -> String.string) ->
     substring -> String.string 
     val tokens : (String.Char.char -> bool) -> substring ->
     substring list 
     val fields : (String.Char.char -> bool) -> substring ->
     substring list 
     val foldl : ((String.Char.char * 'a) -> 'a) -> 'a -> substring
     -> 'a 
     val foldr : ((String.Char.char * 'a) -> 'a) -> 'a -> substring
     -> 'a 
     val app : (String.Char.char -> unit) -> substring -> unit 

end