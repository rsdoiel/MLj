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

signature ARRAY =
sig

     type 'a array = 'a array
     type 'a vector = 'a vector
     val maxLen : int 
     val array : (int * 'a) -> 'a array 
     val fromList : 'a list -> 'a array 
     val tabulate : (int * (int -> 'a)) -> 'a array 
     val length : 'a array -> int 
     val sub : ('a array * int) -> 'a 
     val update : ('a array * int * 'a) -> unit 
     val extract : ('a array * int * int option) -> 'a vector 
     val copy : {src : 'a array, si : int, len : int option, dst :
     'a array, di : int} -> unit 
     val copyVec : {src : 'a vector, si : int, len : int option, dst
     : 'a array, di : int} -> unit 
     val appi : ((int * 'a) -> unit) -> ('a array * int * int
     option) -> unit 
     val app : ('a -> unit) -> 'a array -> unit 
     val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int *
     int option) -> 'b 
     val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int *
     int option) -> 'b 
     val foldl : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b 
     val foldr : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b 
     val modifyi : ((int * 'a) -> 'a) -> ('a array * int * int
     option) -> unit 
     val modify : ('a -> 'a) -> 'a array -> unit 

end