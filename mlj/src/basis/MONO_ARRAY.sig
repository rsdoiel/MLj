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

signature MONO_ARRAY = sig

     eqtype array
     type elem
     type vector
     val maxLen : int 
     val array : (int * elem) -> array 
     val fromList : elem list -> array 
     val tabulate : (int * (int -> elem)) -> array 
     val length : array -> int 
     val sub : (array * int) -> elem 
     val update : (array * int * elem) -> unit 
     val extract : (array * int * int option) -> vector 
     val copy : {src : array, si : int, len : int option, dst :
     array, di : int} -> unit 
     val copyVec : {src : vector, si : int, len : int option,
     dst : array, di : int} -> unit 
     val appi : ((int * elem) -> unit) -> (array * int * int option)
     -> unit 
     val app : (elem -> unit) -> array -> unit 
     val foldli : ((int * elem * 'b) -> 'b) -> 'b -> (array * int *
     int option) -> 'b 
     val foldri : ((int * elem * 'b) -> 'b) -> 'b -> (array * int *
     int option) -> 'b 
     val foldl : ((elem * 'b) -> 'b) -> 'b -> array -> 'b 
     val foldr : ((elem * 'b) -> 'b) -> 'b -> array -> 'b 
     val modifyi : ((int * elem) -> elem) -> (array * int * int
     option) -> unit 
     val modify : (elem -> elem) -> array -> unit 

end