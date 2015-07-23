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

signature VECTOR = 
sig

     type 'a vector = 'a vector
     val maxLen : int 
     val fromList : 'a list -> 'a vector 
     val tabulate : (int * (int -> 'a)) -> 'a vector 
     val length : 'a vector -> int 
     val sub : ('a vector * int) -> 'a 
     val extract : ('a vector * int * int option) -> 'a vector 
     val concat : 'a vector list -> 'a vector 
     val mapi : ((int * 'a) -> 'b) -> ('a vector * int * int option)
     -> 'b vector 
     val map : ('a -> 'b) -> 'a vector -> 'b vector 
     val appi : ((int * 'a) -> unit) -> ('a vector * int * int
     option) -> unit 
     val app : ('a -> unit) -> 'a vector -> unit 
     val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> ('a vector * int
     * int option) -> 'b 
     val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> ('a vector * int
     * int option) -> 'b 
     val foldl : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b 
     val foldr : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b 

end

