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

signature MONO_VECTOR =
sig

  eqtype vector
  eqtype elem
  val maxLen : int 
  val fromList : elem list -> vector 
  val tabulate : (int * (int -> elem)) -> vector 
  val length : vector -> int 
  val sub : (vector * int) -> elem 
  val extract : (vector * int * int option) -> vector 
  val concat : vector list -> vector 
  val appi : ((int * elem) -> unit) -> (vector * int * int option) -> unit 
  val app : (elem -> unit) -> vector -> unit 
  val foldli : ((int*elem*'a) -> 'a) -> 'a -> (vector*int*int option) -> 'a 
  val foldri : ((int*elem*'a) -> 'a) -> 'a -> (vector*int*int option) -> 'a 
  val foldl : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a 
  val foldr : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
  val map : (elem -> elem) -> vector -> vector
  val mapi : (int * elem -> elem) -> vector * int * int option -> vector

end