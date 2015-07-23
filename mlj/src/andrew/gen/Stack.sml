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
(* Stacks with constant-time size operation				*)
(*======================================================================*)
structure Stack :> STACK =
struct

type 'a stack = int * 'a list

val empty = (0, [])

fun push((n,xs),x) = (n+1, x::xs)
fun pop(n, x::xs) = ((n-1,xs), x)
fun isEmpty (0,_) = true
  | isEmpty _ = false
fun numItems (n,xs) = n

fun map f (n, xs) = (n, List.map f xs)
fun find p (n, xs) = 
let 
  fun find' i [] = NONE
    | find' i (x::xs) = 
      if p x then SOME (i-1,x) else find' (i-1) xs
in
  find' n xs
end

fun listItems (n,xs) = rev xs

fun appi f (n,xs) = 
let
  fun app i [] = ()
    | app i (x::xs) = (f (i-1,x); app (i-1) xs)
in
  app n xs
end

fun nth ((n,xs),i) = List.nth(xs, (n-i-1))

end
