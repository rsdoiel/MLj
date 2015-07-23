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

(*----------------------------------------------------------------------*)
(* Miscellaneous stuff							*)
(*----------------------------------------------------------------------*)
structure Gen =
struct

(*----------------------------------------------------------------------*)
(* The identity function!						*)
(*----------------------------------------------------------------------*)
fun id x = x

(*----------------------------------------------------------------------*)
(* Convert a value of type 'a option into one of type 'a list		*)
(*----------------------------------------------------------------------*)
fun optToList NONE = []
  | optToList (SOME v) = [v]

(*----------------------------------------------------------------------*)
(* Map, foldr and foldl with position in list  				*)
(*----------------------------------------------------------------------*)
fun foldri f acc xs = 
  let val (result,_) = 
    foldr (fn (x,(acc,n)) => (f (n-1,x,acc), n-1)) (acc,length xs) xs
  in result end

fun foldli f acc xs = 
  let val (result,_) = 
    foldl (fn (x,(acc,n)) => (f (n,x,acc), n+1)) (acc,0) xs
  in result end

fun mapi f xs =
let fun map' n [] = []
      | map' n (x::xs) = f(n,x) :: map' (n+1) xs
in
  map' 0 xs
end

fun appi f xs =
let fun app' n [] = ()
      | app' n (x::xs) = (f(n,x); app' (n+1) xs)
in
  app' 0 xs
end

fun hd [] = NONE
  | hd (x::xs) = SOME x

fun unimplemented (funname, message) =   
  raise Fail (funname ^ ": " ^ message ^ " not implemented yet")

(*----------------------------------------------------------------------*)
(* What position is string s in map m?					*)
(*----------------------------------------------------------------------*)
fun lookup (m,s:string) =
let
  fun find i [] = NONE
    | find i ((s',v)::rest) = if s=s' then SOME (v,i) else find (i+1) rest
in
  find 0 m
end



end
