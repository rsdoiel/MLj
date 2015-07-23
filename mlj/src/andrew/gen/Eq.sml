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
(* Operators to lift equality operations to parameterised datatypes	*)
(*======================================================================*)
structure Eq :> EQ = 
struct

fun list eq ([],[]) = true
  | list eq (x::xs,y::ys) = eq(x,y) andalso list eq (xs,ys)
  | list eq _ = false

fun option eq (NONE,NONE) = true
  | option eq (SOME x, SOME y) = eq(x,y)
  | option eq _ = false

(*----------------------------------------------------------------------*)
(* Are two lists equal, irrespective of order?				*)
(*----------------------------------------------------------------------*)
fun unordered eq (xs,ys) =
let
  fun remove x [] = NONE
    | remove x (y::ys) = 
      if eq(x,y) then SOME ys
      else 
        case remove x ys of
          NONE => NONE
        | SOME ys' => SOME (y::ys')

  fun check ([], []) = true
    | check ([], _) = false
    | check (x::xs, ys) = 
      case remove x ys of
        NONE => false
      | SOME ys' => check (xs, ys')
in
  check (xs,ys)
end

end
