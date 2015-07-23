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
(* Sorts for type names, used for equality and Java classes/arrays.	*)
(*======================================================================*)
structure TySort :> TYSORT = 
struct

type Sort = { eq : bool, class : bool, constrained : bool }

fun toString { eq, class, constrained } = 
  case (eq,class) of
    (false,false) => "any"
  | (false,true) => "class"
  | (true,false) => "eq"
  | (true,true) => "eqclass"

fun { eq = eq1, class = class1, constrained = c1 } <= 
    { eq = eq2, class = class2, constrained = c2 } =
  (eq1 orelse not eq2) andalso 
  (class1 orelse not class2) andalso
  (c1 orelse not c2)

fun glb ({ eq = eq1, class = class1, constrained = c1 }, 
         { eq = eq2, class = class2, constrained = c2 }) =
   { eq = eq1 orelse eq2, 
     class = class1 orelse class2,
     constrained = c1 orelse c2 }

fun lub ({ eq = eq1, class = class1, constrained = c1 }, 
         { eq = eq2, class = class2, constrained = c2 }) =
   { eq = eq1 andalso eq2, 
     class = class1 andalso class2,
     constrained = c1 andalso c2 }

val eqSort = { eq = true, class = false, constrained = false }
val classSort = { eq = false, class = true, constrained = false }
val constrainedSort = { eq = false, class = false, constrained = false }
val anySort = { eq = false, class = false, constrained = false }

end