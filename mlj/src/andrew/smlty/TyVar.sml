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
(* Sorted/overloaded type variables					*)
(*======================================================================*)
structure TyVar :> TYVAR = 
struct

datatype Sort =
  Normal of TySort.Sort
| Overloaded of TyName.Set.set

(*----------------------------------------------------------------------*)
(* Type variables are tagged with their sorts and a Java attribute      *)
(* (Section 4.1, p16 Defn)						*)
(*----------------------------------------------------------------------*)
datatype TyVarName = Explicit of Syntax.symbol | Implicit of int
type TyVar = TyVarName * Sort

structure TyVarOrd = 
  struct
    type ord_key = TyVar
    fun compare ((Explicit s1,_),(Explicit s2,_)) =Symbol.OrdKey.compare(s1,s2)
      | compare ((Implicit s1,_),(Implicit s2,_)) = Int.compare(s1,s2)
      | compare ((Implicit _,_),(Explicit _,_)) = LESS
      | compare ((Explicit _,_),(Implicit _,_)) = GREATER
  end

structure Set = SetFn(TyVarOrd)
structure Map = MapFn(TyVarOrd)

(*----------------------------------------------------------------------*)
(* Given the syntax for an explicit type variable, construct an 	*)
(* representation with the appropriate sort (Eq or Any)			*)
(*----------------------------------------------------------------------*)
fun explicit v = 
  (Explicit v, 
  Normal  
  { eq = String.sub(JavaString.toMLString (Symbol.toJavaString v), 0) = #"'",
    class = false,
    constrained = false })

(*----------------------------------------------------------------------*)
(* Turn a type variable into a string, adding ' or '' for sorts Any and *)
(* Eq, and putting the sort in parentheses otherwise			*)
(*----------------------------------------------------------------------*)
fun toString (Explicit v,sort) = 
    "'" ^ JavaString.toMLString (Symbol.toJavaString v)

  | toString (Implicit x,sort) = 
    (case sort of
      Normal { constrained = true, ... } => "'$"
    | Normal { eq = true, ... } => "''"
    | _ => "'") ^ Pretty.indexToString (x+26)

fun isExplicit (Explicit _,_) = true
  | isExplicit _ = false

(*----------------------------------------------------------------------*)
(* Return the sort of a type variable					*)
(*----------------------------------------------------------------------*)
fun sort (v,s) = s

(*----------------------------------------------------------------------*)
(* Equality on type variables						*)
(*----------------------------------------------------------------------*)
fun eq (tyvar1, tyvar2) = TyVarOrd.compare(tyvar1, tyvar2) = EQUAL

type Supply = int

(*----------------------------------------------------------------------*)
(* Generate a new type variable with the specified sort			*)
(*----------------------------------------------------------------------*)
fun fresh sort supply = 
  ((Implicit supply, sort), supply+1)

val initial = 0


end
