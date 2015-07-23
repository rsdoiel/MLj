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
(* Various useful operations on MIL terms				*)
(*======================================================================*)
signature MILTERMOPS =
sig

(*----------------------------------------------------------------------*)
(* Construct a type abstraction term TAbs(tyvars,ve) unless tyvars is	*)
(* empty, in which case return ve.                                      *)
(*----------------------------------------------------------------------*)
val tabs : (Var.Var * MILTy.Kind) list * MILTerm.Val -> MILTerm.Val

(*----------------------------------------------------------------------*)
(* Construct a type application term TApp(ve,tys) unless tys is    	*)
(* empty, in which case return ve.                                      *)
(*----------------------------------------------------------------------*)
val tapp : MILTerm.Val * MILTy.Type list -> MILTerm.Val

(*----------------------------------------------------------------------*)
(* MIL terms of type MILTy.bool						*)
(*----------------------------------------------------------------------*)
val trueVal : MILTerm.Val
val falseVal : MILTerm.Val

(*----------------------------------------------------------------------*)
(* Does a particular type variable occur in a value/computation term?	*)
(*----------------------------------------------------------------------*)
val tyVarOccursVal : Var.Var -> MILTerm.Val -> bool
val tyVarOccursCmp : Var.Var -> MILTerm.Cmp -> bool

val substVal       : MILTy.Type Var.Map.map -> MILTerm.Val -> MILTerm.Val
val substCmp       : MILTy.Type Var.Map.map -> MILTerm.Cmp -> MILTerm.Cmp

(*----------------------------------------------------------------------*)
(* Does the size of a value or computation term exceed a value?		*)
(*----------------------------------------------------------------------*)
val valBigger : MILTerm.Val*int -> bool
val cmpBigger : MILTerm.Cmp*int -> bool
val absSize   : MILTerm.Cmp -> int

(*----------------------------------------------------------------------*)
(* Are two values (semantically) equal?					*)
(*   SOME true    => yes                                                *)
(*   SOME false   => no                                                 *)
(*   NONE         => don't know                                         *)
(*----------------------------------------------------------------------*)
val valEq     : MILTerm.Val * MILTerm.Val -> bool option

(*----------------------------------------------------------------------*)
(* The input is the body of a (possibly recursive) function that will be*)
(* `localised' but which requires its `return' to be altered to a       *)
(* continuation function (contfunvar) application. Hence its return     *)
(* type is also changed. So, replace:                                   *)
(*   val vs    by f(vs);                                                *)
(* [v not local function variable]                                      *)
(*   v vs      by let xs <= v vs in f(xs);                              *)
(*             likewise for alloc v, !v, v1:=v2, new, java ops, throw   *)
(* Any local blocks used inside e must also be declared inside e.       *)
(*----------------------------------------------------------------------*)
val addContinuation : 
  Var.Set.set * Var.Var * MILTy.CmpType -> MILTerm.Cmp -> MILTerm.Cmp

val isLocal : MILTerm.FunKind -> bool

end