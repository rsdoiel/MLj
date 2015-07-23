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

structure SimplifyOps =
struct

local 
  open MILTerm 
in

(*----------------------------------------------------------------------*)
(* What is an atom (= Java canonical form)?				*)
(*----------------------------------------------------------------------*)
fun isAtom (kindenv : MILTy.Kind Var.Map.map) v =
case v of
  (* Variables are just Java locals/stack entries *)
  Var _ => 
  true

  (* Constants correspond to Java constants *)
| SCon _ => 
  true

  (* No-op coercions use no instructions at all *)
| Coerce(v, _) => 
  isAtom kindenv v

  (* Null value of unit type *)
| Tuple [] =>
  true

  (* Null value of any type *)
| Null ty =>
  true

  (* Mu intros/elims are there just for type checking purposes *)
| Fold(v, _) => 
  isAtom kindenv v

| Unfold v => 
  isAtom kindenv v

  (* Single-depth NONE : 'a option is represented by Java NULL *)
| Inj(ty, _, []) =>
  (case MILTy.fromSum ty of
    SOME ([[], [ty]] | [[ty], []]) => MILTy.noneIsNull kindenv ty
  | SOME tyss => List.all List.null tyss
  | NONE => false)

  (* ? *)
| Inj(ty, _, [v]) =>
  (case MILTy.fromSum ty of
    SOME ([[], [ty]] | [[ty], []]) => 
    MILTy.someIsNop kindenv ty andalso isAtom kindenv v

  | _ => false)

| TApp(v,_) => isAtom kindenv v

| TAbs(tyvars,v) => isAtom (Var.extend (kindenv, tyvars)) v

| _ => false

(*----------------------------------------------------------------------*)
(* Crude syntactic check for purity.					*)
(*----------------------------------------------------------------------*)
fun isPure (Triv _) = true
  | isPure (LetFun(_,_,_,e)) = isPure e
  | isPure (LetVal(_,_,e)) = isPure e
  | isPure _ = false

end (* of local open *)
        
end (* of struct *)
