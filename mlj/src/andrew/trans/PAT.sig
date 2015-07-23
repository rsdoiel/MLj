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
(* Pattern matching compiler.                 				*)
(*======================================================================*)
signature PAT =
sig

(*----------------------------------------------------------------------*)
(* Translate a typed fn construct.					*)
(*----------------------------------------------------------------------*)
val transFn : 
{
  entity : Entity.Ref,
  transExp : TransOps.ValEnv -> SMLTerm.Exp -> MILTerm.Cmp * MILTy.CmpType,
  transType : SMLTy.Type -> MILTy.Type,
  VE : TransOps.ValEnv,
  EE : TransOps.ExEnv,
  smlty : SMLTy.Type,
  match : SMLTerm.Match
}
-> MILTerm.TAbstr * MILTy.CmpType

(*----------------------------------------------------------------------*)
(* Translate a (generalising) pattern-matching let construct.           *)
(*----------------------------------------------------------------------*)
val transLetPat : 
{
  transExp : TransOps.ValEnv -> SMLTerm.Exp -> MILTerm.Cmp * MILTy.CmpType,
  transType : SMLTy.Type -> MILTy.Type, 
  VE : TransOps.ValEnv,
  EE : TransOps.ExEnv,
  var : Var.Var,
  smlty : SMLTy.Type,
  pat : SMLTerm.Pat,
  fail : MILTerm.Cmp * MILTy.CmpType,
  loc : Syntax.Location
}
-> MILTerm.Cmp * MILTy.CmpType

(*----------------------------------------------------------------------*)
(* Translate an exception handling construct.				*)
(*----------------------------------------------------------------------*)
val transHandle : 
{
  entity : Entity.Ref,
  transExp : TransOps.ValEnv -> SMLTerm.Exp -> MILTerm.Cmp * MILTy.CmpType,
  transType : SMLTy.Type -> MILTy.Type, 
  VE : TransOps.ValEnv,
  EE : TransOps.ExEnv,
  exp : SMLTerm.Exp,
  match : SMLTerm.Match
}
-> MILTerm.Cmp * MILTy.CmpType

end 
