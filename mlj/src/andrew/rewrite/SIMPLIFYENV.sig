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

signature SIMPLIFYENV =
sig
  type Env

  val emptyEnv         : MILTy.Type Var.Map.map -> Env
  val envPlusFunBind   : 
    Env * Var.Var * 
    ((Var.Var*MILTy.Kind) list * MILTerm.FunKind * MILTerm.TAbstr) * MILTy.Type
    -> Env

  val envPlusLocalFun  : Env * Var.Var * MILTy.Type -> Env
  val envPlusTyVars    : Env * (Var.Var*MILTy.Kind) list -> Env
  val envPlusTySubst   : Env * (Var.Var*MILTy.Kind) list*MILTy.Type list -> Env
  val envPlusTypedVars : Env * MILTerm.TypedVar list -> Env

  val envPlusValVar    : 
    Env * Var.Var * MILTerm.Val * MILTy.Type -> 
    Env

  val envPlusValCmp   : 
    Env * MILTerm.TypedVar list * MILTerm.Cmp ->
    Env 

  val envPlusValVars   : 
    Env * Var.Var list * MILTerm.Val list * MILTy.Type list ->
    Env

  val envPlusVars      : Env * Var.Var list * MILTy.Type list -> Env

  val lookupVarType    : Env * Var.Var -> MILTy.Type 
  val lookupVarVal     : Env * Var.Var -> MILTy.Type * MILTerm.Val option 
  val isLocalFun       : Env * Var.Var -> MILTy.Type option
  val isLocalFun'      : Env * Var.Var -> bool
  val lookupBinding    : Env * MILTerm.Val -> MILTerm.Val
  val lookupCommon     : Env * MILTerm.Val -> Var.Var option
  val lookupCmpBind    : Env * MILTerm.Val -> (Var.Var * MILTerm.Cmp) option
  val lookupFunBind    : Env * MILTerm.Val -> 
    (Var.Var*MILTy.Type Var.Map.map * MILTerm.FunKind * MILTerm.TAbstr) option

  val simplifyType     : Env -> MILTy.Type -> MILTy.Type

end
