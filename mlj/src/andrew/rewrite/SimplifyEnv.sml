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
(* The environment used in simplification consists of:     		*)
(* (1) a map from value variables to:                                   *)
(*     (a) their value type;                                            *)
(*     (b) their (optional) binding, which is                           *)
(*         an atomic value term                                         *)
(*      or a non-atomic value term                                      *)
(*      or a non-recursive function binding                             *)
(* (2) a map from type variables to kinds;                              *)
(* (3) a map from type variables to types, used for type application;   *)
(* (4) a map from values to value variables, used for cse.              *)
(*======================================================================*)
structure SimplifyEnv :> SIMPLIFYENV =
struct

local open MILTerm
in

datatype ValBind =
  None
| Atom of MILTerm.Val
| NonAtom of MILTerm.Val
| Fun of (Var.Var * MILTy.Kind) list * MILTerm.FunKind * MILTerm.TAbstr
| LocalFun
| Cmp of MILTerm.Cmp

type Env = 
{ 
  valvars : (MILTy.Type * ValBind) Var.Map.map,
  tyvars  : MILTy.Kind Var.Map.map,
  common  : Var.Var CommonVal.Map.map,
  tysubst : MILTy.Type Var.Map.map
}

fun emptyEnv tyenv = 
{ 
  valvars = Var.Map.foldli (fn (x, ty, valvars) =>
    Var.Map.insert(valvars, x, (ty, None))) Var.Map.empty tyenv,
  tyvars = Var.Map.empty, 
  common = CommonVal.Map.empty,
  tysubst = Var.Map.empty
}

fun envPlusValVar ({valvars, tyvars, common, tysubst} : Env, x, v, ty) =
let
  val atom = SimplifyOps.isAtom tyvars v
  val b = if atom then Atom v else NonAtom v
in
  { 
    tyvars = tyvars, tysubst = tysubst,
    common = if not atom andalso CommonVal.isMappable v 
             andalso Counters.justEnabled Counters.commonVal
             then CommonVal.Map.insert(common, v, x) else common,
    valvars = Var.Map.insert(valvars, x, (ty, b))
  }
end

fun envPlusValVars (env : Env, [], [], []) = env
  | envPlusValVars (env, x::xs, v::vs, ty::tys) =
    envPlusValVars (envPlusValVar (env, x, v, ty), xs, vs, tys)
  | envPlusValVars _ = 
    Debug.fail "SimplifyEnv.envPlusValVars: mismatched lists"

fun envPlusTyVars ({valvars, tyvars, common, tysubst} : Env, tyvars') =
{ 
  valvars = valvars, tysubst = tysubst,
  common = common,
  tyvars = 
    foldl (fn ((x,kind),tyvars) => Var.Map.insert(tyvars, x, kind))
    tyvars tyvars'
}

fun envPlusTySubst ({valvars, tyvars, common, tysubst} : Env, tyvars', tys) =
{ 
  valvars = valvars, 
  common = common,
  tyvars = 
    foldl (fn ((x,kind),tyvars) => Var.Map.insert(tyvars, x, kind))
    tyvars tyvars',
  tysubst = 
    ListPair.foldl 
    (fn ((x,kind),ty,tysubst) => Var.Map.insert(tysubst, x, ty))
    tysubst (tyvars', tys)
}

fun envPlusVars ({valvars, tyvars, common, tysubst} : Env, vars, tys) =
{ 
  valvars = 
    ListPair.foldl (fn (x, ty, valvars) => Var.Map.insert(valvars, x, 
      (ty, None))) valvars (vars, tys),
  common = common,
  tyvars = tyvars, 
  tysubst = tysubst
}

fun envPlusTypedVars ({valvars, tyvars, common, tysubst} : Env, typedvars) =
{ 
  valvars = 
    foldl (fn ((x, ty), valvars) => Var.Map.insert(valvars, x, 
      (ty, None))) valvars typedvars,
  common = common,
  tyvars = tyvars, 
  tysubst = tysubst
}

fun envPlusValCmp ({valvars, tyvars, common, tysubst} : Env, typedvars, e) =
{ 
  valvars = 
  case (typedvars, e) of
    ([(x,ty)], Cond _) =>
    Var.Map.insert(valvars, x, (ty, Cmp e))

  | ([(x,ty)], Java((Java.FieldRef, _, _), _, _)) =>
    Var.Map.insert(valvars, x, (ty, Cmp e))

  | _ => 
    foldl (fn ((x, ty), valvars) => Var.Map.insert(valvars, x, 
      (ty, None))) valvars typedvars,

  common = common,
  tyvars = tyvars,
  tysubst = tysubst
}

fun envPlusFunBind ({valvars, tyvars, common, tysubst} : Env, x, a, ty) =
{ 
  valvars = Var.Map.insert(valvars, x, (ty, Fun a)),
  common = common,
  tyvars = tyvars,
  tysubst = tysubst
}

fun envPlusLocalFun ({valvars, common, tyvars, tysubst} : Env, x, ty) =
{ 
  valvars = Var.Map.insert(valvars, x, (ty, LocalFun)),
  common = common,
  tyvars = tyvars,
  tysubst = tysubst
}

fun lookupVarType ({valvars, ...} : Env, x) =
  case Var.Map.find(valvars, x) of
    NONE =>
    Debug.fail ("SimplifyEnv.lookupVarType: not found: " ^ Var.toString x) 

  | SOME (ty,_) => ty

fun lookupVarVal ({valvars, ...} : Env, x) =
  case Var.Map.find(valvars, x) of
    NONE =>
    Debug.fail ("SimplifyEnv.lookupVarVal: not found: " ^ Var.toString x) 

  | SOME (ty,Atom a) => (ty, SOME a)
  | SOME (ty,_) => (ty, NONE)

fun isLocalFun ({valvars, ...} : Env, x) =
  case Var.Map.find(valvars, x) of
    NONE =>
    Debug.fail ("SimplifyEnv.isLocalFun: not found: " ^ Var.toString x) 

  | SOME (ty,LocalFun) => SOME ty
  | SOME _ => NONE

fun isLocalFun' ({valvars, ...} : Env, x) =
  case Var.Map.find(valvars, x) of
    NONE =>
    Debug.fail ("SimplifyEnv.isLocalFun': not found: " ^ Var.toString x) 

  | SOME (ty,LocalFun) => true
  | SOME (ty,Fun(_,MILTerm.LocalFun,_)) => true
  | SOME _ => false

fun lookupBinding ({valvars, ...} : Env, v as MILTerm.Var x) =
    (case Var.Map.find(valvars, x) of
      SOME (_, NonAtom v) => v
    | _ => v)

  | lookupBinding (_, v) = v

fun lookupCmpBind ({valvars, ...} : Env, v as MILTerm.Var x) =
    (case Var.Map.find(valvars, x) of
      SOME (_, Cmp e) => SOME (x, e)
    | _ => NONE)

  | lookupCmpBind (_, v) = NONE

fun lookupFunBind (env as {valvars, ...} : Env, v) =
  case v of
    Var x => 
    (case Var.Map.find(valvars, x) of
      SOME (_, Fun(_, kind, tabs)) => SOME (x, Var.Map.empty, kind, tabs)
    | _ => NONE)

  | TApp(Var x, tys) =>
    (case Var.Map.find(valvars, x) of
      SOME (_, Fun (tyvars : (Var.Var * MILTy.Kind) list, kind, tabs)) => 
      SOME (x, ListPair.foldl 
        (fn ((x,kind),ty,tysubst) => Var.Map.insert(tysubst, x, ty))
        Var.Map.empty (tyvars, tys), kind, tabs)
    | _ => NONE)

  | _ => NONE

fun lookupCommon ({common, ...} : Env, v) =
    if CommonVal.isMappable v
    then CommonVal.Map.find(common, v)
    else NONE

fun simplifyType ({tysubst, ...} : Env) ty =
    if Var.Map.numItems tysubst = 0 then ty
    else MILTy.subst tysubst ty

end (* of local open *)

end (* of struct *)
