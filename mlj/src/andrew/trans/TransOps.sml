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
(* The state/environment monad used for translation to MIL		*)
(*======================================================================*)
structure TransOps =
struct

(*----------------------------------------------------------------------*)
(* A value environment maps an SML source symbol to:			*)
(*   (1) a MIL term variable          					*)
(*   (2) a MIL type							*)
(*   (3) a list of MIL types to which the variable should be applied    *)
(*----------------------------------------------------------------------*)
type ValEnv = 
  (Var.Var * MILTy.Type * MILTy.Type list) Symbol.OrdMap.map

(*----------------------------------------------------------------------*)
(* An exception environment maps an SML exception to:			*)
(*   (1) an optional `name' variable (for generative exceptions)  	*)
(*   (2) a type that is either a class (Java exn) or a MIL exn type.    *)
(*----------------------------------------------------------------------*)
type ExEnv =
  (Var.Var option * MILTy.Type) SMLTy.ExMap.map

val errors = ref ([] : Error.Error list)
val vs = ref Var.initial
val tvs = ref Var.initial

fun addError err = errors := err :: !errors

fun freshVar () = 
  let val (vs', v) = Var.fresh (!vs)
  in
    vs := vs'; v
  end

fun freshTyVar () =
  let
    val (tvs', v) = Var.fresh (!tvs)  
  in
    tvs := tvs'; v
  end

fun freshVars (VE, []) = (VE, [])
  | freshVars (VE, (v,ty)::vs) = 
    let
      val v' = freshVar ()
      val (VE', vs') = freshVars (VE, vs)
    in
      (Symbol.OrdMap.insert(VE', v, (v',ty,[])), v'::vs')
    end

fun freshOptVars (VE, []) = (VE, [])
  | freshOptVars (VE, SOME (v,ty)::vs) = 
    let
      val v' = freshVar ()
      val (VE',vs') = freshOptVars (VE, vs)
    in
      (Symbol.OrdMap.insert(VE', v, (v',ty,[])), v'::vs')
    end

  | freshOptVars (VE, NONE::vs) =
    let
      val v' = freshVar ()
      val (VE',vs') = freshOptVars (VE, vs)
    in
      (VE', v'::vs')
    end

(*----------------------------------------------------------------------*)
(* Translate an ML type variable into a MIL kind			*)
(* i.e. sort Any becomes kind Any					*)
(*      sort Eq  becomes kind Eq					*)
(*      all others should not be present				*)
(*----------------------------------------------------------------------*)
fun tyVarKind tyvar = 
  case TyVar.sort tyvar of
    TyVar.Normal { eq = true, ... } => MILTy.Eq
  | _ => MILTy.Any

fun freshTyVars (VE, []) = (VE, [])
  | freshTyVars (VE, tyvar::tyvars) = 
    case TyVar.sort tyvar of
      TyVar.Overloaded _ => freshTyVars (VE, tyvars)
    | _ =>
      let
        val v' = freshTyVar ()
        val (VE', vs') = freshTyVars (VE, tyvars)
      in
        (TyVar.Map.insert(VE', tyvar, MILTy.tyvar v'), 
          (v',tyVarKind tyvar)::vs')
      end

(*----------------------------------------------------------------------*)
(* Translate type variables and extend the TV environment appropriately	*)
(*----------------------------------------------------------------------*)
fun freshDebTyVars depth tyvars =
let 
  val (TVE, kinds) =
  Gen.foldri 
    (fn (d, tyvar, (TVE, kinds)) =>
      let 
        val K = tyVarKind tyvar
      in
        (TyVar.Map.insert(TVE, tyvar, MILTy.deb (depth + d)), K::kinds)
      end) 
    (TyVar.Map.empty, [])
  tyvars
in
  (TVE, kinds)
end



end


