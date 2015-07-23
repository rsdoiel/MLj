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
(* Typed simplification of (possibly non-atomic) MIL value terms.	*)
(*======================================================================*)
structure SimplifyVal :> SIMPLIFYVAL =
struct

local 
  open MILTerm Counters MILPretty SimplifyEnv
in

(*----------------------------------------------------------------------*)
(* Recursively apply simplify to the subterms of v, then apply          *)
(* rewrites to the resulting term. For variables, apply the             *)
(* substitution from env. Likewise for types.                           *)
(*----------------------------------------------------------------------*)
fun simplify (env : SimplifyEnv.Env) (v : MILTerm.Val) = 
let
  fun common (v,ty) =
    case SimplifyEnv.lookupCommon (env, v) of
      SOME x =>
      if enabled commonVal
      then
        (Census.addVal(v, ~1); Census.addVar(x, 1);
        (Var x, SimplifyEnv.lookupVarType (env, x)))
      else (v,ty)

    | NONE => 
      (v,ty)
in
  case v of

(*......................................................................*)
(* Sum introduction							*)
(*......................................................................*)
  Inj(ty, i, vs) =>
  common (Inj(ty, i, map (#1 o SimplifyAtom.simplify env) vs), ty)

(*......................................................................*)
(* Exception introduction						*)
(*......................................................................*)
| ExCon(exname, vs) =>
  common (ExCon(exname, map (#1 o SimplifyAtom.simplify env) vs),MILTys.topExn)

| Fold(v, ty) =>
  common (Fold(#1 (simplify env v), ty), ty)

(*......................................................................*)
(* Quantifier introduction						*)
(*                                                                      *)
(* forall-eta:								*)
(*    Fn (t_1,...,t_n) => v {t_1,...,t_n}                               *)
(*       -->   v    (if t_1,...,t_n) not free in v)                     *)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val (v, ty) = simplify (SimplifyEnv.envPlusTyVars (env, tyvars)) v
    fun default () = common (TAbs(tyvars, v), MILTy.forall(tyvars, ty))
    fun applyTAbsCC (tyvars, v) =
    case v of
      TApp(polyv, tys) =>
      let
      fun checkArgs [] [] = 
          if enabled forallEta 
          then (polyv, MILTy.forall(tyvars, ty))
          else default ()

        | checkArgs ((tyvar,k)::tyvars) (ty :: tys) = 
          (case MILTy.fromTyvar ty of
            SOME tyvar' =>
            if Var.eq(tyvar,tyvar') andalso 
              not (MILTermOps.tyVarOccursVal tyvar polyv)
            then checkArgs tyvars tys
            else default ()
          | _ => default ())

        | checkArgs _ _ = 
          default ()
      in
        checkArgs tyvars tys
      end

    | _ => default ()
  in
    applyTAbsCC (tyvars, v)
  end

(*......................................................................*)
(* Product introduction							*)
(*                                                                      *)
(* prod-eta:								*)
(*    (#1 v, ..., #n v) --> v     					*)
(*......................................................................*)
| Tuple vs =>
  let 
    val (vs, tys) = SimplifyAtom.simplifyVec env vs
    fun default () = common (Tuple vs, MILTy.prod tys)
  in
    case vs of
      v as (Var firstx) :: _ =>
      let
        fun good (prodx,v,i) =
           case SimplifyEnv.lookupCommon (env, Proj(i,v)) of
              SOME prodx' => Var.eq(prodx,prodx')
            | _ => false

        fun test (prodx, [], i) = 
            if enabled prodEta
            then 
              (Census.addVar (prodx, 1);
               app (fn v => Census.addVal(v, ~1));
               (Var prodx, MILTy.prod tys))
            else default ()

          | test (prodx, v::vs, i) =
            if good (prodx, v, i) then test (prodx, vs, i+1)
            else default ()

      in
        case SimplifyEnv.lookupCommon (env, Proj(0, Var firstx)) of
          SOME prodx =>
          let
            val ty = SimplifyEnv.lookupVarType (env, prodx)
          in
            case MILTy.fromProd ty of
              NONE => default ()
            | SOME tys' =>
              if length tys = length tys' 
              then test (prodx, vs, 0)
              else default ()
          end

        | _ => default ()
      end

    | _ => 
      default ()
  end
    
(*......................................................................*)
(* Product elimination							*)
(*                                                                      *)
(* prod-beta:							        *)
(*     #i (v_1, ..., v_i, ..., v_n) --> v_i				*)
(*......................................................................*)
| projv as Proj(i, v) =>
  let
    val (v, ty) = SimplifyAtom.simplify env v
    val tys = 
      case MILTy.fromProdCon ty of
        SOME tys => tys
      | NONE => 
        failVal v "SimplifyVal.simplify: expected product/constructor type"

    val resultty = (List.nth(tys, i)) handle Subscript =>
      failVal projv "SimplifyVal.simplify: out of range projection"

    fun default () = common (Proj(i, v), resultty)
  in
    case SimplifyEnv.lookupBinding (env, v) of
      Tuple vs => 
      let 
        val v' = List.nth(vs, i)
      in
        if (case v' of Null _ => false | _ => true) andalso enabled prodBeta
        then
        (
          Census.addVal (v, ~1); Census.addVal(v', 1);
          (v', resultty)
        )
        else default ()
      end

    | _ => default ()
  end

| _ =>
  SimplifyAtom.simplify env v

end

and simplifyVec env [] = ([], [])
  | simplifyVec env (v::vs) =
    let
      val (v,ty) = simplify env v
      val (vs,tys) = simplifyVec env vs
    in
      (v::vs, ty::tys)
    end

end (* of local open *)
        
end (* of struct *)
