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
(* Exhaustively apply the following flattening operations to types:     *)
(* 									*)
(* unitArg, unitResult, unitCon, unitProd:				*)
(*     <ty_1, ..., unit, ..., ty_n>                                     *)
(* --> <ty_1, ..., ..., ty_n>                                           *)
(* 									*)
(* flattenCon:                                                          *)
(*     tys_1 + ... + <(ty_1,...,ty_m)> + ... + tys_n                    *)
(* --> tys_1 + ... + <ty_1,...,ty_m> + ... + tys_n                      *)
(* [unless the sum is of the form <> + <ty> or <ty> + <>                *)
(*======================================================================*)
structure FlattenTypes :> FLATTENTYPES =
struct

local
  open MILTy Counters
in

(*----------------------------------------------------------------------*)
(* True if this is the unit type.					*)
(*----------------------------------------------------------------------*)
fun isUnit (kindenv, dkindenv : MILTy.Kind list) ty = 
  case MILTy.fromProd ty of
    SOME [] => true
  | _ => 
    case MILTy.fromTyvar ty of
      SOME x => 
      (case Var.Map.find(kindenv, x) of
        SOME (MILTy.Bound ty) => isUnit (kindenv, dkindenv) ty
      | _ => false)

    | NONE =>
      case MILTy.fromDeb ty of
        NONE => false
      | SOME i =>
        (case List.nth(dkindenv, i) of
          MILTy.Bound ty => isUnit (kindenv, dkindenv) ty
        | _ => false)

fun whichProj kindenv (i,tys) =
let
  fun loop ([], i', j) =
      Debug.fail "FlattenTypes.whichProj: out of bounds"

    | loop (ty::tys, i', j) = 
      if i=i' then j else 
      if isUnit (kindenv,[]) ty then loop (tys, i'+1, j)
      else loop (tys, i'+1, j+1)
in
  loop (tys, 0, 0)
end

(*----------------------------------------------------------------------*)
(* Apply the unit removal transform described above.       		*)
(*----------------------------------------------------------------------*)
fun transTypes flattenUnit env tys =
let
  fun loop ([], changed, res) = 
      if changed then SOME (rev res) else NONE

    | loop (ty::tys, changed, res) =
      if flattenUnit andalso isUnit env ty
      then loop (tys, true, res)
      else 
        case transType env ty of
          NONE => loop (tys, changed, ty::res)
        | SOME ty => loop (tys, true, ty::res)
in
  loop (tys, false, [])
end

and transType (env as (kindenv,dkindenv)) ty = 
MILTy.deconstruct ty 
{
  tyvar = fn x => if isUnit env ty then SOME MILTys.unit else NONE,
  deb = fn i => if isUnit env ty then SOME MILTys.unit else NONE,
  forall = fn (kinds, ty) => 
    let
      val tyopt = transType (kindenv, kinds @ dkindenv) ty
      val kindsopt = transKinds env kinds
    in
      case (kindsopt, tyopt) of
        (NONE, NONE) => NONE
      | (SOME kinds, NONE) => SOME (MILTy.debforall(kinds, ty))
      | (NONE, SOME ty) => SOME (MILTy.debforall(kinds, ty))
      | (SOME kinds, SOME ty) => SOME (MILTy.debforall(kinds, ty))
    end,

  refty = fn ty => 
    (case transType env ty of
      NONE => NONE
    | SOME ty => SOME (MILTy.refty ty)),

  array = fn ty => 
    (case transType env ty of
      NONE => NONE
    | SOME ty => SOME (MILTy.array ty)),

  vector = fn ty => 
    (case transType env ty of
      NONE => NONE
    | SOME ty => SOME (MILTy.vector ty)),

  prod = fn tys => 
    (case transTypes (justEnabled unitProd) env tys of
      NONE => NONE
    | SOME tys => SOME (MILTy.prod tys)),

  closure = fn ty => NONE,
  java = fn _ => NONE,
  tyname = fn _ => NONE,
  exn = fn _ => NONE,

  con = fn tys => 
    case transTypes (justEnabled unitCon) env tys of
      SOME tys =>
      (case (map MILTy.fromProd tys, justEnabled flattenCon) of
        ([SOME tys], true) => SOME (MILTy.con tys)
      | _ => SOME (MILTy.con tys))

    | NONE =>
      (case (map MILTy.fromProd tys, justEnabled flattenCon) of
        ([SOME tys], true) => SOME (MILTy.con tys)
      | _ => NONE),
    

  arrow = fn (tys, cty) => 
    let val tysopt = transTypes (justEnabled unitArg) env tys
        val (effect, restys) = MILTy.fromCmp cty
        val restysopt = transTypes (justEnabled unitResult) env restys
    in
      case (tysopt, restysopt) of
        (NONE, NONE) => 
        NONE

      | (SOME tys, NONE) => 
        SOME (MILTy.arrow(tys, cty))

      | (NONE, SOME restys) => 
        SOME (MILTy.arrow(tys, MILTy.cmp(effect, restys)))

      | (SOME tys, SOME restys) => 
        SOME (MILTy.arrow(tys, MILTy.cmp(effect, restys)))
    end,

  mu = fn (i, defs) => 
    let val env = (kindenv, map (fn _ => MILTy.Any) defs @ dkindenv)
    in
      case transTypes false env defs of
        NONE => NONE
      | SOME defs => SOME (MILTy.mu(i, defs))
    end,

  sum = fn tyss =>
    transSum env tyss
}

and transSum env tyss =
let
  fun flattenCons ([], changed, res) =
      if changed then SOME (MILTy.sum res) else NONE

    | flattenCons ([ty]::tyss, changed, res) =
      (case (MILTy.fromProd ty, justEnabled flattenCon) of
        (SOME tys, true) => flattenCons (tyss, true, tys::res)
      | _ => flattenCons(tyss, changed, [ty]::res))

    | flattenCons (tys::tyss, changed, res) =
      flattenCons (tyss, changed, tys::res)

  fun removeUnits ([], changed, res) =
      (case res of
        ([[], [_]] | [[_], []]) =>
        if changed then SOME (MILTy.sum (rev res)) else NONE

      | _ => flattenCons (res, changed, []))
  
    | removeUnits (tys::tyss, changed, res) =
      (case transTypes (justEnabled unitCon) env tys of
        NONE => removeUnits (tyss, changed, tys::res)
      | SOME tys => removeUnits (tyss, true, tys::res))       
in
  removeUnits (tyss, false, [])
end

and transKinds env kinds =
let
  fun loop ([], changed, res) = 
      if changed then SOME (rev res) else NONE

    | loop ((kind as (MILTy.Bound ty))::kinds, changed, res) =
      (case transType env ty of
        NONE => loop (kinds, changed, kind::res)
      | SOME ty => loop (kinds, true, MILTy.Bound ty :: res))

    | loop (kind::kinds, changed, res) =
      loop (kinds, changed, kind::res)
in
  loop (kinds, false, [])
end

val isUnit = fn kindenv => isUnit (kindenv, []) 

val transKind = fn kindenv => fn kind =>
  case kind of
    MILTy.Bound ty =>
    (case transType (kindenv,[]) ty of
      NONE => kind
    | SOME ty => MILTy.Bound ty)
  | _ => kind

val transType = fn kindenv => fn ty => 
  case transType (kindenv,[]) ty of
    NONE => ty
  | SOME ty => ty

val transCmpType = fn kindenv => fn cty =>
  let
    val (effect, tys) = MILTy.fromCmp cty
  in
    case transTypes (justEnabled unitResult) (kindenv,[]) tys of
      NONE => cty
    | SOME tys => MILTy.cmp(effect, tys)
  end

end (* of local open *)

end (* of struct *)
