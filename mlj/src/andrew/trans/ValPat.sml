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
(* Translate a valuable pattern into a series of projections and        *)
(* unfoldings.                                                          *)
(*======================================================================*)
structure ValPat :> VALPAT =
struct

local 
  open SMLTerm SMLTy TransOps 
in

fun trans 
  { TVE : MILTy.Type TyVar.Map.map, 
    TNE : MILTy.Type TyName.Map.map,
    tyvars : TyVar.TyVar list,
    pat : SMLTerm.Pat, 
    var = x : Var.Var, 
    smlty : SMLTy.Type 
  } =
let
  val (TVE', miltyvars) = freshTyVars (TVE, tyvars)
  fun transType smlty = 
    MILTy.forall(miltyvars, TransType.transType TVE' TNE smlty)

  fun trans' (pat, x, smlty) =
  let
    fun transRec patrow (binds, VE) =
    case patrow of
      [] => 
      (binds, VE)

    | (lab, pat)::patrow =>
      let 
        val (fldty, i, n) = SMLTy.fieldType (smlty, lab)
        val x' = freshVar ()
        val (binds', VE') = trans' (pat, x', fldty)
      in
        transRec patrow (binds @ (x', 
          MILTermOps.tabs(miltyvars, 
            MILTerm.Proj(i, 
            MILTerm.TApp(MILTerm.Var x, map (MILTy.tyvar o #1) miltyvars)))) 
            :: binds',
          Symbol.OrdMap.unionWith #2 (VE, VE'))
      end
  in
    case pat of
      PatWild => 
      ([], Symbol.OrdMap.empty)

    | PatVar(v,smlty) =>
      ([], Symbol.OrdMap.insert 
        (Symbol.OrdMap.empty, v, (x, transType smlty, [])))

    | PatCon(con, defs as (isrec, (tvs, tyname, CE)), tys, patopt) =>
      let 
        val (i, tyopt) = SMLTy.findCon(tyname, CE, con)
        val tyopt = Option.map (SMLTy.appSubst (ListPair.zip(tvs, tys))) tyopt
      in
        case (patopt, tyopt) of
          (NONE, NONE) =>
          ([], Symbol.OrdMap.empty)

        | (SOME pat, SOME smlty) =>
          if isrec then
          let
            val x' = freshVar ()
            val (binds, VE) = trans' (pat, x', smlty)
          in
            ((x', MILTermOps.tabs(miltyvars, 
            MILTerm.Unfold (MILTerm.TApp(MILTerm.Var x,
          map (MILTy.tyvar o #1) miltyvars))))::binds, VE)
          end
          else trans' (pat, x, smlty)

        | _ =>
          Debug.fail "ValPat.trans: arity mismatch"
      end

    | PatRecord(openrec, patrow) =>
      transRec patrow ([], Symbol.OrdMap.empty)

    | PatLayer((v,smlty'), pat) => 
      let
        val (binds, VE) = trans' (pat, x, smlty)
      in
        (binds, Symbol.OrdMap.insert (VE, v, (x, transType smlty', [])))
      end

    | _ =>
      Debug.fail "ValPat.trans: refutable pattern"

  end

  val (binds, VE) = trans' (pat, x, smlty)
in
  { bindings = binds, VE = VE, TVE = TVE', tyvars = miltyvars }
end

end (* of local open *)
end (* of struct *)
