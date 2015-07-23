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
(* Elaboration of pattern expressions					*)
(*======================================================================*)
structure ElabPat :> ELABPAT =
struct

local 
  open Syntax ElabOps Env EnvOps SMLTy SMLPrimTy ElabTy
in

structure Map = Symbol.OrdMap

(*----------------------------------------------------------------------*)
(* Patterns (p26-27 Defn)						*)
(* Return a quadruple consisting of:					*)
(*   the translated expression						*)
(*   an environment for the pattern variables 				*)
(*   a type for the pattern						*)
(*   whether or not the pattern is `valuable'                           *)
(*----------------------------------------------------------------------*)
fun infPat (C : Context) ((loc,prepat) : Pat) =
  case prepat of

  (* Rule 32 *)
    PatWild => 
    (SMLTerm.PatWild, Map.empty, SMLTy.freshType (), true)

  (* Rule 33 *)
  | PatSCon scon => 
    (SMLTerm.PatSCon(scon, loc), Map.empty, ElabSCon.typeSCon scon, false)

  (* Rule 34,35 *)
  | PatVar vid =>
    let
      val vid = ElabOps.vidToLongid vid
    in
      case EnvLookup.lookupVid (EofC C, loc, vid) of

    (* Rule 35 *)
      SOME (ValBind.ConSch(sch,CE as (_,(_,_,m)))) =>
      let val (tys, ty) = SMLSchOps.instantiate sch
      in
        (SMLTerm.PatCon(List.last vid, CE, tys, NONE), Map.empty, ty, 
        Map.numItems m = 1)
      end

    (* Rule 35 *)
    | SOME (ValBind.ExTy(ty, exbind)) => 
      (unify ((SOME loc, "exception pattern", ty),
                   (NONE, "expected", exnType));
      (SMLTerm.PatExCon(exbind, NONE), Map.empty, ty, false))

    (* Rule 34 *)
    | _ =>       
      case vid of
        [v] =>
        let
          val ty = SMLTy.freshType ()
        in
          (SMLTerm.PatVar (v,ty), Map.insert (Map.empty, v, 
          ValBind.VarSch(SMLSchOps.monoType ty)), ty, true)
        end

      | _ => 
        (error (Error.error (loc, 
          "found a variable when expecting a constructor: " ^ 
          Pretty.longidToString vid), []);
        (SMLTerm.PatWild, Map.empty, SMLTy.freshType (), true))
  end

  (* Rule 36 *)
  | PatRecord(openrec, patrow) =>
    let 
      val (row, VE', rho, valuable) = infPatRow C patrow
      val rty = 
        if openrec then SMLTy.openRecType (Map.listItemsi rho)
        else SMLTy.recType (Map.listItemsi rho)
    in
      (SMLTerm.PatRecord(openrec, row), VE', rty, valuable)
    end

  (* Rule 37 elided: parentheses *)

  (* Rule 40 elided: atpat -> pat *)

  (* Special case of rule 35 with references *)
  | PatCon(vid, pat) => 
    if (case vid of [id] => Symbol.equal(id, 
      Symbol.symbol (JavaString.fromString "ref")) | _ => false)
    then
      let
        val (p, VE', ty', valuable) = infPat C pat
      in
        (SMLTerm.PatRef p, VE', SMLTy.refType ty', false)
      end
    else
  
  (* Rules 41 *)
    (case EnvLookup.lookupVid (EofC C, loc, vid) of
    SOME (ValBind.ConSch(sch, CE as (_,(_,_,m)))) => 
      let
        val ty = SMLTy.freshType ()
        val (tys, ty'') = SMLSchOps.instantiate sch
        val (p, VE', ty', valuable) = infPat C pat
      in
        unify ((SOME loc, "constructor", ty''),
                   (NONE, "expected", funType (ty',ty)));
        (SMLTerm.PatCon(List.last vid, CE, tys, SOME p), VE', ty, 
        Map.numItems m = 1 andalso valuable)
      end

  | SOME (ValBind.ExTy(ty',exbind)) =>
      let
        val (p, VE', ty, valuable) = infPat C pat
      in
        unify ((SOME loc, "exception constructor", ty'),
                 (NONE, "expected", funType (ty, exnType)));
        (SMLTerm.PatExCon(exbind, SOME(ty,p)), VE', exnType, false)
      end

  | _ => 
      (error (Error.error (loc, 
        "non-constructor applied to argument in pattern: " ^
        Pretty.longidToString vid), []);
      (SMLTerm.PatWild, Map.empty, SMLTy.freshType (), true))
  )

  (* Rule 42 *)
  | PatConstraint(pat as (loc1,_), tyexp as (loc2,_)) =>
    let
      val (p, VE, ty, valuable) = infPat C pat
      val ty' = infTy C tyexp
      val ty'' = unify ((SOME loc1, "pattern", ty),
                 (SOME loc2, "type constraint", ty'))
    in
      (p, VE, ty'', valuable)
    end

  (* Rule 43 *)
  | PatLayer((_,v), tyopt, pat as (loc,_)) => 
    let
      val (p, VE, ty, valuable) = infPat C pat
      val ty = 
        case tyopt of
        NONE => ty
      | SOME (ty' as (loc',_)) => 
        let
          val ty' = infTy C ty'
          val ty = unify ((SOME loc, "pattern", ty),
               (SOME loc', "type constraint", ty'))
        in
          ty'
        end
    in
      (SMLTerm.PatLayer((v,ty), p), Map.insert (VE, v, 
        ValBind.VarSch(SMLSchOps.monoType ty)), ty, valuable)
    end

  (* Derived forms *)
  | PatTuple pats =>
    infPat C (patTuple loc pats)

  | PatList pats =>
    infPat C (patList loc pats)

(*----------------------------------------------------------------------*)
(* Pattern Rows (p26 Defn)						*)
(*----------------------------------------------------------------------*)
and infPatRow C patrow = 
case patrow of
(* Rule 38 *)
  [] => ([], Map.empty, Map.empty, true)

(* Rule 39 *)
| (lab, pat as (loc,_))::patrow =>
  let
    val (p, VE, ty, valuable) = infPat C pat
    val (ps, VE', tyrow, valuable') = infPatRow C patrow
    val VE = patmerge loc (VE, VE')
  in
    ((lab,p)::ps, VE, Map.insert(tyrow, lab, ty), 
    valuable andalso valuable')
  end

end (* of local open *)

end (* of struct *)
