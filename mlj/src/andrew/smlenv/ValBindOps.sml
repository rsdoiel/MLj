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
(* Value binding operations.                                            *)
(*======================================================================*)
structure ValBindOps :> VALBINDOPS =
struct

open ValBind

(*----------------------------------------------------------------------*)
(* Free type variables							*)
(*----------------------------------------------------------------------*)
fun tyvars (ExTy(ty,_))    = SMLTy.tyvars ty
  | tyvars (VarSch sch)    = SMLSchOps.tyvars sch
  | tyvars (ConSch(sch,_)) = SMLSchOps.tyvars sch
  | tyvars (JavaTys(_,tys))   =
    foldr TyVar.Set.union TyVar.Set.empty (map SMLTy.tyvars tys)

(*----------------------------------------------------------------------*)
(* Apply a type name renaming to a binding				*)
(*----------------------------------------------------------------------*)
fun rename r vb =
case vb of
  ExTy(ty,exname) => ExTy(SMLTy.renameType r ty,exname)
| VarSch(SMLSch.TypeScheme(tyvars, ty)) =>
  VarSch(SMLSch.TypeScheme(tyvars, SMLTy.renameType r ty))
| ConSch(SMLSch.TypeScheme(tyvars, ty), (isrec, (tyvars', tyname, CE))) =>
  ConSch(SMLSch.TypeScheme(tyvars, SMLTy.renameType r ty), 
    (isrec, (tyvars', TyName.rename r tyname,
    Symbol.OrdMap.map (Option.map (SMLTy.renameType r)) CE)))
| JavaTys(c,tys) =>
  JavaTys(c, map (SMLTy.renameType r) tys)

(*----------------------------------------------------------------------*)
(* Apply a realisation to a binding					*)
(*----------------------------------------------------------------------*)
fun appRealisation psi vb = 
case vb of
  ExTy(ty, exname) => ExTy(SMLTy.appRealisation psi ty, exname)
| VarSch(SMLSch.TypeScheme(tyvars, ty)) =>
  VarSch(SMLSch.TypeScheme(tyvars, SMLTy.appRealisation psi ty)) 
| ConSch(SMLSch.TypeScheme(tyvars, ty), (isrec, (tyvars', tyname, CE))) =>
  ConSch(SMLSch.TypeScheme(tyvars, SMLTy.appRealisation psi ty), 
    (isrec, (tyvars', tyname, 
    Symbol.OrdMap.map (Option.map (SMLTy.appRealisation psi)) CE)))
| JavaTys(c, tys) =>
  JavaTys(c, map (SMLTy.appRealisation psi) tys)

(*----------------------------------------------------------------------*)
(* Rename exceptions							*)
(*----------------------------------------------------------------------*)
fun appExMap exmap vb = 
case vb of
  ExTy(ty, exname) => ExTy(ty, 
  case SMLTy.ExMap.find(exmap, exname) of
    NONE => exname
  | SOME exname => exname)
| other => other

(*----------------------------------------------------------------------*)
(* Type names in a binding						*)
(*----------------------------------------------------------------------*)
fun tynames (ExTy(ty,_))    = SMLTy.tynames ty
  | tynames (VarSch(sch))   = SMLSchOps.tynames sch
  | tynames (ConSch(sch,_)) = SMLSchOps.tynames sch
  | tynames (JavaTys(_,tys))   = 
    foldr TyName.Set.union TyName.Set.empty (map SMLTy.tynames tys)

(*----------------------------------------------------------------------*)
(* String representation for diagnostics and signature display		*)
(*----------------------------------------------------------------------*)
fun toString (ExTy(ty,_))    = SMLTy.openTypeToString ty
  | toString (VarSch(sch))   = SMLSchOps.toString sch
  | toString (ConSch(sch,_)) = SMLSchOps.toString sch
  | toString (JavaTys(c,tys))   = 
    Pretty.simpleVec " and " SMLTy.openTypeToString tys

(*----------------------------------------------------------------------*)
(* Equality: used to compare signatures for purposes of recompilation	*)
(*----------------------------------------------------------------------*)
fun eq (VarSch(sch1), VarSch(sch2)) = 
    SMLSchOps.eq (sch1, sch2)

  | eq (ConSch(sch1,CE1), ConSch(sch2, CE2)) =
    SMLSchOps.eq(sch1, sch2) (* ?? andalso CE1=CE2 ?? *)

  | eq (ExTy(ty1,exname1), ExTy(ty2, exname2)) =
    SMLTy.eq(ty1,ty2) andalso SMLTy.ExMap.Key.compare(exname1, exname2) = EQUAL

  | eq (JavaTys(c1,tys1), JavaTys(c2,tys2)) =
    ClassHandle.equal(c1,c2) andalso Eq.list SMLTy.eq (tys1,tys2)

  | eq _ =
    false    

end
