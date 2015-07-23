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
(* Operations on ML type schemes					*)
(*======================================================================*)
structure SMLSchOps :> SMLSCHOPS =
struct

local open SMLSch in

(*----------------------------------------------------------------------*)
(* Free type variables in a type scheme					*)
(*----------------------------------------------------------------------*)
fun tyvars (TypeScheme(tvs, ty)) = 
TyVar.Set.difference (SMLTy.tyvars ty, TyVar.Set.addList(TyVar.Set.empty, tvs))

fun tynames (TypeScheme(tvs, ty)) = SMLTy.tynames ty

(*----------------------------------------------------------------------*)
(* Quantify zero variables to convert a type into a type scheme		*)
(*----------------------------------------------------------------------*)
fun monoType ty = TypeScheme([], ty)

(*----------------------------------------------------------------------*)
(* Quantify all variables to convert a type into a type scheme		*)
(*----------------------------------------------------------------------*)
fun polyType ty = TypeScheme(TyVar.Set.listItems (SMLTy.tyvars ty), ty)

(*----------------------------------------------------------------------*)
(* Instantiate a type scheme to a type with fresh type variables 	*)
(* in place of the bound variables.					*)
(* Return the fresh type variables used, as types			*)
(*----------------------------------------------------------------------*)
fun instantiate (TypeScheme(tyvars, ty)) =
  let val S = 
    map (fn tyvar => 
      (tyvar, SMLTy.tyVarType (SMLTy.freshTyVar (TyVar.sort tyvar)))) tyvars
  in
    (map #2 S, SMLTy.appSubst S ty)
  end

fun appRealisation psi (TypeScheme(tyvars, ty)) =
  TypeScheme(tyvars, SMLTy.appRealisation psi ty)

fun toString (TypeScheme(tyvars, ty)) =
  (if Controls.isOn "showQuantifiers" 
  then Pretty.vec ("", "{", "} ", "{", "} ", ",") TyVar.toString tyvars
  else "") ^ SMLTy.openTypeToString ty

fun eq (TypeScheme(tyvars1, ty1), TypeScheme(tyvars2, ty2)) =
  length tyvars1=length tyvars2
  andalso SMLTy.eq(ty1, SMLTy.appSubst 
      (ListPair.zip(tyvars2, map SMLTy.tyVarType tyvars1)) ty2)

end (* of local open *)
end (* of struct *)
