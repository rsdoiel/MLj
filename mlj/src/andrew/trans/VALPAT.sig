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
(* Irrefutable tattern matching translation.   				*)
(*======================================================================*)
signature VALPAT =
sig

val trans : 
  {
    TVE : MILTy.Type TyVar.Map.map,
    TNE : MILTy.Type TyName.Map.map,
    tyvars : TyVar.TyVar list,
    pat : SMLTerm.Pat,
    var : Var.Var,
    smlty : SMLTy.Type
  } -> 
  {
    bindings : (Var.Var * MILTerm.Val) list,
    VE : TransOps.ValEnv,
    TVE : MILTy.Type TyVar.Map.map,
    tyvars : (Var.Var * MILTy.Kind) list
  }

end 
