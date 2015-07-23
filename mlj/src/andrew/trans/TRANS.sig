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
(* Translation into MIL							*)
(*======================================================================*)
signature TRANS =
sig

(*----------------------------------------------------------------------*)
(* Translate an SML typed term to a MIL term.				*)
(*----------------------------------------------------------------------*)
val trans : 
  {
    SE          : (Var.Var * MILTy.Type) Symbol.OrdMap.map,
                                                (* structure ID map *)
    EE          : MILTy.Type SMLTy.ExMap.map,   (* exception name map *)
    supply      : Var.Supply,                   (* initial supply *)
    entity  	: Entity.Ref,   		(* entity reference *)
    strexp      : SMLTerm.StrExp,		(* source term *)
    tynameTys   : MILTy.Type TyName.Map.map	(* type name -> MIL ty map *)
  }
  ->
  {
    term        : MILTerm.Cmp,                  (* target term *)
    cty         : MILTy.CmpType,                (* type of target term *)
    errors      : Error.Error list,             (* errors and warnings *)
    varsupply   : Var.Supply,                   (* value variable supply *)
    tyvarsupply : Var.Supply                    (* type variable supply *)
  }   

end
