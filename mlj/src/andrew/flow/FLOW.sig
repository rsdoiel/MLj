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
(* Flow analysis.                                                       *)
(*======================================================================*)
signature FLOW =
sig

val flow :   
  (* Set if value bindings are to be hoisted *)
  bool ->

  (* Type environment *)
  MILTy.Type Var.Map.map ->

  (* Computation term *)
  MILTerm.Cmp -> 

  {
    (* Scopes in which applications appear *)
    apps :   
    FlowTypes.Scope list Var.Map.map,

    (* Functions are local/known/other; scope is defining scope *)  
    kinds :
    (MILTerm.FunKind * FlowTypes.Scope) Var.Map.map,

    (* Refs that are used in a higher-order way *)
    higherrefs :  
    Var.Set.set,

    (* Bindings to be hoisted *)
    hoistedvals :
    (Var.Var * MILTerm.Val) MILPathOps.Map.map,

    (* Extra type variables to be added to hoisted variables *)
    hoistedvars : (Var.Var * MILTy.Type list) Var.Map.map,

    (* Function definitions to be hoisted (those with no fvs) *)
    hoistedfuns :
    ((Var.Var * MILTy.Kind) list * MILTerm.FunKind * MILTerm.FunDef) 
      MILPathOps.Map.map,

    (* The variables that are bound to hoisted functions *)
    hoistedfunvars :
    Var.Set.set
    
  }

end