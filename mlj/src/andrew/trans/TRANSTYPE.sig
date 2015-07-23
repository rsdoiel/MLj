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
(* Translation of SML types and environments into MIL.			*)
(*======================================================================*)
signature TRANSTYPE =
sig

(*----------------------------------------------------------------------*)
(* Translate an SML type into a MIL type				*)
(*----------------------------------------------------------------------*)
val transType : 
  MILTy.Type TyVar.Map.map ->    (* type variable bindings *)
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  SMLTy.Type ->                  (* source type *)
  MILTy.Type                     (* target type *)

(*----------------------------------------------------------------------*)
(* Translate a closed SML type scheme into a MIL type                   *)
(*----------------------------------------------------------------------*)
val transScheme :
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  SMLSch.TypeScheme ->           (* source type scheme *)
  MILTy.Type                     (* target type *)

(*----------------------------------------------------------------------*)
(* Translate a constructor environment as a sum type			*)
(*----------------------------------------------------------------------*)
val transCE :
  MILTy.Type TyVar.Map.map ->       (* type variable bindings *)
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  SMLTy.Type option Symbol.OrdMap.map ->
  MILTy.Type  

(*----------------------------------------------------------------------*)
(* Translate realisation						*)
(*----------------------------------------------------------------------*)
val transRealisation :
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  SMLTy.Realisation ->           (* the realisation *)
  MILTy.Type TyName.Map.map

(*----------------------------------------------------------------------*)
(* Translate datatype definitions					*)
(*----------------------------------------------------------------------*)
val transDE :
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  SMLTy.DatEnv ->                (* the datatype environment *)
  MILTy.Type TyName.Map.map


(*----------------------------------------------------------------------*)
(* Translate the environment for a top-level structure into its   	*)
(* corresponding MIL tuple type.                                        *)
(*----------------------------------------------------------------------*)
val transE :
  MILTy.Type TyName.Map.map ->   (* type name bindings *)
  Env.Env -> 
  MILTy.Type


end
