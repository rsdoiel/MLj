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
(* Operations on ML type schemes (Section 4.5, p19 Defn)		*)
(*======================================================================*)
signature SMLSCHOPS =
sig

(*----------------------------------------------------------------------*)
(* Free type variables and type names in a type scheme			*)
(*----------------------------------------------------------------------*)
val tyvars         : SMLSch.TypeScheme -> TyVar.Set.set
val tynames        : SMLSch.TypeScheme -> TyName.Set.set

(*----------------------------------------------------------------------*)
(* Quantify zero variables to convert a type into a type scheme		*)
(*----------------------------------------------------------------------*)
val monoType 	   : SMLTy.Type -> SMLSch.TypeScheme

(*----------------------------------------------------------------------*)
(* Quantify all variables to convert a type into a type scheme		*)
(*----------------------------------------------------------------------*)
val polyType	   : SMLTy.Type -> SMLSch.TypeScheme

(*----------------------------------------------------------------------*)
(* Instantiate a type scheme to a type with fresh type variables in	*)
(* place of the bound variables.                                        *)
(*----------------------------------------------------------------------*)
val instantiate    : SMLSch.TypeScheme -> (SMLTy.Type list * SMLTy.Type)

(*----------------------------------------------------------------------*)
(* Apply a realisation							*)
(*----------------------------------------------------------------------*)
val appRealisation : 
  SMLTy.Realisation -> SMLSch.TypeScheme -> SMLSch.TypeScheme

(*----------------------------------------------------------------------*)
(* Pretty-print								*)
(*----------------------------------------------------------------------*)
val toString :
  SMLSch.TypeScheme -> string

(*----------------------------------------------------------------------*)
(* Equality								*)
(*----------------------------------------------------------------------*)
val eq :
  SMLSch.TypeScheme * SMLSch.TypeScheme -> bool


end

