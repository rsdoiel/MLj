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
(* ML type structures (Section 4.9, p20 Defn)		                *)
(* We depart slightly from the definition here: a type structure is one *)
(* of the following:                                                    *)
(*   1. An arity, tyname and constructor environment (for datatypes);   *)
(*   2. A type function (for type abbreviations and where-instantiated  *)
(*      abstract types);                                                *)
(*   3. An arity and tyname (for opaque types in signatures and         *)
(*      abstype in the core).                                           *)
(*======================================================================*)
signature TYSTR = 
sig

(*---------------------------------------------------------------------*)
(* The type for type structures and constructor/deconstructor functions.*)
(*----------------------------------------------------------------------*)
type TyStr
val makeConcrete : TyVar.TyVar list * SMLTy.Type -> TyStr
val makeAbstract : TyVar.TyVar list * TyName.TyName -> TyStr
val makeDatatype : bool * SMLTy.DatDef -> TyStr
val makeClassType: TyName.TyName * SMLTy.ClassDef -> TyStr
val fromAbstract : TyStr -> (TyVar.TyVar list*TyName.TyName) option
val fromDatatype : TyStr -> (bool * SMLTy.DatDef) option
val fromConcrete : TyStr -> (TyVar.TyVar list * SMLTy.Type) option
val fromClassType: TyStr -> (TyName.TyName * SMLTy.ClassDef) option

(*----------------------------------------------------------------------*)
(* Type names in a type structure					*)
(*----------------------------------------------------------------------*)
val tynames : TyStr -> TyName.Set.set

(*----------------------------------------------------------------------*)
(* Arity of a type structure						*)
(*----------------------------------------------------------------------*)
val arity : TyStr -> int

(*----------------------------------------------------------------------*)
(* Apply the type function associated with a type structure to some     *)
(* type parameters to produce a type.					*)
(*----------------------------------------------------------------------*)
val apply : TyStr * SMLTy.Type list -> SMLTy.Type

(*----------------------------------------------------------------------*)
(* Apply a type realisation to a type structure; this turns abstract	*)
(* tystrs into concrete tystrs and is disallowed on datatype tystrs.    *)
(*----------------------------------------------------------------------*)
val appRealisation : SMLTy.Realisation -> TyStr -> TyStr

(*----------------------------------------------------------------------*)
(* Apply a renaming to a type structure					*)
(*----------------------------------------------------------------------*)
val rename : TyName.Renaming -> TyStr -> TyStr

datatype MatchResult = 
  Success of SMLTy.Realisation
| Failure of string

val match1 :
  SMLTy.Realisation -> (TyStr * TyStr) -> MatchResult

val match2 :
  (TyStr * TyStr) -> string option

(*----------------------------------------------------------------------*)
(* Pretty-print.							*)
(*----------------------------------------------------------------------*)
val toString :
  TyStr -> string

(*----------------------------------------------------------------------*)
(* Equality								*)
(*----------------------------------------------------------------------*)
val eq :
  TyStr * TyStr -> bool

end


