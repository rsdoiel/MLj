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
(* MIL types: keep everything abstract so that they can be implemented  *)
(* efficiently.                                                         *)
(*======================================================================*)
signature MILTY =
sig

(*----------------------------------------------------------------------*)
(* The abstract data type for MIL value and computation types.    	*)
(*----------------------------------------------------------------------*)
type Type and CmpType 

(*----------------------------------------------------------------------*)
(* A Kind describes a type in the same way that a type describes a term *)
(* Kinds are currently the following:                                   *)
(*    Any         any value type;                                       *)
(*    Eq          any type admitting equality;                          *)
(*    Bound ty    an upper bound wrt the subtype order described below. *)
(* Future extension: arrows on kinds.                                   *)
(*----------------------------------------------------------------------*)
datatype Kind = Any | Eq | Bound of Type

(*----------------------------------------------------------------------*)
(* Type constructor functions. 						*)
(* There are two varieties of type variables:                           *)
(*   (1) Those that are bound in a term (i.e. by big lambda)            *)
(*       These are just generated from a variable supply.               *)
(*   (2) Those that are bound in a type (by a quantifier, a mu, or a    *)
(*       lambda). These are de Brujn indices (0 = innermost binder).    *)
(*       In the following constructs:                                   *)
(*         abs ([K_1,...,K_n], ty)                                      *)
(*         debforall ([K_1,...,K_n], ty)                                *)
(*         mu (i, [ty_1,...,ty_n])                                      *)
(*       the first binder (K_1 or ty_1) is treated as `innermost'.      *)
(*----------------------------------------------------------------------*)
val app   : Type * Type list -> Type        (* Type constructor application *)
val abs   : Kind list * Type -> Type        (* Type constructor abstraction *)
val tyvar : Var.Var -> Type                 (* Term-bound tyvar (var supply)*)
val deb   : int -> Type                     (* Type-bound tyvar (de Brujn) *)
val tyname: TyName.TyName -> Type           (* Imported type *)
val prod  : Type list -> Type               (* Product type *)
val sum   : Type list list -> Type          (* Flattened sum type *)
val exn   : MILExn.Exn*Type list -> Type    (* (ML) Exception type *)
val arrow : Type list * CmpType -> Type     (* Multiple arg arrow type *)
val refty : Type -> Type                    (* Reference type *)
val array : Type -> Type                    (* Array type *)
val vector: Type -> Type                    (* Vector type *)
val java  : Types.base_type -> Type         (* Java primitive/class type *)
val debforall : Kind list * Type -> Type    (* Polymorphic type, de Brujn *)
val forall:(Var.Var*Kind) list*Type->Type   (* Polymorphic type, named vars *)
val cmp  : Effect.Effect*Type list->CmpType (* Computation type *)
val mu    : int * Type list -> Type         (* (i+1)'th recursive type *)

(*----------------------------------------------------------------------*)
(* These types only appear late in compilation				*)
(*----------------------------------------------------------------------*)
val con   : Type list -> Type             (* Universal constructor *)
val closure: int option*Type list -> Type (* Closure type *)
          
(*----------------------------------------------------------------------*)
(* noeffect tys is shorthand for cmp(Effect.none, tys)			*)
(*----------------------------------------------------------------------*)
val noeffect  : Type list -> CmpType

(*----------------------------------------------------------------------*)
(* Type deconstructors. 		                          	*)
(*----------------------------------------------------------------------*)
val fromProd      : Type -> Type list option
val fromSum       : Type -> Type list list option
val fromCon       : Type -> Type list option
val fromExn       : Type -> (MILExn.Exn * Type list) option
val fromArrow     : Type -> (Type list * CmpType) option
val fromRefty     : Type -> Type option
val fromArray     : Type -> Type option
val fromVector    : Type -> Type option
val fromJava      : Type -> Types.base_type option
val fromTyvar     : Type -> Var.Var option
val fromForall    : Type -> (Kind list * Type) option
val fromCmp       : CmpType -> Effect.Effect * Type list
val fromMu        : Type -> (int * Type list) option
val fromClosure   : Type -> (int option * Type list) option
val fromTyname    : Type -> TyName.TyName option
val fromDeb       : Type -> int option

(*----------------------------------------------------------------------*)
(* Derived type deconstructors.						*)
(*                                                                      *)
(*   fromProdCon returns SOME tys for any type of the form              *)
(*     prod tys, con tys, exn(e,tys) or closure(i,tys)                  *)
(*   or NONE if not of this form.                                       *)
(*                                                                      *)
(*----------------------------------------------------------------------*)
val fromProdCon   : Type -> Type list option

(*----------------------------------------------------------------------*)
(* Finite maps for domain types and pairs of types.			*)
(*----------------------------------------------------------------------*)
structure Map     : ORD_MAP where type Key.ord_key = Type
structure PairMap : ORD_MAP where type Key.ord_key = Type*Type

(*----------------------------------------------------------------------*)
(* Substitute types for (term-bound) type variables.			*)
(*----------------------------------------------------------------------*)
val subst         : Type Var.Map.map -> Type -> Type

(*----------------------------------------------------------------------*)
(* Given an initial map from types to types (with no free type-bound    *)
(* tyvars), construct a memoizing function that applies this            *)
(* recursively to a type.                                               *)
(*----------------------------------------------------------------------*)
val replace       : Type Map.map -> (Type -> Type)

(*----------------------------------------------------------------------*)
(* Given a function whose domain is types, construct a memoized version	*)
(*----------------------------------------------------------------------*)
val memoize       : ((Type -> 'a) -> (Type -> 'a)) -> (Type -> 'a)

(*----------------------------------------------------------------------*)
(* Equality. Constant time because of hash-consing.                	*)
(*----------------------------------------------------------------------*)
val eq            : Type * Type -> bool

(*----------------------------------------------------------------------*)
(* `Equivalence'. This ignores effect information and is *not* constant *)
(* time. Use only for diagnostic purposes!                              *)
(*----------------------------------------------------------------------*)
val equiv         : Type * Type -> bool

(*----------------------------------------------------------------------*)
(* We define a subtype ordering <= on types, with the interpretation:   *)
(*      ty1 <= ty2                                                      *)
(* iff  ty1 will get exactly the same Java representation type as ty2,  *)
(*      AND { rep(v) | v : ty1 } is a subset of { rep(v) | v : ty2 }    *)
(*      where rep(v) is the Java representation of MIL value v.         *)
(*                                                                      *)
(* The subtyping ordering is taken wrt bounds on type variables.        *)
(*----------------------------------------------------------------------*)
val forceBounds   : Kind Var.Map.map -> Type -> Type
val leq           : Kind Var.Map.map -> Type * Type -> bool
val leqs          : Kind Var.Map.map -> Type list * Type list -> bool

(*----------------------------------------------------------------------*)
(* Given two closed types ty1 and ty2, return				*)
(*   SOME ty    if ty is the least upper bound of ty1 and ty2 wrt       *)
(*              the subtype ordering;                                   *)
(*   NONE       if no lub exists.                                       *)     
(*----------------------------------------------------------------------*)
val lub           : Type * Type -> Type option
val lubs          : Type list * Type list -> Type list option

(*----------------------------------------------------------------------*)
(* Subclassing test (used for exceptions). This is *not* the same as    *)
(* subtyping. Returns NONE for "don't know" or "can't be bothered".     *)
(*----------------------------------------------------------------------*)
val subClass      : Type * Type -> bool option

(*----------------------------------------------------------------------*)
(* Given a type ty, determine whether inl <> : 1+ty is represented by	*)
(* Java's null pointer.                                                 *)
(*----------------------------------------------------------------------*)
val noneIsNull    : Kind Var.Map.map -> Type -> bool

(*----------------------------------------------------------------------*)
(* Given a type ty, determine whether inr <v> : 1+ty is a no-op.	*)
(*----------------------------------------------------------------------*)
val someIsNop     : Kind Var.Map.map -> Type -> bool

(*----------------------------------------------------------------------*)
(* Unfold a recursive type.           					*)
(*----------------------------------------------------------------------*)
val unfold        : int * Type list -> Type

(*----------------------------------------------------------------------*)
(* Free type variables. Constant time because of memoization.        	*)
(*----------------------------------------------------------------------*)
val tyvars        : Type -> Var.Set.set

(*----------------------------------------------------------------------*)
(* Union the effects in two computation types.				*)
(* The result types are taken from the second argument.                 *)
(*----------------------------------------------------------------------*)
val unionCmpTypes : CmpType * CmpType -> CmpType
val cmpTypePlus   : CmpType * Effect.Effect -> CmpType

(*----------------------------------------------------------------------*)
(* Pretty-printing							*)
(*----------------------------------------------------------------------*)
val toString      : Type -> string
val cmpToString   : CmpType -> string
val boundTyVarToString : Var.Var * Kind -> string

(*----------------------------------------------------------------------*)
(* General deconstructor.                                           	*)
(*----------------------------------------------------------------------*)
val deconstruct : Type ->
{
  tyvar     : Var.Var -> 'a,
  deb       : int -> 'a,
  tyname    : TyName.TyName -> 'a,
  java      : Types.base_type -> 'a,
  refty     : Type -> 'a,
  array     : Type -> 'a,
  vector    : Type -> 'a,
  prod      : Type list -> 'a,
  con       : Type list -> 'a,
  exn       : MILExn.Exn * Type list -> 'a,
  sum       : Type list list -> 'a,
  arrow     : Type list * CmpType -> 'a,
  mu        : int * Type list -> 'a,
  forall    : Kind list * Type -> 'a,
  closure   : int option * Type list -> 'a
} -> 'a

(*----------------------------------------------------------------------*)
(* Hash table info: each bucket with the unmasked hash codes and types. *)
(* Use only for diagnostics.                                            *)
(*----------------------------------------------------------------------*)
val stats : unit -> (word*Type) list list

end