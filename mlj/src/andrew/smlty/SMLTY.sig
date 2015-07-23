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
(* SML types (Section 4.2, p17 Defn)					*)
(* We also put the monad for type inference here.			*)
(*======================================================================*)
signature SMLTY = 
sig

(*----------------------------------------------------------------------*)
(* The datatype used for ML types is hidden.				*)
(*----------------------------------------------------------------------*)
type Type

(*----------------------------------------------------------------------*)
(* Java field: name, modifiers, type and final static value info.       *)
(*----------------------------------------------------------------------*)
type FieldInfo = 
  JavaString.t * Field.flag list * Type * Constants.constant option

(*----------------------------------------------------------------------*)
(* Java method: name, modifiers, argument types and (optional) result   *)
(* type.                                                                *)
(*----------------------------------------------------------------------*)
type MethodInfo = 
  JavaString.t * Method.flag list * Type list * Type option

(*----------------------------------------------------------------------*)
(* Java class: modifiers, (optional) superclass and list of interfaces. *)
(*----------------------------------------------------------------------*)
type ClassInfo = 
  Class.flag list * Type option * Type list

(*----------------------------------------------------------------------*)
(* The full class definition.						*)
(*----------------------------------------------------------------------*)
type ClassDef =
  ClassInfo * FieldInfo list * MethodInfo list

(*----------------------------------------------------------------------*)
(* Datatype environments: a list of lists (strongly-connected 		*)
(* components) each consisting of					*)
(*   the type variable arguments to the type constructor		*)
(*   the stamp associated with the type constructor			*)
(*   whether the scc is recursive                                       *)
(*   the constructor environment: types (or NONE) for each constructor	*)
(*   or an empty map if this is a replicated datatype                   *)
(*----------------------------------------------------------------------*)
type DatDef = TyVar.TyVar list * TyName.TyName * Type option Symbol.OrdMap.map
type DatEnv = (bool * DatDef list) list

type ClassEnv = ClassDef TyName.Map.map

(*----------------------------------------------------------------------*)
(* MLExn:   an exception defined by exception <excon> [ of <ty> ]       *)
(*          the boolean indicates generativity.                         *)
(* JavaExn: an exception defined by _classexception <excon> = <class>   *)
(*----------------------------------------------------------------------*)
datatype Exn = 
  MLExn of Type option * bool
| JavaExn of Type

(*----------------------------------------------------------------------*)
(* An exception environment (for a single module) maps stamps to the    *)
(* exn info as defined above and the qualified identifier for display.	*)
(*----------------------------------------------------------------------*)
type ExEnv = (Exn*Syntax.longid) IMap.map

(*----------------------------------------------------------------------*)
(* An exception name is then a module identifier paired with a stamp.	*)
(*----------------------------------------------------------------------*)
type ExName = Entity.Ref * int

structure ExMap : ORD_MAP where type Key.ord_key = ExName

(*----------------------------------------------------------------------*)
(* A realisation is the type-name version of `substitution'.		*)
(* A renaming is a special case.                                        *)   
(*----------------------------------------------------------------------*)
type TypeFcn = TyVar.TyVar list * Type
type Realisation = TypeFcn TyName.Map.map

(*----------------------------------------------------------------------*)
(* Various type constructor functions					*)
(*----------------------------------------------------------------------*)
val tyVarType  : TyVar.TyVar -> Type
val funType    : Type*Type -> Type
val recType    : (Syntax.symbol*Type) list -> Type
val consType   : Type list * TyName.TyName -> Type
val baseType   : TyName.TyName -> Type
val tupleType  : Type list -> Type
val refType    : Type -> Type
val arrayType  : Type -> Type

(*----------------------------------------------------------------------*)
(* Destructor functions: get the type of a particular field in a record	*)
(* type (and its position), and get the content type of a ref type.	*)
(*----------------------------------------------------------------------*)
val fieldType     : Type*Syntax.symbol -> Type*int*int
val fromTyVar     : Type -> TyVar.TyVar option
val fromFunType   : Type -> (Type * Type) option
val fromConsType  : Type -> (Type list * TyName.TyName) option
val fromRefType   : Type -> Type option
val fromArrayType : Type -> Type option
val fromProd      : Type -> Type list option

(*----------------------------------------------------------------------*)
(* Determine the sort of a type or list of types			*)
(* If the first argument is true, then the sorts of all type variables  *)
(* are taken to be TySort.Eq (used for datatype equality status)        *)
(*----------------------------------------------------------------------*)
val sort           : bool -> Type -> TySort.Sort
val sortList       : bool -> Type list -> TySort.Sort

(*----------------------------------------------------------------------*)
(* Free type variables and type names 					*)
(*----------------------------------------------------------------------*)
val tyvars         : Type -> TyVar.Set.set
val tynames   	   : Type -> TyName.Set.set

(*----------------------------------------------------------------------*)
(* Apply a substitution							*)
(*----------------------------------------------------------------------*)
val appSubst 	   : (TyVar.TyVar*Type) list -> Type -> Type
val eq             : Type*Type -> bool
val compare        : Type*Type -> order
val appRealisation : Realisation -> Type -> Type
val renameType     : TyName.Renaming -> Type -> Type
 
(*----------------------------------------------------------------------*)
(* Pretty-print a type.							*)
(* A type name environment is required for pretty-printing the tynames. *)
(*----------------------------------------------------------------------*)
val exNameToString : ExName -> string
val tyvarsToString : TyVar.TyVar list -> string
val toString	   : Type -> string
val openTypeToString: Type -> string
val DEtoString     : DatEnv -> string
val EEtoString     : ExEnv -> string
val realisationToString : Realisation -> string

(*----------------------------------------------------------------------*)
(* Translate a type inductively using the functions provided.		*)
(*----------------------------------------------------------------------*)
val trans : 
  (TyVar.TyVar -> 'a) 			(* for type variables *)
* (TyName.TyName * 'a list -> 'a) 	(* for constructed types *)
* ((Syntax.symbol*'a) list -> 'a)       (* for record types *)
* ('a * 'a -> 'a)			(* for function types *)
* ('a -> 'a) 				(* for reference types *)
* ('a -> 'a) 				(* for array types *)
-> Type -> 'a

val transRealisation : 
  (TyVar.TyVar list * TyName.TyName * Type -> 'a) ->
  Realisation -> 'a list

type 'a ElabResult = 
  'a * Error.Error list * DatEnv * ExEnv * ClassEnv * Realisation
type ErrorArg = string * Type

val error       : Error.Error * ErrorArg list -> unit
val freshTyVar  : TyVar.Sort -> TyVar.TyVar
val freshType   : unit -> Type
val freshConstrainedType : Type list -> Type
val openRecType : (Syntax.symbol*Type) list -> Type

val freshTyName : Syntax.longid * TySort.Sort -> TyName.TyName
val freshRecTyNames: Syntax.longid list * TySort.Sort -> TyName.TyName list
val makeRenaming: 
  Syntax.longid * TyName.Set.set -> TyName.Renaming * TyName.Set.set

val freshVar    : unit -> Syntax.symbol 
val addDE       : DatEnv -> unit
val addClass    : TyName.TyName * ClassDef -> unit
val appRenamingDE: TyName.Renaming -> unit 
val addRealisation : Realisation -> unit

val freshExName     : Syntax.longid*Type option*bool -> ExName 
val freshJavaExName : Syntax.longid*Type -> ExName 
val getStamp        : unit -> TyName.Supply
val getEntity       : unit -> Entity.Ref 
val getClassEnv     : unit -> ClassEnv

val runelab         : Entity.Ref -> ClassEnv -> (unit -> 'a) -> 'a ElabResult

(*----------------------------------------------------------------------*)
(* Unify two types 							*)
(*----------------------------------------------------------------------*)
val unify :  
   (Syntax.Location option * string * Type) *
   (Syntax.Location option * string * Type)  (* The two types to be unified *)
-> Type

(*----------------------------------------------------------------------*)
(* Anti-unify two types, or a list of types				*)
(*----------------------------------------------------------------------*)
val antiunify :
   Type * Type -> ((Type * Type) TyVar.Map.map * Type) option
val antiunifylist :
   Type list -> (Type list TyVar.Map.map * Type) option

(*----------------------------------------------------------------------*)
(* Resolve the overloaded type variables in a type excluding those      *)
(* in the set given as argument.                                        *)
(*----------------------------------------------------------------------*)
val resolve : 
  Syntax.Location		(* Location at which to report errors *)
-> TyVar.Set.set                (* Exclude these variables from resolution *)
-> Type 			(* The type to resolve *)
-> TyVar.Set.set

(*----------------------------------------------------------------------*)
(* Given psi, ty1 and ty2, find a substitution S such that              *)
(* S(ty1) = ty2).                                                       *)
(*----------------------------------------------------------------------*)
val match :
   bool 
-> Type * Type			(* ty1 and ty2 *)
-> Type TyVar.Map.map option	(* SOME(S) or NONE if no match *)

val findCon: 
  TyName.TyName * Type option Symbol.OrdMap.map * Syntax.symbol -> 
  int * Type option

end

