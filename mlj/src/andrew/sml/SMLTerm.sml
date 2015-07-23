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
(* Type-annotated SML terms						*)
(* These differ from the abstract syntax tree in the following ways:	*)
(* 1. identifier status has been resolved, so Vid -> Var, Con or ExCon;	*)
(*    similarly in patterns.                                            *)
(* 2. quantifiers are eliminated explicitly in Var and Con and 		*)
(*    introduced explicitly in Val and ValRec.				*)
(* 3. types are explicit in a number of places.         		*)
(* 4. declarations include only value bindings -- no types.             *)
(* 5. paths through structures are annotated with projection indexes.   *)
(* Some explicit types are technically redundant but make it possible   *)
(* to translate terms into MIL without doing type inference again.      *)
(*======================================================================*)
structure SMLTerm = 
struct

(*----------------------------------------------------------------------*)
(* Typed (core) expressions.						*)
(*----------------------------------------------------------------------*)
datatype Exp =
(* Special constants; source location retained for overflow errors *)
  SCon of SCon.SCon * SMLTy.Type * Syntax.Location

(* Java constants with their type: used for inlining only *)
| JCon of Constants.constant

(* Variables specialised at a particular types *)
| Var of longid * SMLTy.Type list

(* Overloaded variable with given overloaded sort specialised at given types *)
(* The longid will simply be bound to a tuple (not a type abstraction) *)
(* If type parameters are present, then translate as a projection *)
| OverloadedVar of longid * TyName.Set.set * SMLTy.Type list

(* Constructors specialised at a particular type, with constructor env *)
| Con of Syntax.symbol * (bool * SMLTy.DatDef) * SMLTy.Type list

(* Exception constructors with type of parameter *)
| ExCon of SMLTy.ExName * SMLTy.Type option

(* Function application *)
| App of Exp * Exp

(* Typed function abstractions *)
| Fn of SMLTy.Type * Match

(* Let *)
| Let of Dec * Exp

(* Handle an exception *)
| Handle of Exp * Match

(* Raise an exception; location used for uncaught top-level exceptions *)
| Raise of Exp * SMLTy.Type * Syntax.Location

(* Records; retain order of fields because of side-effects in expressions *)
| Record of (Syntax.symbol*Exp) list

(* Java operations *)
| Java of (Java.OpType * SMLTy.Type option * JavaString.t option) * Exp list * 
    SMLTy.Type option * Effect.Effect

(*----------------------------------------------------------------------*)
(* Declarations: for core and modules              			*)
(*----------------------------------------------------------------------*)
and DecItem =

(* Non-recursive bindings *)
  Val of Syntax.Location * TyVar.TyVar list * SMLTy.Type * Pat * Exp

(* Recursive bindings *)
| ValRec of TyVar.TyVar list * RecBind NList.list

(* Local declaration *)
| Local of Dec * Dec

(* New names for exceptions *)
| Exception of SMLTy.ExName

(* Internal class type definition; constructors appear in methods *)
| ClassType of TyName.TyName * SMLTy.ClassInfo * Fields * Methods

(* Structure bindings *)
| Structure of Syntax.symbol * StrExp

(*----------------------------------------------------------------------*)
(* Patterns								*)
(*----------------------------------------------------------------------*)
and Pat =

(* Wildcards *)
  PatWild

(* Special constants; source location retained for overflow errors *)
| PatSCon of SCon.SCon * Syntax.Location

(* Typed variables (types only necessary for Bind exceptions!)  *)
| PatVar of Syntax.symbol * SMLTy.Type

(* Datatype constructors, possibly applied *)
| PatCon of Syntax.symbol * (bool*SMLTy.DatDef) * SMLTy.Type list * Pat option

(* Exception constructors, possibly applied *)
| PatExCon of SMLTy.ExName * (SMLTy.Type*Pat) option

(* Reference types *)
| PatRef of Pat

(* Records *)
| PatRecord of bool * ((Syntax.symbol * Pat) list)

(* Layered patterns *)
| PatLayer of (Syntax.symbol*SMLTy.Type) * Pat

(*----------------------------------------------------------------------*)
(* Structure expressions                                                *)
(*----------------------------------------------------------------------*)
and StrExp =
(* Explicit structure *)
  Struct of Syntax.symbol Symbol.OrdMap.map * StrExp Symbol.OrdMap.map

(* Named structure *)
| Strid of longid

(* Core let *)
| StrLet of Dec * StrExp

(*----------------------------------------------------------------------*)
(* Recursive function binding						*)
(*----------------------------------------------------------------------*)
withtype RecBind = Syntax.symbol * Exp * SMLTy.Type
and Dec = DecItem list
and longid = Syntax.symbol * (Syntax.symbol * int) list

(* Match clauses have source locations used for warning messages and
   the type of the rhs for use in the translation to MIL *)
and Match = SMLTy.Type * (Syntax.Location * Pat * Exp) list
and Fields = (SMLTy.FieldInfo * Exp option) list
and Methods = (SMLTy.MethodInfo * Symbol.symbol option list * Exp option) list

end
