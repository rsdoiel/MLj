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
(* Datatype for terms in MIL						*)
(*======================================================================*)
structure MILTerm = 
struct


(*----------------------------------------------------------------------*)
(* We distinguish between _atomic_ value terms and _flat_               *)
(* non-atomic value terms. The idea is that an atom corresponds         *)
(* to a Java `canonical form' -- that is, a local variable or constant. *)
(* Atoms (ranged over by a) are the following:                          *)
(*    Var x                             variable                        *)
(*    SCon (basety, jcon)               Java constant                   *)
(*    Coerce(a, ty)                     free coercion                   *)
(*    Tuple []                          free tuple (null pointer)       *)
(*    Null ty                           null value                      *)
(*    Fold(a, ty)                       mu introduction                 *)
(*    Unfold a                          mu elimination                  *)
(*    TApp(a, tys)                      type application                *)
(*    TAbs(tyvars, a)                   type abstraction                *)
(*    Inj(ty, i, [])                    if ty is enumeration or         *)
(*                                         ty has free NONE injection   *)
(*    Inj(ty, i, [a])                   if ty has free SOME injection   *)
(*                                                                      *)     
(* All other value terms (ranged over by v) must have the form:         *)
(*    a                                 atom                            *)
(*    Inj (ty, i, [a_1, ..., a_n])	(i+1)'th inj. into sum type ty  *)
(*    ExCon (exname, [a_1, ..., a_n])   exception constructor        	*)
(*    Tuple [a_1, ..., a_n]		n-tuple of values           	*)
(*    Proj (i, a)                       (i+1)'th component of tuple/con *)
(*    Closure(i, [a_1, ..., a_n])       i'th closure class with fvs vs  *)
(*                                      and app method tys as given     *)
(*    TAbs(tyvars, v)                   type abstraction                *)
(*                                                                      *)
(* IMPORTANT: non-atomic value terms *only* appear in Triv,LetVal,Init  *)
(*----------------------------------------------------------------------*)
datatype Val =
  Var of Var.Var
| SCon of MILTy.Type * Constants.constant
| Inj of MILTy.Type * int * Val list
| Coerce of Val * MILTy.Type
| ExCon of MILTy.Type * Val list
| Tuple of Val list
| Proj of int * Val
| TApp of Val * MILTy.Type list
| TAbs of (Var.Var * MILTy.Kind) list * Val
| Fold of Val * MILTy.Type
| Unfold of Val
| Null of MILTy.Type
| Closure of int * Val list

(*----------------------------------------------------------------------*)
(* Computation expressions (ranged over by e):        			*)
(*   App(a, [a_1, ..., a_n])						*)
(*     n-argument function application               			*)
(*   Java(j, [a_1, ..., a_n], NONE | SOME ty)  			        *)
(*     n-argument java primitive or method call				*)
(*   Let(e_1, ([(x_1,ty_1),...,(x_n,ty_n)], e_2))         		*)
(*     Moggi-let: let x_1,...,x_n <= e_1 in e_2		                *)
(*   Triv [v_1, ..., v_n]						*)
(*     Moggi-style value-into-computation, for multiple values		*)
(*   Case(a, [(i_1, abs_1), ..., (i_n, abs_n)], NONE | SOME e)	        *)
(*     case on sum, with or without default clause.		        *)
(*     The cases must be in ascending order.                            *)
(*   CaseSCon(a, [(c_1, abs_1), ..., (c_n, abs_n)], SOME e)             *)
(*     case on special constant with (obligatory) default.		*)
(*     The cases must be in ascending order for int-compatible types.   *)
(*     The abstractions must be parameterless.                          *)
(*   CaseExCon(a, [(excon_1, abs_1), ..., (excon_n, abs_n)], SOME e)    *)
(*     case on exception with (obligatory) default.       		*)
(*   Throw(a, tys, loc)							*)
(*     raise an exception (ve), whole expression has result tys    	*) 
(*   TryLet(e, abstr1, abstr2)						*)
(*     evaluate e; abstr1 is the handler, abstr2 is for success         *)
(*   LetRec([tv_1,...,tv_m], defs, e)                                   *)
(*     bind recursive function definitions in e                         *)
(*   LetClass(info, fields, methods, e)                                 *)
(*     Java class info; treated as a computation as it must not be      *)
(*     copied or removed.                                               *)
(*   Alloc(refkind,a)                                                   *)
(*     Create a new reference cell with initial value a.                *)
(*   Deref a                                                            *)
(*     Read the value of the reference cell a.                          *)
(*   Assign(a1,a2)                                                      *)
(*     Assign the value a2 to the reference cell a1.                    *)
(*   Cond(t,a1,a2,e1,e2)                                                *)
(*     Conditional test: if a1 t a2 then e1 else e2.                    *)
(*     For Java types, comparisons are allowed only on numeric types,   *)
(*       and equality tests are for pointer equality on reference types *)
(*   LetVal(x,v,e)                                                      *)
(*     let x <= val v in e                                              *)
(*   Init(x,i,v,e)                                                      *)
(*     initialise i'th component of tuple/constructor x to the value v  *)
(*     and rebind in e.                                                 *)
(*----------------------------------------------------------------------*)
and Cmp =
  App of Val * Val list		
| Java of 
  (Java.OpType * MILTy.Type option * JavaString.t option) * 
  Val list * MILTy.CmpType
| Let of Cmp * TAbstr
| LetVal of Var.Var * Val * Cmp
| Triv of Val list
| Case of int Cases
| CaseSCon of Constants.constant Cases
| CaseExCon of MILTy.Type Cases
| Cond of CondTest * Val * Val * Cmp * Cmp
| Throw of Val * MILTy.Type list * (Entity.Ref * Syntax.Location)
| TryLet of Cmp * TAbstr list * TAbstr
| LetFun of (Var.Var * MILTy.Kind) list * FunKind * FunDef * Cmp
| LetClass of 
  MILTy.Type * ClassInfo * FieldInfo list * MethodInfo list * Cmp
| Alloc of RefKind * Val
| Deref of Val
| Assign of Val*Val
| Encap of Cmp
| Init of Var.Var * int * Val * Cmp

and FunKind = 
  AnyFun			(* implemented by a closure *)
| KnownFun			(* implemented by a static method *)
| LocalFun			(* implemented by a block *)

and RefKind =
  AnyRef			(* implemented by an object *)
| GlobalRef			(* implemented by a static field *)

and FunDef =
  RecFun of RecFunDef           (* mutually recursive set of defns *)
| Fun of Var.Var * TAbstr       (* non-recursive function *)

and CondTest =
  MLEq                          (* ML-style structural equality *)
| JavaTest of Tests.test        (* Java-style test *)

withtype TypedVar = Var.Var * MILTy.Type

(*----------------------------------------------------------------------*)
(* Explicitly-typed abstraction on several variables (possibly none)	*)
(*----------------------------------------------------------------------*)
and TAbstr = TypedVar list * Cmp 

and RecFunDef = (Var.Var * Var.Var * TAbstr * MILTy.CmpType) list

(*----------------------------------------------------------------------*)
(* Implicitly-typed abstraction on several variables (possibly none)	*)
(*----------------------------------------------------------------------*)
and Abstr = Var.Var list * Cmp

(*----------------------------------------------------------------------*)
(* Common type for CaseCon, CaseExCon, CaseSCon				*)
(* Boolean is true if arguments to constructors are bound; if false     *)
(* then the constructor itself is bound.                                *)
(*----------------------------------------------------------------------*)
and 'a Cases = Val * bool * ('a * Abstr) (* NList. *) list * Cmp option

(*----------------------------------------------------------------------*)
(* Inheritance info				                        *)
(*----------------------------------------------------------------------*)
and ClassInfo = Class.flag list * MILTy.Type option * MILTy.Type list

(*----------------------------------------------------------------------*)
(* Name of field, modifiers, type, and optional initialiser		*)
(*----------------------------------------------------------------------*)
and FieldInfo  = 
  JavaString.t * Field.flag list * MILTy.Type * Constants.constant option

(*----------------------------------------------------------------------*)
(* Name of method, modifiers, types of arguments (not including "this"),*)
(* type of result (if any), and body (including "this" as first arg if  *)
(* non-static).                                                         *)
(* The variable associated with the body is just for convenience of     *)
(* identification.                                                      *)
(*----------------------------------------------------------------------*)
and MethodInfo = 
  JavaString.t * Method.flag list * 
  MILTy.Type list * MILTy.Type option * (Var.Var * Abstr) option

end
