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
(* Various operations on SML Java types  				*)
(*======================================================================*)
signature SMLJAVAOPS =
sig

val fromOptionType : SMLTy.Type -> SMLTy.Type option

(*----------------------------------------------------------------------*)
(* Tests for various categories of Java types (see 4.2, JLS)        	*)
(*----------------------------------------------------------------------*)
val isReference    : SMLTy.Type -> bool
val isPrimitive    : SMLTy.Type -> bool
val isNumeric      : SMLTy.Type -> bool
val isArithmetic   : SMLTy.Type -> bool
val isFloatingPoint: SMLTy.Type -> bool
val isIntegral     : SMLTy.Type -> bool

(*----------------------------------------------------------------------*)
(* Given a predicate for exportability of internal class types, return  *)
(* a predicate for types.                                               *)
(* A type is exportable if it is:                                       *)
(*   a primitive Java type; or                                          *)
(*   <ty> option where <ty> is an exportable class type; or             *)
(*   <ty> Java.array option where <ty> is exportable.                   *) 
(*----------------------------------------------------------------------*)
val isExportable   : (TyName.TyName -> bool) -> (SMLTy.Type -> bool)

(*----------------------------------------------------------------------*)
(* Given a predicate for determining whether a class is a Java class,	*)
(* return a predicate for types.                                        *)
(* A type is a Java type if it is:                                      *)
(*   a primitive Java type; or                                          *)
(*   <ty> or <ty> option where <ty> is a Java class type; or            *)
(*   <ty> Java.array or <ty> Java.array option with <ty> a Java type.   *)
(*----------------------------------------------------------------------*)
val isJava         : (TyName.TyName -> bool) -> (SMLTy.Type -> bool)

val fromJava       : SMLTy.Type -> SMLTy.Type option

val isExportableClassDef : 
  (TyName.TyName -> bool) -> (SMLTy.Type * SMLTy.ClassDef) -> string option

val primWidening  : SMLTy.Type * SMLTy.Type -> bool
val primNarrowing : SMLTy.Type * SMLTy.Type -> bool
val methodConv    : SMLTy.ClassEnv -> bool -> SMLTy.Type * SMLTy.Type -> bool

val package       : SMLTy.Type -> Syntax.longid

val getInherited    :   
  SMLTy.ClassEnv ->
  SMLTy.Type * bool * 
  (SMLTy.FieldInfo -> bool) * (SMLTy.MethodInfo -> bool) ->
  ((SMLTy.Type * SMLTy.FieldInfo) list * 
   (SMLTy.Type * SMLTy.MethodInfo) list)
  
val getMethods    :
  SMLTy.ClassEnv ->   
  (bool * SMLTy.Type * bool * JavaString.t * SMLTy.Type list) ->
  ((SMLTy.Type * SMLTy.MethodInfo) list * 
   (SMLTy.Type * SMLTy.MethodInfo) list)
  
val getConstructors    :   
  SMLTy.ClassEnv ->
  (bool * SMLTy.Type * SMLTy.Type list) ->
  ((SMLTy.Type * SMLTy.MethodInfo) list * 
   (SMLTy.Type * SMLTy.MethodInfo) list)
  
val getFields     :
  SMLTy.ClassEnv -> 
  (bool * SMLTy.Type * JavaString.t) ->
  ((SMLTy.Type * SMLTy.FieldInfo) list)

val mostSpecific  :
  SMLTy.ClassEnv -> 
  ((SMLTy.Type * SMLTy.MethodInfo) * 
   (SMLTy.Type * SMLTy.MethodInfo) list) ->
  (SMLTy.Type * SMLTy.MethodInfo) option

val cast : 
  SMLTy.ClassEnv ->
  SMLTerm.Exp ->
  SMLTy.Type * SMLTy.Type ->
  SMLTerm.Exp option

val castClass : 
  SMLTy.ClassEnv ->
  SMLTerm.Exp ->
  SMLTy.Type * SMLTy.Type ->
  SMLTerm.Exp

end
