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
(* See the signature for more details.                                  *)
(*======================================================================*)
structure TyStr :> TYSTR =
struct

datatype TyStr = 
  Datatype of bool * SMLTy.DatDef
| Concrete of TyVar.TyVar list * SMLTy.Type
| Abstract of TyVar.TyVar list * TyName.TyName
| ClassType of TyName.TyName * SMLTy.ClassDef

structure FieldSet = SetFn(
  struct
    type ord_key = SMLTy.FieldInfo
    fun compare ((name1,_,ty1,_),(name2,_,ty2,_)) =
      case JavaString.compare(name1,name2) of
        EQUAL => SMLTy.compare (ty1,ty2)
      | other => other
  end)

structure MethodSet = SetFn(
  struct
    type ord_key = SMLTy.MethodInfo
    fun compare ((name1,_,tys1,tyopt1),(name2,_,tys2,tyopt2)) =
      case JavaString.compare(name1,name2) of
        EQUAL => 
        (case Compare.option SMLTy.compare (tyopt1,tyopt2) of
          EQUAL => 
          Compare.list SMLTy.compare (tys1,tys2)
        | other => other)
      | other => other
  end)

(*----------------------------------------------------------------------*)
(* Constructors								*)
(*----------------------------------------------------------------------*)
val makeConcrete = Concrete
val makeAbstract = Abstract
val makeDatatype = Datatype
val makeClassType= ClassType

(*----------------------------------------------------------------------*)
(* Arity (number of type parameters) of a type structure.	        *)
(*----------------------------------------------------------------------*)
fun arity (Datatype (_, (tyvars, _, _))) = length tyvars
  | arity (Concrete (tyvars, _)) = length tyvars
  | arity (Abstract (tyvars, _)) = length tyvars
  | arity (ClassType _) = 0

(*----------------------------------------------------------------------*)
(* Type names in a type structure					*)
(*----------------------------------------------------------------------*)
fun tynames (Datatype(_, (_,tyname,CE))) =
    Symbol.OrdMap.foldr 
    (fn (NONE,T) => T | (SOME ty,T) => TyName.Set.union(SMLTy.tynames ty, T)) 
    (TyName.Set.singleton tyname) CE
  | tynames (Concrete(_,ty)) = SMLTy.tynames ty
  | tynames (Abstract(_,tyname)) = TyName.Set.singleton tyname
  | tynames (ClassType (tyname, ((_,tyopt,tys), fields, methods))) =    
    foldr (fn (ty,T) => TyName.Set.union(SMLTy.tynames ty, T))    
    (foldr (fn ((_,_,ty,_),T) => TyName.Set.union(SMLTy.tynames ty, T))
    (foldr (fn ((_,_,tys,tyopt),T) => 
       foldr (fn (ty,T) => TyName.Set.union(SMLTy.tynames ty, T)) T
       (tys @ Gen.optToList tyopt)) (TyName.Set.singleton tyname) methods) 
         fields)
    (tys @ Gen.optToList tyopt)

(*----------------------------------------------------------------------*)
(* Apply a tyname renaming to a type structure.				*)
(*----------------------------------------------------------------------*)
fun rename r tystr =
case tystr of
  Datatype(isrec, (tyvars, tyname, CE)) => 
  Datatype(isrec, (tyvars, TyName.rename r tyname,
    Symbol.OrdMap.map (Option.map (SMLTy.renameType r)) CE))

| Concrete(tyvars, ty) =>
  Concrete(tyvars, SMLTy.renameType r ty)

| Abstract(tyvars, tyname) =>
  Abstract(tyvars, TyName.rename r tyname)

| ClassType(tyname, ((flags,tyopt,tys), fields, methods)) =>
  ClassType(TyName.rename r tyname, (
    (flags,Option.map (SMLTy.renameType r) tyopt,map (SMLTy.renameType r) tys),
    map (fn (n,m,ty,c) => (n,m,SMLTy.renameType r ty,c)) fields,
    map (fn (n,m,tys,tyopt) => (n,m,map (SMLTy.renameType r) tys,
      Option.map (SMLTy.renameType r) tyopt)) methods))
  

(*----------------------------------------------------------------------*)
(* Apply a realisation to a type structure. This may turn an abstract	*)
(* type into a concrete one; on datatypes it can lead to an ill-formed  *)
(* tystr.                                                               *)
(*----------------------------------------------------------------------*)
fun appRealisation psi tystr =
case tystr of
  Abstract(tyvars, tyname) =>
  (case TyName.Map.find(psi, tyname) of
    NONE => tystr
  | SOME (tyvars, ty) =>
    Concrete(tyvars, ty))

| Concrete(tyvars, ty) =>
  Concrete(tyvars, SMLTy.appRealisation psi ty)

| Datatype(isrec, (tyvars, tyname, CE)) =>
  Datatype(isrec, (tyvars, 
    case TyName.Map.find(psi, tyname) of
      NONE => tyname
    | SOME (tyvars, ty) =>
      case SMLTy.fromConsType ty of
        NONE => raise Fail "TyStr.appRealisation: ill-formed tystr"
      | SOME (_, tyname') => tyname',
    Symbol.OrdMap.map (Option.map (SMLTy.appRealisation psi)) CE))

| ClassType(tyname, ((flags,tyopt,tys), fields, methods)) =>
  ClassType(
    case TyName.Map.find(psi, tyname) of
      NONE => tyname
    | SOME (tyvars, ty) =>
      case SMLTy.fromConsType ty of
        NONE => raise Fail "TyStr.appRealisation: ill-formed tystr"
      | SOME (_, tyname') => tyname',
    ((flags,Option.map (SMLTy.appRealisation psi) tyopt, 
      map (SMLTy.appRealisation psi) tys),
    map (fn (n,m,ty,c) => (n,m,SMLTy.appRealisation psi ty,c)) fields,
    map (fn (n,m,tys,tyopt) => (n,m,map (SMLTy.appRealisation psi) tys,
      Option.map (SMLTy.appRealisation psi) tyopt)) methods))
  

(*----------------------------------------------------------------------*)
(* Apply a type structure to some type parameters. Arities are not	*)
(* checked.                                                             *)
(*----------------------------------------------------------------------*)
fun apply (tystr, tys) = 
case tystr of
  Datatype(isrec, (tyvars, tyname, CE)) => 
  SMLTy.consType(tys, tyname)

| Concrete(tyvars, ty) =>
  SMLTy.appSubst (ListPair.zip (tyvars, tys)) ty

| Abstract(tyvars, tyname) =>
  SMLTy.consType(tys, tyname)

| ClassType (tyname, classdef) =>
  SMLTy.consType([], tyname)

(*----------------------------------------------------------------------*)
(* Pretty-print.							*)
(*----------------------------------------------------------------------*)
fun toString tystr =
case tystr of
  Datatype(isrec, (tyvars, tyname, CE)) =>
  TyName.toString tyname ^ ": " ^
  Pretty.vec("", "{", "}", "{", "}", " | ")
  (fn (c,tyopt) => JavaString.toMLString (Symbol.toJavaString c) ^ 
  (case tyopt of NONE => "" | SOME ty => " of " ^ SMLTy.toString ty)) 
  (Symbol.OrdMap.listItemsi CE)

| Concrete(tyvars, ty) =>
  Pretty.vec("", "/\\", ".", "/\\", ".", ",") TyVar.toString tyvars ^
  SMLTy.toString ty

| Abstract(tyvars, tyname) =>
  TyName.toString tyname

| ClassType(tyname,_) =>
  TyName.toString tyname


(*----------------------------------------------------------------------*)
(* Equality								*)
(* Rather strict at present: no alpha-conversion allowed.               *)
(* This function is used only to determine if recompilation is required *)
(* so it must return true if the tystrs are equal but can return false  *)
(* if it's uncertain.                                                   *)
(*----------------------------------------------------------------------*)
fun eq (Datatype(_, (tyvars1, tyname1, CE1)), 
        Datatype(_, (tyvars2, tyname2, CE2))) =
    Eq.list TyVar.eq (tyvars1, tyvars2) andalso
    TyName.eq (tyname1, tyname2) andalso
    Eq.list (fn ((id1, tyopt1), (id2, tyopt2)) => Symbol.equal(id1,id2)
             andalso Eq.option SMLTy.eq (tyopt1, tyopt2)) 
    (Symbol.OrdMap.listItemsi CE1, Symbol.OrdMap.listItemsi CE2)

  | eq (Concrete(tyvars1, ty1), Concrete(tyvars2, ty2)) =
    Eq.list TyVar.eq (tyvars1, tyvars2) andalso
    SMLTy.eq (ty1, ty2)

  | eq (Abstract(tyvars1, tyname1), Abstract(tyvars2, tyname2)) =
    Eq.list TyVar.eq (tyvars1, tyvars2) andalso
    TyName.eq(tyname1, tyname2)

(*
  | eq (ClassType(tyname1, ((flags1,tyopt1,tys1),fields1,methods1)),
        ClassType(tyname2, ((flags2,tyopt2,tys2),fields2,methods2))) =
    TyName.eq (tyname1, tyname2) andalso
    Eq.list op= (flags1,flags2) andalso
    Eq.list SMLTy.eq (tys1,tys2) andalso
    Eq.opt SMLTy.eq (tyopt1,tyopt2) andalso
    Eq.list (
*)

  | eq _ =
    false

(*----------------------------------------------------------------------*)
(* Deconstructors       						*)
(*----------------------------------------------------------------------*)
fun fromAbstract tystr =
case tystr of
  Abstract a => SOME a
| _ => NONE

fun fromConcrete tystr =
case tystr of
  Concrete a => SOME a
| _ => NONE

fun fromDatatype tystr =
case tystr of
  Datatype a => SOME a
| _ => NONE

fun fromClassType tystr =
case tystr of
  ClassType c => SOME c
| _ => NONE

datatype MatchResult = 
  Success of SMLTy.Realisation
| Failure of string

(*----------------------------------------------------------------------*)
(* First stage of signature matching: determine a realisation psi given *)
(* the type structures from the structure and signature.                *)
(*----------------------------------------------------------------------*)
local 
  open SMLTy 
in

fun match1 psi (strTyStr, sigTyStr) =
(*......................................................................*)
(* First check the arities: these *must* be the same 			*)
(*......................................................................*)
  if arity strTyStr <> arity sigTyStr 
  then Failure 
  "Mismatch between number of type parameters specified in structure and \
  \signature"
  else 

  case (strTyStr, sigTyStr) of

(*......................................................................*)
(* Both specify datatypes: assign one tyname to the other.		*)
(* HACK: don't do this if they are equal (for bootstrapping lists, etc.)*)
(*......................................................................*)
  (Datatype(_, (strtvs, strTyName, strCE)), 
   Datatype(_, (_, sigTyName, sigCE))) =>
  Success (if TyName.eq(sigTyName, strTyName) 
  then psi
  else TyName.Map.insert(psi, sigTyName, 
         (strtvs, consType(map tyVarType strtvs, strTyName))))

(*......................................................................*)
(* Structure specifies a datatype, signature leaves it abstract.	*)
(* Assign one tyname to the other.                                      *)
(*......................................................................*)
| (Datatype(_, (strtvs, strTyName, strCE)), Abstract(_, sigTyName)) =>
  Success (TyName.Map.insert(psi, sigTyName, 
    (strtvs, consType(map tyVarType strtvs, strTyName))))

(*......................................................................*)
(* Structure specifies a datatype, signature gives a type defn. Error!	*)
(*......................................................................*)
| (Datatype(_, (strtvs, strTyName, strCE)), Concrete (_, ty)) =>
  (case SMLTy.fromConsType ty of
    SOME (_, sigTyName) => 
    Success (if TyName.eq(sigTyName, strTyName) 
    then psi
    else TyName.Map.insert(psi, sigTyName, 
           (strtvs, consType(map tyVarType strtvs, strTyName))))
  | NONE => 
    Failure 
    "Structure specifies datatype but signature specifies non-datatype type")
  
(*......................................................................*)
(* Both specify concrete types. Defer check until enrichment time.      *)
(*......................................................................*)
| (Concrete(strtyvars, strTy), Concrete(sigtyvars, sigTy)) =>
  Success psi
    
(*......................................................................*)
(* Structure specifies concrete type, signature leaves it abstract.	*)
(*......................................................................*)
| (Concrete(strtyvars, strTy), Abstract(sigArity, sigTyName)) =>
  Success (TyName.Map.insert(psi, sigTyName, (strtyvars, strTy)))

(*......................................................................*)
(* Structure specifies concrete type, signature gives datatype.       	*)
(*......................................................................*)
| (Concrete(strtyvars, strTy), Datatype(_, (sigtyvars, sigTyName, _))) =>
  Failure 
    "Structure specifies concrete type but signature specifies datatype"

(*......................................................................*)
(* Structure specifies abstype, signature also leaves it abstract. 	*)
(*......................................................................*)
| (Abstract(strtvs, strTyName), Abstract(sigtvs, sigTyName)) =>
  Success (TyName.Map.insert(psi, sigTyName, 
    (strtvs, consType(map tyVarType strtvs, strTyName))))

(*......................................................................*)
(* Structure specifies abstype, signature gives concrete type. Error!	*)
(*......................................................................*)
| (Abstract _, Concrete _) =>
  Failure "Structure specifies abstype but signature specifies type"

(*......................................................................*)
(* Structure specifies abstype, signature gives datatype. Error!        *)
(*......................................................................*)
| (Abstract _, Datatype _) =>
  Failure "Structure specifies abstype but signature specifies datatype"

(*......................................................................*)
(* Structure specifies classtype, signature leaves it abstract.		*)
(*......................................................................*)
| (ClassType (strTyName, _), Abstract(sigArity, sigTyName)) =>
  Success (TyName.Map.insert(psi, sigTyName, ([], 
    SMLTy.consType([], strTyName))))

| (Datatype _, ClassType _) =>
  Failure "Structure specifies datatype but signature specifies class type"

| (Abstract _, ClassType _) =>
  Failure "Structure specifies abstype but signature specifies class type"

| (ClassType (strTyName, strClassDef), ClassType (sigTyName, sigClassDef)) =>
  Success (if TyName.eq(strTyName, sigTyName)
  then psi
  else TyName.Map.insert(psi, sigTyName, ([], SMLTy.consType([], strTyName))))

| (ClassType _, Datatype _) =>
  Failure "Structure specifies class type but signature specifies datatype"

| (Concrete _, ClassType _) =>
  Failure "Structure specifies type but signature specifies class type"

| (ClassType _, Concrete _) =>
  Failure "Structure specifies class type but signature specifies type"
    
(*----------------------------------------------------------------------*)
(* Second stage of signature matching: enrichment.                      *)
(*----------------------------------------------------------------------*)
fun match2 (strTyStr, sigTyStr) =
  case (strTyStr, sigTyStr) of

(*......................................................................*)
(* Both specify a datatype.						*)
(* Just check domains of constructor environments: actual types will be *)
(* checked anyway in value environments.                                *)
(*......................................................................*)
  (Datatype(_, (strtvs, strTyName, strCE)), 
   Datatype(_, (_, sigTyName, sigCE))) =>
  let
    val strDom = Symbol.OrdSet.addList(Symbol.OrdSet.empty,
      map #1 (Symbol.OrdMap.listItemsi strCE))
    val sigDom = Symbol.OrdSet.addList(Symbol.OrdSet.empty,
      map #1 (Symbol.OrdMap.listItemsi sigCE))
    val sigExtra = Symbol.OrdSet.difference (sigDom, strDom)
    val strExtra = Symbol.OrdSet.difference (strDom, sigDom)
    val sigExtraPresent = not (Symbol.OrdSet.isEmpty sigExtra)
    val strExtraPresent = not (Symbol.OrdSet.isEmpty strExtra)
  in
    if sigExtraPresent andalso strExtraPresent
    then SOME "datatype does not match specification"
    else
    if strExtraPresent
    then SOME (
      "datatype constructors " ^ Pretty.simpleVec ","
      (JavaString.toMLString o Symbol.toJavaString) 
      (Symbol.OrdSet.listItems strExtra) ^ " not present in signature")
    else
    if sigExtraPresent
    then SOME (
      "specified datatype constructors " ^ Pretty.simpleVec ","
      (JavaString.toMLString o Symbol.toJavaString)
      (Symbol.OrdSet.listItems sigExtra) ^ " not present in structure")
    else NONE
  end

(*......................................................................*)
(* Both specify concrete types. Check that they're the same.            *)
(*......................................................................*)
| (Concrete(strtyvars, strty), Concrete(sigtyvars, sigty)) =>
  if eq (strty, 
         appSubst (ListPair.zip(sigtyvars, map tyVarType strtyvars)) sigty)
  then NONE
  else SOME "type does not match specification"


(*......................................................................*)
(* Both specify class types. Check that they're the same when private   *)
(* fields and methods have been removed.                                *)
(*......................................................................*)
| (ClassType(strtyname, ((strflags,strsuper,strints), strfields, strmethods)),
   ClassType(sigtyname, ((sigflags,sigsuper,sigints), sigfields, sigmethods)))
  =>
  if not (Eq.unordered op= (strflags, sigflags))
  then SOME "class modifiers do not match specification in signature"
  else 
  if not (Eq.option SMLTy.eq (strsuper,sigsuper))
  then SOME "superclass does not match specification in signature"
  else 
  if not (Eq.unordered SMLTy.eq (strints, sigints))
  then SOME "superinterfaces do not match specification in signature"
  else 
  let
    val strfieldset = FieldSet.addList(FieldSet.empty, strfields)
    val sigfieldset = FieldSet.addList(FieldSet.empty, sigfields)
    val strmethodset = MethodSet.addList(MethodSet.empty, strmethods)
    val sigmethodset = MethodSet.addList(MethodSet.empty, sigmethods)
    fun checkFieldMods ([],[]) = NONE
      | checkFieldMods ((name,flags1,_,_)::fields1,(_,flags2,_,_)::fields2) =
        if not (Eq.unordered op= (flags1, flags2))
        then SOME ("field modifiers for \"" ^ 
          valOf (JavaString.toString name) ^ 
          "\" do not match specification in signature")
        else checkFieldMods (fields1,fields2)

    fun checkMethodMods ([],[]) = checkFieldMods 
      (FieldSet.listItems strfieldset, FieldSet.listItems sigfieldset)
      | checkMethodMods ((name,flags1,_,_)::methods1,(_,flags2,_,_)::methods2)=
        if not (Eq.unordered op= (flags1, flags2))
        then SOME ("method modifiers for \"" ^ 
          valOf(JavaString.toString name) ^ 
          "\" do not match specification in signature")
        else checkMethodMods (methods1,methods2)
  in
    if not (FieldSet.equal (strfieldset,sigfieldset))
    then SOME "fields do not match specification in signature"
    else if not (MethodSet.equal (strmethodset,sigmethodset))
    then SOME "methods do not match specification in signature"
    else checkMethodMods 
      (MethodSet.listItems strmethodset, MethodSet.listItems sigmethodset)
  end
    
| _ =>
  NONE    

end (* of local open *)

end
