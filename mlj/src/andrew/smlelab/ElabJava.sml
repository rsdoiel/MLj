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
(* Type checking for Java operations     				*)
(*======================================================================*)
structure ElabJava :> ELABJAVA =
struct

local open Syntax Env EnvOps SMLTy SMLPrimTy SMLSch SMLSchOps in

(*----------------------------------------------------------------------*)
(* Return the effect (total/partial, pure/impure) of this operation 	*)
(*----------------------------------------------------------------------*)
local open Java Effect in
fun effectOf jop = 
case jop of
(* ClassCastException *)
  Cast            => throws

(* read effect (non-null assumed) *)
| GetField        => reads

| FieldRef        => none

| InstanceOf      => none

| InvokeSpecial   => any
| Invoke          => any
| InvokeInterface => any
| Pure            => allocs
| Synchronize     => any

(* <init> method might side-effect or loop *)
| New             => any

(* write effect (non-null assumed) *)
| PutField        => writes

end

(*----------------------------------------------------------------------*)
(* Check that a type is a Java class or array type. Return the type.	*)
(*----------------------------------------------------------------------*)
fun checkReference (loc,arg,ty) = 
let
  fun fail () =
    (error(Error.error(loc, "expected Java class or array type"),
      [("expression", ty)]); 
      (SMLTy.tyVarType (freshTyVar (TyVar.Normal TySort.classSort))))
in
  if SMLJavaOps.isReference ty then ty
  else fail ()
end

(*----------------------------------------------------------------------*)
(* Check that a type is a Java type. Return the type.	                *)
(*----------------------------------------------------------------------*)
fun checkJava (loc,ty) = 
let
  fun fail () =
    (error(Error.error(loc, "expected Java type"), 
      [("expression", ty)]); SMLTy.freshType ())
in
  if SMLJavaOps.isJava (fn _ => true) ty
  then ty
  else fail ()
end

(*----------------------------------------------------------------------*)
(* Java operation with effect    					*)
(*----------------------------------------------------------------------*)
fun javaTerm(jop as (joptype,_,_), args, tyopt) =
let
  val effect = 
    if (joptype = Java.Div orelse joptype = Java.Rem) 
    andalso SMLJavaOps.isFloatingPoint (valOf tyopt)
    then Effect.none else effectOf joptype
in
  (SMLTerm.Java(jop, args, tyopt, effect),
   case tyopt of NONE => unitType | SOME ty => ty)
end

(*----------------------------------------------------------------------*)
(* Generate an error and return a bogus term with fresh type.		*)
(*----------------------------------------------------------------------*)
fun javaError (message, loc) =
  (error (Error.error(loc, message), []);
  (SMLTerm.Record [], SMLTy.freshType ()))

fun javaError1 (message, loc, args) =
  (error (Error.error(loc, message), args);
  (SMLTerm.Record [], SMLTy.freshType ()))

(*----------------------------------------------------------------------*)
(* Check the types of a Java operation, returning an SML term and type.	*)
(*   loc is the position in the source of the Java op.                  *)
(*   args is a list of (loc,term,type) pairs representing the arguments *)
(*   firstsuper is set if the first argument is _super                  *)
(*----------------------------------------------------------------------*)
fun elab (C,E)
  (loc, jop as (optype : Java.OpType,tyopt,nameopt:JavaString.t option), args,
   firstsuper) =
let

val currentclassty = classofC C

fun isCurrentClass ty = 
  case currentclassty of
    NONE => false
  | SOME currentclasstyname => 
    SMLTy.eq (ty, SMLTy.consType ([], currentclasstyname))

fun isClass ty = #class (SMLTy.sort false ty)

(*......................................................................*)
(* For invoke/invokesuper/new/invokestatic perform steps as described   *)
(* in 15.11.1, Java language spec:                                      *)
(*   1. Determine Class or Interface to Search: for invoke/new, this is *)
(*      the class of the first argument, for invokesuper, it is its     *)
(*      superclass. This is done prior to checkInvoke.                  *)
(*   2. (a) Find methods that are applicable and accessible.            *)
(*      (b) Choose the Most Specific Method.                            *)
(*   3. Is the Chosen Method Appropriate? Just check it's not static.   *)
(*      Determine invocation mode.                                      *)
(*......................................................................*)
fun checkInvoke (static, classty, includeroot, name, args, make) =

(* Step 2.(a) *)
(case SMLJavaOps.getMethods E
  (isCurrentClass classty, classty, includeroot, name, map #3 args) of
  ([], []) => 
  javaError ("method " ^ JavaString.toMLString name ^  
     " not found from Java class " ^ SMLTy.toString classty, loc)

| ([], ms) =>
  let
    val argtyss = Dups.removeDups (Eq.list SMLTy.eq) 
      (map (fn (class, (_, _, argtys,_)) => argtys) ms)
    val s = Pretty.simpleVec " or "
    (fn argtys => Pretty.vec ("()", "(", ")", "(", ")", ",")
      SMLTy.toString argtys) argtyss
  in
    javaError ("method " ^ JavaString.toMLString name ^ 
      " invoked with wrong argument types " ^
      Pretty.vec ("()", "(", ")", "(", ")", ",") SMLTy.toString (map #3 args) ^
      ": try " ^ s, 
      loc)
  end

(* Step 2.(b) *)
| (m::ms, _) =>
  case SMLJavaOps.mostSpecific E (m,ms) of
    NONE =>
    javaError ("ambiguous invocation of method " ^ 
      JavaString.toMLString name, loc)

  (* Step 3. *)
  | SOME (actualclassty, (_,flags,actualtys,tyopt)) =>
    let
      val static' = List.exists (fn Method.STATIC => true | _ =>false) flags
      val private = List.exists (fn Method.PRIVATE => true | _ =>false) flags
      val ((mods,_,_),_,_) = SMLClassDefOps.tyToClassDef E actualclassty
      val interface = List.exists (fn m => m=Class.INTERFACE) mods
      val actualoptype = 
        if interface then Java.InvokeInterface
        else
        if optype = Java.InvokeSpecial orelse
         (private andalso not static') then Java.InvokeSpecial 
        else Java.Invoke
    in
      if static <> static'
      then 
        javaError("method " ^ JavaString.toMLString name ^ " is " ^
          (if static' then "static" else "not static"), loc)
      else 
        make 
        (
          actualoptype,
          actualclassty, 
          map (fn ((_,e,ty1),ty2) => valOf (SMLJavaOps.cast E e (ty1,ty2)))
              (ListPair.zip(args,actualtys)),
          tyopt
        )
    end) handle SMLClassDefOps.ClassNotFound name =>
    javaError ("class not found: " ^ name, loc)

(*......................................................................*)
(* Look for a field starting at a particular class.			*)
(*......................................................................*)
fun checkField (static, classty, name, make) =
(case SMLJavaOps.getFields E (isCurrentClass classty, classty, name) of
  [] =>
  javaError ("field " ^ JavaString.toMLString name 
    ^ " not found from Java class " ^ SMLTy.toString classty, loc)

| (_::_::_) =>
  javaError ("more than one field called " ^ JavaString.toMLString name ^ 
            " is accessible from Java class " ^ SMLTy.toString classty, 
            loc)

| [(actualclassty, (_, flags, ty, constantvalue))] =>
  let
    val static' = List.exists (fn Field.STATIC => true | _ =>false) flags
    val final = List.exists (fn Field.FINAL => true | _ =>false) flags
  in
    if static <> static'
    then 
      javaError("field " ^ JavaString.toMLString name ^ " is " ^ 
        (if static' then "static" else "not static"), loc)
    else 
      make
      (
        final,
        actualclassty,
        if not final andalso optype = Java.FieldRef 
        then SMLTy.refType ty else ty,
        constantvalue
      )
  end) handle SMLClassDefOps.ClassNotFound name =>
  javaError ("class not found: " ^ name, loc)

  fun checkAllInvoke includeroot = 
  (case tyopt of
    SOME ty => 
    if not (isClass ty) then 
      javaError1 ("expected class type in static method invocation", loc, 
        [("type specified", ty)])
    else
      checkInvoke(true, ty, includeroot, valOf nameopt, args, 
        fn (optype', actualclassty, exps, tyopt) =>
          (javaTerm((optype', SOME actualclassty, nameopt), exps, tyopt)))
    
  | NONE => 
    case args of
      (loc, arg, ty)::args =>
      if not (isClass ty) then
        javaError1 ("expected object as first argument to invoke", loc, 
          [("argument type", ty)])

      else
        checkInvoke (false, ty, includeroot, valOf nameopt, args,
          fn (optype, actualclassty, exps, tyopt) =>
            (javaTerm
              ((if firstsuper then Java.InvokeSpecial else optype, 
              NONE, nameopt), 
              SMLJavaOps.castClass E arg (ty,actualclassty) :: exps, tyopt)))

    | _ =>
      javaError ("expected at least one argument to invoke", loc)
  )

in

case optype of

(*......................................................................*)
(* For cast just check that the argument and type is Java.              *)
(*......................................................................*)
  Java.Cast =>
  (let 
    val ty2 = valOf tyopt
  in
    case args of
      [a1 as (loc1, arg1, ty1)] =>
      (checkJava (loc1, ty1); checkJava (loc1, ty2);
      (case SMLJavaOps.cast E arg1 (ty1, ty2) of
        NONE => javaError ("illegal _cast types", loc)
      | SOME result => (result, ty2))
      handle SMLClassDefOps.ClassNotFound name =>
        javaError ("class not found: " ^ name, loc))

    | _ =>
      javaError ("expected one argument to _cast", loc)
  end
  handle SMLClassDefOps.ClassNotFound name =>
      javaError ("class not found: " ^ name, loc))
  
| (Java.GetField | Java.FieldRef) =>
  (case tyopt of

(*......................................................................*)
(* For non-static getfield check that the arg is an object              *)
(* and look up the field.	                                        *)
(*......................................................................*)
    NONE =>
    (case args of
    [(loc, arg, ty)] =>
    if not (isClass ty) then
      javaError1 ("expected object as argument to field access", loc, 
        [("argument type", ty)])

    else
      checkField (false, ty, valOf nameopt,
        fn (final, actualclassty, fldty, _) =>
          (javaTerm ((if final then Java.GetField else optype, NONE, nameopt), 
           [SMLJavaOps.castClass E arg (ty, actualclassty)], SOME fldty)))

    | _ =>
      javaError ("expected one argument to getfield", loc))

(*......................................................................*)
(* For getstatic look up the field.					*)
(*......................................................................*)
  | SOME ty =>
    if null args 
    then
    if not (isClass ty) then
      javaError1 ("expected class type", loc, [("type specified", ty)])

    else
      checkField (true, ty, valOf nameopt,
        fn (final, actualclassty, ty, conval) =>
          case (final, conval) of
            (true, SOME jcon) =>
            (SMLTerm.JCon jcon, ty)

          | (true, NONE) =>
            (SMLTerm.Java((optype, SOME actualclassty, 
              nameopt), [], SOME ty, Effect.none), ty)

          | _ =>
            (javaTerm ((optype, SOME actualclassty,nameopt), [], SOME ty))
      )
    else 
      javaError ("expected zero arguments to getstatic", loc)
  )

(*......................................................................*)
(* For instanceof check that the argument is a reference type.       	*)
(*......................................................................*)
| Java.InstanceOf =>
  (case args of
    [a1 as (loc1, arg1, ty1)] =>
    
    (checkReference a1;
    if SMLJavaOps.isReference (valOf tyopt)
    then (javaTerm((optype, tyopt, NONE), [arg1], SOME boolType))
    else javaError1 ("expected reference type", loc, 
      [("type specified", valOf tyopt)])
    )

  | _ =>
    javaError ("expected one argument to _instanceof", loc))

(*......................................................................*)
(* For _pure just check that there's only one argument                  *)
(*......................................................................*)
| Java.Pure =>
  (case args of
    [a1 as (loc1, arg1, ty1)] =>
    (javaTerm((optype, NONE, NONE), [arg1], SOME ty1))

  | _ =>
    javaError ("expected one argument to _pure", loc))

(*......................................................................*)
(* For _synchronize check that the first arg is a class or ref type     *)
(*......................................................................*)
| Java.Synchronize =>
  (case args of
    [a1 as (loc1, arg1, ty1), a2 as (loc2, arg2, ty2)] =>
    if (isClass ty1) 
    then
      (javaTerm((optype, NONE, NONE), [arg1,arg2], SOME ty2))
    else
    let
      val ty = SMLTy.freshType ()
    in
      unify ((SOME loc1, "synchronize on", ty1), 
                 (NONE, "expected", SMLTy.refType ty));
      (javaTerm((optype, NONE, NONE), [arg1,arg2], SOME ty2)) 
    end

  | _ =>
    javaError ("expected two arguments to _synchronize", loc))

(*......................................................................*)
(* Static or virtual method invocation					*)
(*......................................................................*)
| Java.Invoke => checkAllInvoke true
| Java.InvokeSpecial => checkAllInvoke false

(*......................................................................*)
(* Make sure that valOf(tyopt) resolves to a Java class.	30/9/97	*)
(*......................................................................*)
| Java.New =>
  let
    val ty = valOf tyopt
  in
    if not (isClass ty) then
    javaError1 ("expected class type", loc, [("type specified", ty)])

    else
    (let val (classinfo,_,_) = SMLClassDefOps.tyToClassDef E ty
    in
      if List.exists (fn m => m=Class.ABSTRACT) (#1 classinfo)
      then javaError ("cannot instantiate abstract class", loc)
      else

      case SMLJavaOps.getConstructors E (isCurrentClass ty,ty, map #3 args) of

      ([], []) => 
      javaError ("constructor not found", loc)

    | ([], ms) =>
      let
        val argtyss = Dups.removeDups (Eq.list SMLTy.eq) 
          (map (fn (class, (_, _, argtys,_)) => argtys) ms)
        val s = Pretty.simpleVec " or "
          (fn argtys => Pretty.vec ("()", "(", ")", "(", ")", ",")
            SMLTy.toString argtys) argtyss
      in
        javaError("constructor invoked with wrong argument types " ^
          Pretty.vec ("()", "(", ")", "(", ")", ",") SMLTy.toString 
          (map #3 args) ^
          ": try " ^ s, loc)
      end

    | (m::ms, _) =>
      case SMLJavaOps.mostSpecific E (m,ms) of
        NONE =>
        javaError ("ambiguous constructor invocation", loc)

      | SOME (_,(_,flags,actualtys,_)) =>
        (javaTerm((Java.New, NONE, NONE),
          map (fn ((_,e,ty1),ty2) => valOf (SMLJavaOps.cast E e (ty1,ty2)))
            (ListPair.zip(args,actualtys)), tyopt))
    end) handle SMLClassDefOps.ClassNotFound name =>
      javaError ("class not found: " ^ name, loc)
  end

| Java.PutField =>
  (case tyopt of

(*......................................................................*)
(* For putfield check that the first arg is an object and that the type *)
(* of the second arg matches its definition.	                        *)
(*......................................................................*)
    NONE =>
    (case args of
      [a1 as (loc1,arg1,ty1), a2 as (loc2, arg2, ty2)] =>
      if not (isClass ty1) then
        javaError1 ("expected object as first argument to putfield", loc,
          [("argument type", ty1)])

      else
        checkField (false, ty1, valOf nameopt,
          fn (final, actualclassty, fldty, _) =>
            if final 
            then javaError ("field is final", loc)
            else
            if SMLJavaOps.methodConv E true (ty2, fldty)
            then 
              (javaTerm((optype, NONE, nameopt), 
                [SMLJavaOps.castClass E arg1 (ty1,actualclassty),
                 valOf(SMLJavaOps.cast E arg2 (ty2,fldty))], NONE))
            else
              javaError1 ("argument has wrong type for field", loc,
                [("argument type", ty2), ("field type", fldty)])
      )

    | _ =>
      javaError ("expected two arguments to putfield", loc)
    )

(*......................................................................*)
(* For putstatic check that the argument type matches the definition.   *)
(*......................................................................*)
  | SOME ty =>
    (case args of
      [a1 as (loc1,arg1,ty1)] =>
      if not (isClass ty) then
        javaError1 ("expected class type", loc, [("type specified", ty)])

      else
        checkField (true, ty, valOf nameopt,
          fn (final, actualclassty, fldty, _) =>
            if final 
            then javaError ("field is final", loc)
            else
            if SMLJavaOps.methodConv E true (ty1, fldty)
            then 
              (javaTerm((optype, SOME actualclassty, nameopt), 
                [valOf(SMLJavaOps.cast E arg1 (ty1,fldty))], NONE))
            else
              javaError1 ("argument has wrong type for field", loc,
                [("argument type", ty1), ("field type", fldty)])
        
      )

    | _ => 
      javaError ("expected one argument to putstatic", loc)
    )
  )

| Java.This =>
  (case EnvLookup.lookupVid' (EofC C, loc, [Ids.thisSym]) of
    SOME (ValBind.VarSch(SMLSch.TypeScheme(_, ty)), longid) =>
    (SMLTerm.Var ((Ids.thisSym, []), []), ty)

  | _ =>
    javaError ("_this only valid in a virtual method definition", loc)
  )

| Java.Super =>
  (case EnvLookup.lookupVid' (EofC C, loc, [Ids.superSym]) of
    SOME (ValBind.VarSch(SMLSch.TypeScheme(_, ty)), longid) =>
    (SMLTerm.Java((Java.NopCast, NONE, NONE), 
      [SMLTerm.Var ((Ids.thisSym, []), [])], SOME ty, Effect.none), ty)

  | _ =>
    javaError ("_super only valid in a virtual method definition", loc)
  )

| _ =>
  javaError ("Java operation not implemented yet", loc)

end (* of let *)

end (* of local *)

end (* of struct *)
