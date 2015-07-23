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
(* Type check Java _classtype and _classexception declarations.		*)
(*======================================================================*)
structure ElabJavaDec :> ELABJAVADEC =
struct

structure T = SMLTerm

local open 
  Syntax ElabOps Env EnvOps SMLTy SMLPrimTy ElabTy ElabPat 
  ElabCore
in

structure Map = Symbol.OrdMap
structure JSMap = MapFn(
  struct
    type ord_key = JavaString.t
    val compare = JavaString.compare
  end)

fun checkClass loc ty =
  if #class (SMLTy.sort false ty) then ()
  else
    error(Error.error(loc, "expected class type"), [])

fun mem modifier mods = List.exists (fn m => modifier=m) mods

fun convFieldMod m =
case m of
  JavaFlags.PUBLIC => Field.PUBLIC
| JavaFlags.PRIVATE => Field.PRIVATE
| JavaFlags.PROTECTED => Field.PROTECTED
| JavaFlags.STATIC => Field.STATIC
| JavaFlags.FINAL => Field.FINAL
| JavaFlags.VOLATILE => Field.VOLATILE
| JavaFlags.TRANSIENT => Field.TRANSIENT

fun convMethMod m =
case m of
  JavaFlags.PUBLIC => Method.PUBLIC
| JavaFlags.PRIVATE => Method.PRIVATE
| JavaFlags.PROTECTED => Method.PROTECTED
| JavaFlags.STATIC => Method.STATIC
| JavaFlags.FINAL => Method.FINAL
| JavaFlags.SYNCHRONIZED => Method.SYNCHRONIZED
| JavaFlags.ABSTRACT => Method.ABSTRACT

fun convClassMod m =
case m of
  JavaFlags.PUBLIC => Class.PUBLIC
| JavaFlags.FINAL => Class.FINAL
| JavaFlags.INTERFACE => Class.INTERFACE
| JavaFlags.ABSTRACT => Class.ABSTRACT

fun infMonoTy C (loc,tyexp) =
  let
    val ty = infTy C (loc,tyexp)
    val tyvars = TyVar.Set.filter TyVar.isExplicit (SMLTy.tyvars ty)
  in 
    if TyVar.Set.isEmpty tyvars
    then ty
    else (error(Error.error(loc, "expected monomorphic type"), []); ty)
  end

fun isAbstractClass flags =
  List.exists (fn m => m=Class.ABSTRACT orelse m=Class.INTERFACE) flags

(*----------------------------------------------------------------------*)
(* Check validity of method and field types.				*)
(* (1) If more than one method has the same name and no. of args, then: *)
(*     (a) they must all have Java types for args and result; and       *)
(*     (b) the Java arg types must be distinct (after option removal).  *)
(*         [8.4, JLS]                                                   *)
(* (2) Field names must be distinct. [8.3, JLS]                         *)
(*----------------------------------------------------------------------*)
fun checkClassItemsTys loc 
    (FE:SMLTy.FieldInfo list, ME:SMLTy.MethodInfo list) =
let
  fun gatherMethods ((name,modifiers,argtys,restyopt), m) =
  case JSMap.find(m, name) of
    NONE => 
    JSMap.insert(m, name, IMap.insert(IMap.empty, length argtys,
      [(argtys, restyopt)]))
  | SOME im =>
    case IMap.find(im, length argtys) of
      NONE =>
      JSMap.insert(m, name, 
        IMap.insert(im, length argtys, [(argtys, restyopt)]))

    | SOME ms =>
      JSMap.insert(m, name, 
        IMap.insert(im, length argtys,(argtys,restyopt)::ms))

  val m = foldl gatherMethods JSMap.empty ME

  fun checkMethods [] = ()
    | checkMethods ((name, im)::m) =
      let
        fun checkMethods' [] = checkMethods m 
          | checkMethods' ((numargs, sigs)::im) =
            if length sigs <= 1 then checkMethods' im
            else
            let
              fun checkSigs argtyss [] = ()
                | checkSigs argtyss ((argtys,restyopt)::rest) =
                  case Option.map SMLJavaOps.fromJava restyopt of
                    SOME NONE => 
                    error (Error.error(loc, 
                      "multiple methods with name " ^  
                       JavaString.toMLString name ^ 
                      " do not all have Java types"), [])

                | _ =>            
                  let 
                    val argtys1 = List.mapPartial SMLJavaOps.fromJava argtys
                  in
                    if length argtys1 <> numargs
                    then error (Error.error(loc,
                      "multiple methods with name " ^ 
                       JavaString.toMLString name ^
                      " do not all have Java types"), [])
                    else
                      if List.exists (fn argtys2 => 
                        Eq.list SMLTy.eq (argtys1,argtys2)) argtyss
                      then error (Error.error(loc,
                        "multiple methods with name " ^ 
                        JavaString.toMLString name ^ 
                        " have same signature"), [])
                      else checkSigs (argtys1::argtyss) rest
                  end
            in
              checkSigs [] sigs;
              checkMethods' im
            end
     
      in
        checkMethods' (IMap.listItemsi im)
      end                        
in
  (case Dups.duplicates JavaString.equal (map #1 FE) of
    [] => ()
  | dups => error(Error.error(loc, 
    "duplicate field names: " 
    ^ Pretty.simpleVec "," JavaString.toMLString dups), []));
  checkMethods (JSMap.listItemsi m)
end
  

(*----------------------------------------------------------------------*)
(* Check validity of inheritance.                    			*)
(* First, superclasses [8.1.3, JLS].                                    *)
(* (a) If superclass is not public then it must be in the same package. *)
(* (b) The superclass must not be final.                                *)
(* Note: it's not possible for a class to subclass itself because (in   *)
(* contrast to Java) we don't type check several classes simultaneously.*)
(*                                                                      *)
(* Second, superinterfaces [8.1.4, JLS].                                *)
(* (a) If any interface is not public then it must be in the same       *)
(*     package.                                                         *)
(* (b) Superinterfaces must be distinct.                                *)
(*                                                                      *)
(* Third, overridden/hidden methods [8.4.6.1].                          *)
(* (a) Types of arguments and result must match overridden/hidden       *)
(*     method.                                                          *)
(* (b) Static method cannot hide non-static method.                     *)
(* (c) Non-static method cannot override static method.                 *)
(* (d) Disallow overriding methods in signatures.                       *) 
(*                                                                      *)
(* Return non-overriding methods in ME.                                 *)
(*----------------------------------------------------------------------*)
fun checkInherit (C,E,loc,insig) 
  (classty,
   classdef as (classinfo as (flags,superopt,ints) : SMLTy.ClassInfo, 
   FE : SMLTy.FieldInfo list, 
   ME : SMLTy.MethodInfo list)) =

let
  val (inheritedfields, inheritedmethods) = SMLJavaOps.getInherited E
    (classty, false, fn _ => true, fn _ => true)

  fun checkMethod (methodinfo as (name, methmods, argtys, restyopt)) =
  let
    val numargs = length argtys
    val isExportableMethod = 
      List.all (SMLJavaOps.isExportable (fn _ => true))
      (argtys @ Gen.optToList restyopt)

    val inheritedmethods = 
      List.filter (fn (_, (name', _, argtys', _)) => 
        JavaString.equal(name,name') andalso length argtys' = numargs) 
      inheritedmethods

    val doinclude =
    if not isExportableMethod
    then 
      if null inheritedmethods then true
      else 
        (error (Error.error(loc, 
          "possibly-overriding method has non-exportable type: " ^ 
          JavaString.toMLString name), []); true)
    else 
    let
      val inheritedmethods = 
        List.filter (fn (_, (_, _, argtys', _)) =>
          Eq.list SMLTy.eq (argtys, argtys')) inheritedmethods
    in
      case inheritedmethods of
        [] => true
      | (_,(name', methmods', argtys', restyopt'))::_ =>
        if not (Eq.list SMLTy.eq (argtys,argtys'))
        then 
          (error (Error.error(loc, 
            "overriding method has wrong argument types: " ^ 
            JavaString.toMLString name), []); true)
        else 
        if not (Eq.option SMLTy.eq (restyopt, restyopt'))
        then 
          (error (Error.error(loc, 
            "overriding method has wrong result type: " ^ 
            JavaString.toMLString name), []); true)
        else 
        let
          val static = mem Method.STATIC methmods
          val static' = mem Method.STATIC methmods'
          val final' = mem Method.FINAL methmods'
        in
          if final' 
          then 
            (error (Error.error(loc,
              "final method cannot be overridden: " ^ 
              JavaString.toMLString name), []); true)
          else
          if static<>static'
          then (error (Error.error(loc,
          (if static 
          then "static method cannot hide non-static (instance) method: "
          else "non-static (instance) method cannot override static method: ")
          ^ JavaString.toMLString name), []); true)

          else 
          let
            val public = mem Method.PUBLIC methmods
            val public' = mem Method.PUBLIC methmods'
            val protected = mem Method.PROTECTED methmods
            val protected' = mem Method.PROTECTED methmods'
            val private = mem Method.PRIVATE methmods
          in
            if public' andalso not public
            then (error (Error.error(loc,
              "non-public method cannot override public method: " 
              ^ JavaString.toMLString name), []); true)
            else if protected' andalso not (protected orelse public)
            then (error (Error.error(loc,
              "non-public/protected method cannot override protected method: " 
              ^ JavaString.toMLString name), []); true)
            else if private andalso not (public' orelse protected')
            then (error (Error.error(loc,
              "private method cannot override non-public/protected method: " 
              ^ JavaString.toMLString name), []); true)
            else if insig andalso not static
            then (error (Error.error(loc,
              "overriding method not permitted in signature: " ^ 
              JavaString.toMLString name), []); true)
            else static
          end
        end
      end
  in
    if doinclude andalso not (List.exists (fn m => m=Method.PRIVATE) methmods)
    then (SOME methodinfo) else NONE
  end

  fun checkSuper () =
  case superopt of
    NONE => 
    ()

  | SOME superclassty =>
    let
      val (sclassinfo as (flags,_,_), sFE, sME) = 
        SMLClassDefOps.tyToClassDef E superclassty
    in
      if mem Class.INTERFACE flags      
      then error (Error.error(loc, 
        "specified superclass is an interface type"), [])
      else if not (mem Class.PUBLIC flags
          orelse Eq.list Symbol.equal
            (SMLJavaOps.package classty, SMLJavaOps.package superclassty))
      then error (Error.error(loc, 
          "non-public superclass not accessible"), [])
      else 
      if mem Class.FINAL flags
      then error (Error.error(loc, "superclass is final"), [])
      else ()
    end

  fun checkInt intty =
    let
      val (intinfo as (flags,_,_), iFE, iME) = 
        SMLClassDefOps.tyToClassDef E intty
    in
      if not (mem Class.INTERFACE flags)
      then error (Error.error(loc, 
        "specified superinterface is a class type"), [])
      else if not (mem Class.PUBLIC flags
        orelse Eq.list Symbol.equal
          (SMLJavaOps.package classty, SMLJavaOps.package intty))
      then error (Error.error(loc, 
          "non-public superinterface not accessible"), [])
      else ()
    end

  fun checkInts () =
    ((case Dups.duplicates SMLTy.eq ints of
        [] => ()
      | dups => error(Error.error(loc, "duplicate superinterfaces: " ^ 
          Pretty.simpleVec "," SMLTy.toString dups), []));
    map checkInt ints;
    checkSuper ())

(*......................................................................*)
(* Crummy repeat of getInherited just to see if there's any abstract	*)
(* methods left in a non-abstract class.                                *)
(*......................................................................*)
  fun checkAbstract () =
    if isAbstractClass flags then ()
    else  
    let
      val (inheritedfields, inheritedmethods) = SMLJavaOps.getInherited E
      (classty, true, fn _ => false, fn _ => true)
      fun checkMethod (classty',(name, methmods, argtys, restyopt)) =
        if mem Method.ABSTRACT methmods 
          andalso not (SMLTy.eq(classty,classty'))
        then error (Error.error(loc, 
          "abstract method not implemented: " ^ JavaString.toMLString name),[])
        else ()
    in
      app checkMethod inheritedmethods
    end

  val MEopts = map checkMethod ME
in
  checkInts ();
  checkAbstract ();
  (List.filter
    (fn (_,mods,_,_) => not (List.exists (fn m => m=Field.PRIVATE) mods))
    FE, List.mapPartial Gen.id MEopts)
end


(*----------------------------------------------------------------------*)
(* Elaborate the _type_ part of Java field and method definitions.      *)
(* Also check:                                                          *)
(* (1) Abstract methods may only appear in abstract classes.            *)
(*----------------------------------------------------------------------*)
fun infClassItemsTys (triple as (C,loc,classmods)) items =
case items of
  [] => 
  ([], [])
 
| ((loc, Field { modifiers, name, ty = tyexp, initial = expopt })::items) =>
  let
    val ty = infMonoTy C tyexp
    val (FE, ME) = infClassItemsTys triple items
  in
    ((name, map convFieldMod modifiers, ty, NONE)::FE, ME)
  end

| ((loc, Method { modifiers, name, args, result, body })::items) =>
  (
    if mem JavaFlags.ABSTRACT modifiers andalso not (isAbstractClass classmods)
    then error (Error.error(loc, 
      "abstract method must belong to abstract class"), [])
    else ();
    let
      val argtys = map (infMonoTy C o #2) args
      val restyopt = Option.map (infMonoTy C) result
      val (FE, ME) = infClassItemsTys triple items
    in
      (FE, (name, map convMethMod modifiers, argtys, 
        case restyopt of
          NONE => NONE
        | SOME resty => 
          if SMLTy.eq (resty, unitType) then NONE else SOME resty)::ME)
    end
  )

| ((loc, Constructor { modifiers, args, inits, body })::items) =>
  let
    val argtys = map (infMonoTy C o #2) args
    val (FE, ME) = infClassItemsTys triple items
  in
    (FE, (JavaString.fromString "<init>", 
    map convMethMod modifiers, argtys, NONE)::ME)
  end

(*----------------------------------------------------------------------*)
(* Elaborate a constructor invocation expression			*)
(*   isSuper is true if this is a _super invocation                     *)
(*   class is the class in which to search for <init> methods           *)
(*   args is a list of source expressions that are the arguments        *)
(*----------------------------------------------------------------------*)
fun infConInvoc (C, E, loc) (isSuper, classty, args) =
  (* Type check the arguments *)
  let
    val pairs = map (infExp C) args
    val (argterms, argtys) = ListPair.unzip pairs

    (* Actually make the invoke: for _super cast the first argument *)
    fun makeInvoke exps =
    let 
      val v = SMLTerm.Var ((Ids.thisSym, []), [])
      val thisOrSuper = 
        if isSuper 
        then SMLTerm.Java((Java.NopCast, NONE, NONE), [v], 
          SOME classty, Effect.none)
        else v
    in
      SMLTerm.Val(loc, [], unitType, SMLTerm.PatWild,
        SMLTerm.Java((Java.InvokeSpecial, NONE, 
        SOME (JavaString.fromString "<init>")),
        thisOrSuper :: exps, NONE, Effect.any))
    end
  in
    (case SMLJavaOps.getConstructors E (not isSuper, classty, argtys) of

      ([], []) => 
      (error (Error.error(loc, "constructor not found"), []); [])

    | ([], ms) =>
      let
        val argtyss = Dups.removeDups (Eq.list SMLTy.eq) 
          (map (fn (class, (_, _, argtys,_)) => argtys) ms)
        val s = Pretty.simpleVec " or "
          (fn argtys => Pretty.vec ("()", "(", ")", "(", ")", ",")
            SMLTy.toString argtys) argtyss
      in
        (error (Error.error(loc, 
          "constructor invoked with wrong argument types " ^
          Pretty.vec ("()", "(", ")", "(", ")", ",") SMLTy.toString argtys ^
          ": try " ^ s), []); [makeInvoke []])
      end

    | (m::ms, _) =>
      case SMLJavaOps.mostSpecific E (m,ms) of
        NONE =>
        (error (Error.error(loc, 
          "ambiguous constructor invocation"), []); [])

      | SOME (_,(_,flags,actualtys,tyopt)) =>
        [makeInvoke (ListPair.map 
            (fn ((e,ty1),ty2) => valOf (SMLJavaOps.cast E e (ty1,ty2)))
            (pairs, actualtys))]
    )
    handle SMLClassDefOps.ClassNotFound name =>
    (error (Error.error(loc, "class not found: " ^ name), []);
     [makeInvoke []])

  end

(*----------------------------------------------------------------------*)
(* Elaborate a set of field initialisers                  		*)
(*----------------------------------------------------------------------*)
fun infFieldInits (C, FE, loc, required) fldinits =
let
  val fields = List.filter (fn (_,mods,_,_) =>
    not (List.exists (fn m => m=Field.STATIC) mods)) FE

  fun infFieldInit (id, arg) =
    let
      val (exp, ty) = infExp C arg
      fun find [] =
          (error (Error.error(loc, "no such field " ^ 
          JavaString.toMLString id), []); [])

        | find ((name,mods,fldty,_)::fields) =
          if JavaString.equal(name,id)
          then 
            (unify ((SOME loc, "expression", ty),
                         (NONE, "field type", fldty));
            [SMLTerm.Val(loc, [], unitType, SMLTerm.PatWild,
                  SMLTerm.Java((Java.PutField, NONE, SOME id),
                  [SMLTerm.Var ((Ids.thisSym, []), []), exp],
                  NONE, Effect.writes))])
          else find fields
    in
      find fields
    end

  val decss = map infFieldInit fldinits
in
  if required andalso length fields <> length fldinits
  then error(Error.error(loc, "not all fields initialised"), [])
  else ();
  List.concat decss
end

(*----------------------------------------------------------------------*)
(* Elaborate the _value_ part of Java field and method definitions.     *)
(* Restrictions:                                                        *)
(*   static fields of non-exportable type must have initialisers;       *)
(*   non-static fields must not have initialisers.                      *)
(* Unchecked:                                                           *)
(*   initialisers should not raise exceptions [8.3.2,JLS].              *)
(*----------------------------------------------------------------------*)
fun infClassItemsVals (allFE,thisclass,superclass) (C,CE,loc) (items,FE,ME) =
case (items,FE,ME) of
  ([], [], []) => 
  ([], [])
 
| ((loc, Field {initial,modifiers,...})::items, 
    (fldinfo as (name,_,ty,_))::FE, ME) =>
  let
    val isStatic = List.exists (fn x => x=JavaFlags.STATIC) modifiers
    val initial = 
    case initial of
      NONE => 
      if isStatic andalso not (SMLJavaOps.isExportable (fn _ => true) ty)
      then 
        (error(Error.error(loc, 
          "static field of non-exportable type must have initialiser: "
          ^ JavaString.toMLString name), []); NONE)
      else NONE

    | SOME (exp as (loc1,_)) =>
      if isStatic then
      let
        val (e, ty') = infExp C exp
      in
        unify ((SOME loc1, "field expression", ty'),
                     (NONE, "field type", ty));
        SOME e
      end
      else
        (error(Error.error(loc,
          "non-static field declaration must not have \
          \initialiser (use constructor): " ^ JavaString.toMLString name), []);
        NONE)
    val (FE', ME') = infClassItemsVals (allFE,thisclass,superclass) (C,CE,loc) 
      (items,FE,ME)
  in
    ((fldinfo, initial)::FE', ME')
  end

| ((loc, Method { body, args, ... })::items, FE, 
  (mthinfo as (_,flags,argtys,restyopt))::ME) =>
  let
    val VE = 
      ListPair.foldr (fn ((argnameopt,_), argty, VE) =>
        case argnameopt of
          NONE => VE
        | SOME argname =>
          Map.insert(VE,argname,ValBind.VarSch(SMLSchOps.monoType argty)))
        (if List.exists (fn Method.STATIC => true | _ => false) flags
        then Map.empty
        else 
        let
          val withsuper = Map.insert(Map.empty, Ids.superSym,
              ValBind.VarSch(SMLSchOps.monoType superclass))

          val withthis = Map.insert(withsuper, Ids.thisSym, 
            ValBind.VarSch(SMLSchOps.monoType thisclass))

          val withthis = Map.insert(withthis, Ids.thisSym2, 
            ValBind.VarSch(SMLSchOps.monoType thisclass))
        in
          withthis
        end)
      (args, argtys)
    val etyopt = Option.map (infExp (CplusVE C VE)) body
    val (FE', ME') = infClassItemsVals (allFE,thisclass,superclass) (C,CE,loc)
      (items,FE,ME)
    val body = 
      case (etyopt, restyopt) of
      (SOME (bodye,ty1), SOME ty2) => 
      (unify ((Option.map #1 body, "method expression", ty1),
                   (NONE, "method return type", ty2)); SOME bodye)

    | (SOME (bodye,ty), NONE) => 
      (unify ((Option.map #1 body, "method expression", ty),
                   (NONE, "expected", unitType)); SOME bodye)

    | _ =>
      NONE
  in
    (FE', (mthinfo : SMLTy.MethodInfo, map #1 args,
      body : SMLTerm.Exp option) :: ME')
  end

| ((loc, Constructor { body, args, inits, ... })::items, FE, 
  (mthinfo as (_,flags,argtys,restyopt))::ME) =>
  let
    val VE = 
      ListPair.foldr (fn ((argnameopt,_), argty, VE) =>
        case argnameopt of 
          NONE => VE
        | SOME argname =>
          Map.insert(VE,argname,ValBind.VarSch(SMLSchOps.monoType argty)))
        Map.empty
        (args, argtys)

    val VESuper =
       Map.insert(VE, Ids.superSym,
         ValBind.VarSch(SMLSchOps.monoType superclass))

    val VESuperThis = 
       Map.insert(VESuper, Ids.thisSym, 
          ValBind.VarSch(SMLSchOps.monoType thisclass))

    val VESuperThis = 
       Map.insert(VESuperThis, Ids.thisSym2, 
          ValBind.VarSch(SMLSchOps.monoType thisclass))

    val (isSuper, invocClass, invocArgs, fldinits) =
      case inits of
        SuperInvoc (exps,fldinits) => (true, superclass, exps, fldinits)
      | ThisInvoc exps => (false, thisclass, exps, [])
      | NoInits => 
        Debug.fail "ElabJavaDec.infClassItemsVals: missing initialisers"
 
    val etyopt = Option.map (infExp (CplusVE C VESuperThis)) body
    val (FE', ME') = infClassItemsVals (allFE,thisclass,superclass) (C,CE,loc)
      (items,FE,ME)

    val body = 
    case etyopt of
      SOME (bodye,ty) => 
      (unify ((Option.map #1 body, "type of constructor body", ty),
                   (NONE, "expected", unitType)); SOME bodye)

    | _ =>
      NONE

    val invocdecs = infConInvoc (CplusVE C VE, CE, loc) 
      (isSuper, invocClass, invocArgs)
    val flddecs = infFieldInits (CplusVE C VESuper,allFE,loc,isSuper) fldinits
  in
    (FE', (mthinfo : SMLTy.MethodInfo, map #1 args, 
      case body of
        NONE => SOME (SMLTerm.Let(invocdecs@flddecs, SMLTerm.Record []))
      | SOME body => SOME (SMLTerm.Let(invocdecs@flddecs, body)))::ME')
  end

| _ =>
  Debug.fail "ElabJavaDec.infClassItemsVals: field and method info has changed"

fun elab insig C (loc,javadec) =
case javadec of

(*......................................................................*)
(* External exception							*)
(*......................................................................*)
  ClassException((_,excon), classid) =>
  let
    val longid = SMLClassDefOps.classToLongid classid
    val CE = getClassEnv ()
  in
    if SMLClassDefOps.checkExplicit longid 
    then
      if SMLClassDefOps.subClass CE 
        (SMLTy.baseType (TyName.externalClass longid), TopEnv.exceptionclass)
      then 
        let
          val exname = freshJavaExName (pathofC C @ [excon], 
          SMLTy.baseType (TyName.externalClass longid))
        in
          ([], VEinE (Map.insert(Map.empty, excon, 
          ValBind.ExTy(exnType, exname))))
        end
      else
        (SMLTy.error(Error.error(loc, 
          "class does not subclass java.lang.Exception: " ^ 
          JavaString.toMLString classid), []);
        ([], emptyE))

    else
      (SMLTy.error(Error.error(loc, "external class not found: " 
        ^ Pretty.longidToString longid), []);
      ([], emptyE))
  end

(*......................................................................*)
(* Internal class							*)
(*......................................................................*)
| ClassType { tycon, modifiers, super, implements, body } =>
  let
    val classname = SMLTy.freshTyName (pathofC C @ [tycon], TySort.classSort)
    val classty = SMLTy.consType([], classname)
    val tystr = TyStr.makeConcrete([], classty)
    val TE = Map.insert(Map.empty, tycon, tystr)
    val mods = map convClassMod modifiers
    val superty = Option.map (infTy C) super
    val _ = Option.map (checkClass loc) superty
    val implementstys = map (infTy C) implements  
    val _ = map (checkClass loc) implementstys
    val (allFE, allME) = infClassItemsTys (CplusTE C TE,loc,mods) body
    val _ = checkClassItemsTys loc (allFE,allME)
    val superty = getOpt (superty, TopEnv.objectclass)
    val classinfo = 
      (
        mods,
        SOME superty,
        implementstys
      )
    val allclassdef = (classinfo,allFE,allME)
    val alltystr = TyStr.makeClassType (classname, allclassdef)
    val tystr = TyStr.makeClassType (classname, allclassdef)
    val allTE = Map.insert(Map.empty, tycon, alltystr)
    val allE = EplusTE emptyE allTE

    val C = CwithClass (CplusE C allE) classname
    val _ = addClass (classname, allclassdef)
    val CE = getClassEnv ()
    val (FE, ME) = checkInherit (C,CE,loc,insig) (classty,allclassdef)
    val classdef = (classinfo,FE,ME)
    val tystr = TyStr.makeClassType (classname, classdef)
    val TE = Map.insert(Map.empty, tycon, tystr)
    val E = EplusTE emptyE TE
  in
    if insig
    then ([], E)
    else
    let
      val (FEdefs, MEdefs) = 
        infClassItemsVals (allFE,classty,superty) (C,CE,loc) 
        (body,allFE,allME)
    in
      ([T.ClassType(classname, classinfo, FEdefs, MEdefs)], E)
    end
  end handle SMLClassDefOps.ClassNotFound name =>
    (error(Error.error(loc, "class not found: " ^ name), []); 
     ([], EnvOps.emptyE))
  

end (* of local *)

end (* of struct *)
