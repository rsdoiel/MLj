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
structure SMLJavaOps :> SMLJAVAOPS = 
struct

local 
  open TyName
in

(*----------------------------------------------------------------------*)
(* If ty is of the form ty' option then return SOME ty', otherwise NONE	*)
(*----------------------------------------------------------------------*)
fun fromOptionType ty =
  case SMLTy.fromConsType ty of
    SOME ([ty'], tyname) =>      
    if eq(tyname, optionTyName) then SOME ty'
    else NONE

  | _ => NONE

(*----------------------------------------------------------------------*)
(* Return true if ty is a reference type (class or array)		*)
(*----------------------------------------------------------------------*)
fun isClass ty = #class (SMLTy.sort false ty)
fun isReference ty = isClass ty orelse isSome (SMLTy.fromArrayType ty)

(*----------------------------------------------------------------------*)
(* Return true if ty is a base type in the list specified.		*)
(*----------------------------------------------------------------------*)
fun isTyNameIn tynames ty =
  case SMLTy.fromConsType ty of
    SOME ([], tyname) => 
    List.exists (fn tyname' => TyName.eq(tyname,tyname')) tynames

  | _ => 
    false

(*----------------------------------------------------------------------*)
(* Return true if ty is a Java primitiave type (2.4.1, JVM; 4.2, JLS)   *)
(*----------------------------------------------------------------------*)
val isPrimitive =
  isTyNameIn 
  [javaBoolean, javaByte, javaChar, javaDouble, javaFloat, javaInt, 
   javaLong, javaShort]

(*----------------------------------------------------------------------*)
(* Return true if ty is a Java numeric type (2.4.1, JVM; 4.2, JLS)	*)
(*----------------------------------------------------------------------*)
val isNumeric =
  isTyNameIn
  [javaByte, javaChar, javaDouble, javaFloat, javaInt, javaLong, javaShort]

(*----------------------------------------------------------------------*)
(* Return true if ty is a Java `arithmetic' type                        *)
(*----------------------------------------------------------------------*)
val isArithmetic =
  isTyNameIn
  [javaDouble, javaFloat, javaInt, javaLong]

(*----------------------------------------------------------------------*)
(* Return true if ty is a Java integral type (2.4.1, JVM; 4.2, JLS)	*)
(*----------------------------------------------------------------------*)
val isIntegral =
  isTyNameIn
  [javaByte, javaChar, javaInt, javaLong, javaShort]

(*----------------------------------------------------------------------*)
(* Return true if ty is a Java floating point type(2.4.1, JVM; 4.2, JLS)*)
(*----------------------------------------------------------------------*)
val isFloatingPoint =
  isTyNameIn
  [javaDouble, javaFloat]

fun isExportableClassType isExportableClass ty =
    case SMLTy.fromConsType ty of
       SOME ([], tyname) => 
       isSome (TyName.fromExternalClass tyname) orelse isExportableClass tyname
     | _ => false

(*----------------------------------------------------------------------*)
(* Return true if ty is an `exportable Java type'.                      *)
(*----------------------------------------------------------------------*)
fun isExportable isExportableClass ty =
  isPrimitive ty orelse
  (case fromOptionType ty of
    NONE => false
  | SOME ty => 
    (case SMLTy.fromArrayType ty of
      SOME ty => isExportable isExportableClass ty
    | NONE => 
      case SMLTy.fromConsType ty of
        SOME ([], tyname) => 
        isSome (TyName.fromExternalClass tyname) 
        orelse isExportableClass tyname
      | _ => false))

(*----------------------------------------------------------------------*)
(* Return true if ty is a `Java type', with or without options.         *)
(*----------------------------------------------------------------------*)
fun isJava isExportableClass ty =
  isPrimitive ty orelse
  let
    val ty = getOpt (fromOptionType ty, ty)
  in
    case SMLTy.fromArrayType ty of
      SOME ty => isJava isExportableClass ty
    | NONE =>
      case SMLTy.fromConsType ty of
        SOME ([], tyname) => 
        isSome (TyName.fromExternalClass tyname) 
        orelse isExportableClass tyname
      | _ => false
  end

(*----------------------------------------------------------------------*)
(* Return SOME ty' (options removed) if ty is a `Java type',            *)
(* with or without options. Otherwise return NONE                       *)
(*----------------------------------------------------------------------*)
fun fromJava ty =
  if isPrimitive ty then SOME ty
  else
  let
    val ty = getOpt (fromOptionType ty, ty)
  in
    if isClass ty then SOME ty
    else
      case SMLTy.fromArrayType ty of
        NONE => NONE
      | SOME ty => 
        case fromJava ty of
          NONE => NONE
        | SOME ty => SOME (SMLTy.arrayType ty)
  end


(*----------------------------------------------------------------------*)
(* Return true if type ty1 can be converted to type ty2 by means of	*)
(* a widening primitive conversion (5.1.2, Java language spec).         *)
(*----------------------------------------------------------------------*)
  fun primWidening (ty1, ty2) =
  case (SMLTy.fromConsType ty1, SMLTy.fromConsType ty2) of

    (SOME ([], tyname1), SOME ([], tyname2)) =>
    List.exists 
      (fn (tyname1', tynames) => 
        eq(tyname1, tyname1') andalso List.exists 
          (fn tyname2' => eq(tyname2, tyname2')) tynames)

    [(javaByte,  [javaShort, javaInt, javaLong, javaFloat, javaDouble]),
     (javaShort, [javaInt, javaLong, javaFloat, javaDouble]),
     (javaChar,  [javaInt, javaLong, javaFloat, javaDouble]),
     (javaInt,   [javaLong, javaFloat, javaDouble]),
     (javaLong,  [javaFloat, javaDouble]),
     (javaFloat, [javaDouble])]

  | _ => 
    false

(*----------------------------------------------------------------------*)
(* Return true if type ty1 can be converted to type ty2 by means of	*)
(* a narrowing primitive conversion (5.1.3, Java language spec).        *)
(*----------------------------------------------------------------------*)
  fun primNarrowing (ty1, ty2) =
  case (SMLTy.fromConsType ty1, SMLTy.fromConsType ty2) of

    (SOME ([], tyname1), SOME ([], tyname2)) =>
    List.exists 
      (fn (tyname1', tynames) => 
        eq(tyname1, tyname1') andalso List.exists 
          (fn tyname2' => eq(tyname2, tyname2')) tynames)

    [(javaByte,  [javaChar]),
     (javaShort, [javaByte, javaChar]),
     (javaChar,  [javaByte, javaShort]),
     (javaInt,   [javaByte, javaShort, javaChar]),
     (javaLong,  [javaByte, javaShort, javaChar, javaInt]),
     (javaFloat, [javaByte, javaShort, javaChar, javaInt, javaLong]),
     (javaDouble, [javaByte, javaShort, javaChar, javaInt, javaLong, 
        javaFloat])]

  | _ => 
    false

end (* of local open *)

(*----------------------------------------------------------------------*)
(* Return true if type ty1 can be converted to type ty2 by means of	*)
(* an identity conversion (5.1.1, Java language spec).                  *)
(* If elideopt=true then we also permit the following conversions:      *)
(*    ty option --> ty option      if ty is a reference type.           *)
(*    ty --> ty option             if ty is a reference type.           *)
(* For the identity we also allow ty1 to be _instantiated_ to ty2 (i.e. *)
(* for ty1 to contain type variables).                                  *)
(*----------------------------------------------------------------------*)
fun idConv {update,elideopt} (ty1, ty2) =
  isSome (SMLTy.match update (ty1, ty2)) 
  orelse
  (elideopt andalso
    case (fromOptionType ty1, fromOptionType ty2) of
      (NONE, NONE) => false
    | (SOME ty1', SOME ty2') => 
      isReference ty1' andalso isSome (SMLTy.match update (ty1', ty2'))
    | (NONE, SOME ty2') => 
      isReference ty1 andalso isSome (SMLTy.match update (ty1, ty2'))
    | (SOME ty1', NONE) => false)

(*----------------------------------------------------------------------*)
(* Return true if type ty1 can be converted to type ty2 by means of	*)
(* a widening reference conversion (5.1.4, Java language spec).         *)
(* If elideopt=true then we also permit the following conversions:      *)
(*    r1 option --> r2 option    if r1 --> r2 by a widening ref conv.   *)
(*    r1 --> r2 option           if r1 --> r2 by a widening ref conv.   *)
(*----------------------------------------------------------------------*)
fun refWidening' CE (elideto,elidefrom) (ty1, ty2) =
let
  fun withoutEliding (ty1, ty2) =
  (*..................................................................*)
  (* Firstly test to see if ty1 is a subclass of ty2.		      *)
  (*..................................................................*)
  if isClass ty1 andalso isClass ty2
  then SMLClassDefOps.subClass CE (ty1, ty2)
  else
    case (SMLTy.fromArrayType ty1, SMLTy.fromArrayType ty2) of

  (*..................................................................*)
  (* If ty1 = ty1' Java.array and ty2 = ty2' Java.array then recurse  *)
  (* on ty1' and ty2'.                                                *)
  (*..................................................................*)
      (SOME ty1', SOME ty2') =>
      refWidening' CE (elideto,elidefrom) (ty1',ty2')

  (*..................................................................*)
  (* If ty1 = ty1' Java.array and ty2 = Object or ty2 = Cloneable     *)
  (* then this counts as a widening reference conversion.             *)
  (*..................................................................*)
    | (SOME ty1', NONE) =>
      SMLTy.eq (ty2, TopEnv.objectclass) orelse
      SMLTy.eq (ty2, TopEnv.cloneableclass)

    | _ => false
in
  (*..................................................................*)
  (* Check for r1 option --> r2 option or r1 --> r2 option.	      *)
  (*..................................................................*)
  if elideto orelse elidefrom then
    case (fromOptionType ty1, fromOptionType ty2) of
      (NONE, NONE) => withoutEliding (ty1, ty2)
    | (SOME ty1', SOME ty2') => withoutEliding (ty1', ty2')
    | (NONE, SOME ty2') => elideto andalso withoutEliding (ty1, ty2')
    | (SOME ty1', NONE) => elidefrom andalso withoutEliding (ty1', ty2)

  else withoutEliding (ty1, ty2)
end

fun refWidening CE elideopt (ty1,ty2) = 
  refWidening' CE (elideopt,false) (ty1,ty2)

fun refNarrowing CE elideopt (ty1,ty2) =
  refWidening' CE (false,elideopt) (ty2,ty1)
            
(*----------------------------------------------------------------------*)
(* Return true if type ty1 can be converted to type ty2 by method 	*)
(* invocation conversion (15.11.2.1 and 5.3, Java language spec).       *)
(* If elideopt=true then we also permit the additional conversions      *)
(* listed above.                                                        *)
(*----------------------------------------------------------------------*)
fun methodConv CE elideopt (ty1, ty2) =
  idConv {update=false,elideopt=true} (ty1, ty2) orelse 
  primWidening (ty1, ty2) orelse refWidening CE elideopt (ty1, ty2)

(*----------------------------------------------------------------------*)
(* Return the package containing a Java class				*)
(*----------------------------------------------------------------------*)
fun package ty =
  case SMLTy.fromConsType ty of
    SOME([], tyname) => rev (List.tl (rev (TyName.longid tyname)))
  | _ => []

(*----------------------------------------------------------------------*)
(* Given a root class, collect the methods and fields that are          *)
(* inherited by the class. [8.2, JLS]        	                        *)
(* If includeroot=true, include the root class methods and fields too.  *)
(* Only return fields and methods matching the given predicates.        *) 
(*----------------------------------------------------------------------*)
fun getInherited CE (rootclassty, includeroot, fieldp, methodp) =
let
  fun search (includeroot,isroot) [] = ([], [])
    | search (includeroot,isroot) (classty::classes) =

  let
    val (classinfo, fields, methods) = 
      SMLClassDefOps.tyToClassDef CE classty

    val samePackage = isroot orelse 
      Eq.list Symbol.equal (package rootclassty, package classty)

    fun isAccessibleMethod (methodinfo as (name, flags, _, _)) =    
      let 
        val private = 
          List.exists (fn Method.PRIVATE => true | _ => false) flags
        val public = 
          List.exists (fn Method.PUBLIC => true | _ => false) flags
      in
        not (JavaString.equal(name,JavaString.fromString "<init>")) 
        andalso not (JavaString.equal(name, JavaString.fromString "<clinit>"))
         andalso
        (isroot orelse not private) andalso (public orelse samePackage)
        andalso methodp methodinfo
      end

    fun isAccessibleField (fldinfo as (_, flags, _, _)) =    
      let 
        val private = 
          List.exists (fn Field.PRIVATE => true | _ => false) flags
        val public = 
          List.exists (fn Field.PUBLIC => true | _ => false) flags
      in
        (isroot orelse not private) andalso 
        (public orelse samePackage) andalso
        fieldp fldinfo
      end

    val accessibleMethods = 
      if includeroot 
      then List.filter isAccessibleMethod methods else []
    val accessibleFields = 
      if includeroot 
      then List.filter isAccessibleField fields else []

    fun tag m = (classty, m)
    
    fun hides ((name1,_,_,_),(name2,_,_,_)) = JavaString.equal(name1,name2)
    fun overrides ((name1,_,argtys1,_),(name2,_,argtys2,_)) =
      JavaString.equal(name1,name2) andalso Eq.list SMLTy.eq (argtys1,argtys2) 

    val (fs, ms) = (map tag accessibleFields, map tag accessibleMethods)

    fun isHidden (_,fldinfo) = 
      List.exists (fn fldinfo' =>hides(fldinfo',fldinfo)) accessibleFields
 
    fun isOverridden (_,mthinfo) =
      List.exists (fn mthinfo' =>overrides(mthinfo',mthinfo)) accessibleMethods

    val (fs', ms') = search (true,false) 
      (Dups.removeDups SMLTy.eq 
        (Gen.optToList (#2 classinfo) @ #3 classinfo @ classes))
  in
    (fs @ List.filter (not o isHidden) fs',
     ms @ List.filter (not o isOverridden) ms')
  end
in
  search (includeroot,true) [rootclassty]
end


(*----------------------------------------------------------------------*)
(* Given a context class and a root class, collect the methods with	*)
(* the specified name that are `accessible' (15.11.2.1, Java language   *)
(* spec) and split these into two lists: those that are `applicable'    *)
(* and those that are not.                                              *)
(*----------------------------------------------------------------------*)
fun getMethods CE (privateAccess, class, includeroot, name, argtys) =
let
  val (fields, methods) = 
    getInherited CE (class, includeroot, fn _ => false,
      fn (name',_,_,_) => JavaString.equal(name,name'))

  fun isApplicable (_, (name, mods, argtys', _)) =      
    length argtys = length argtys'
    andalso (privateAccess orelse 
      not (List.exists (fn m => m=Method.PRIVATE) mods))
    andalso ListPair.all (methodConv CE true) (argtys, argtys')
in
  List.partition isApplicable methods
end

(*----------------------------------------------------------------------*)
(* Given a class, collect together all accessible                       *)
(* <init> methods and split these into two lists: those that are        *)
(* `applicable' and those that are not.                                 *)
(*----------------------------------------------------------------------*)
fun getConstructors CE (privateAccess, classty, argtys) =
let
  val (classinfo, fields, methods) = 
    SMLClassDefOps.tyToClassDef CE classty

  fun isApplicable (_, _, argtys', _) =      
      length argtys = length argtys'
      andalso ListPair.all (methodConv CE true) (argtys, argtys')

  fun isAccessible (name, flags, _, _) =    
      let 
        val private = 
          List.exists (fn Method.PRIVATE => true | _ => false) flags
      in
        JavaString.equal(name, JavaString.fromString"<init>") 
        andalso (not private orelse privateAccess)
      end
    val accessible = List.filter isAccessible methods
    fun tag m = (classty, m)
    val (valid, invalid) = List.partition isApplicable accessible
    
    val (valid, invalid) = (map tag valid, map tag invalid)
in
  (valid, invalid)
end

(*----------------------------------------------------------------------*)
(* Given a means of obtaining a ClassDef from an SML class type,	*)
(* list the fields that are  `accessible' (15.10.1, Java language spec) *)
(* from the given class.                                                *)
(*----------------------------------------------------------------------*)
fun getFields CE (privateAccess, classty, name) =
let
  val (fields, methods) = 
    getInherited CE (classty, true, 
      fn (name',_,_,_) => JavaString.equal(name,name'), fn _ => false)

  fun isApplicable (_, (_, mods, _, _)) =      
    privateAccess orelse 
      not (List.exists (fn m => m=Field.PRIVATE) mods)
in
  List.filter isApplicable fields
end

fun mostSpecific CE (m, []) = SOME m
  | mostSpecific CE (m1, (m2::ms)) =
    let
      fun moreSpecific 
          ((class1, (_, _, argtys1, _)), 
           (class2, (_, _, argtys2, _))) =
        methodConv CE true (class1, class2)
        andalso
        ListPair.all (methodConv CE true) (argtys1, argtys2)
    in
      if moreSpecific (m1, m2) then mostSpecific CE (m1, ms) else
      if moreSpecific (m2, m1) then mostSpecific CE (m2, ms) else
      NONE
    end

(*----------------------------------------------------------------------*)
(* Create a no-op cast operation if necessary; also infer effect info.	*)
(*----------------------------------------------------------------------*)
fun cast CE e (ty1, ty2) =
  if SMLTy.eq(ty1, ty2) 
  then 
    SOME e
  else  

  if idConv {update=true,elideopt=true} (ty1, ty2) 
    orelse refWidening CE true (ty1, ty2)
  then 
    SOME (SMLTerm.Java((Java.NopCast, NONE, NONE), [e], SOME ty2, Effect.none))
  else 

  if refNarrowing CE true (ty1, ty2) 
  then
    SOME (SMLTerm.Java((Java.Cast, NONE, NONE), [e], SOME ty2, Effect.throws))
  else
  if primWidening (ty1, ty2) orelse primNarrowing (ty1,ty2)
  then
    SOME (SMLTerm.Java((Java.Cast, NONE, NONE), [e], SOME ty2, Effect.none))
  else
    NONE

(*----------------------------------------------------------------------*)
(* Create a no-op (class) cast operation if necessary; also infer       *)
(* effect info.	                                                        *)
(*----------------------------------------------------------------------*)
fun castClass CE e (class1, class2) =
  if SMLTy.eq(class1, class2) then e
  else 
  let
    val nop = isClass class1 andalso isClass class2 andalso 
      SMLClassDefOps.subClass CE (class1, class2)
  in
    SMLTerm.Java((if nop then Java.NopCast else Java.Cast, NONE, NONE), 
      [e], SOME class2, if nop then Effect.none else Effect.throws)
  end

(*----------------------------------------------------------------------*)
(* Return NONE if this class is exportable, under the assumption that	*)
(* all of "classes" are exportable.                                     *)
(* Otherwise return SOME reason.                                        *)
(*----------------------------------------------------------------------*)
fun isExportableClassDef isExternalClass
  (classty,((_,superopt,ints),fields,methods)) =
let
  fun findBadMethodArgs [] = NONE
    | findBadMethodArgs ((name,flags,argtys,restyopt)::methods) =
      if List.exists (fn f => f = Method.PRIVATE) flags
      orelse List.all (isExportable isExternalClass) argtys
      then findBadMethodArgs methods
      else SOME 
        ("arguments of non-private method " ^ valOf(JavaString.toString name)^ 
         " does not have exportable type")

  fun findBadMethodResults [] = findBadMethodArgs methods

    | findBadMethodResults ((name,flags,argtys,NONE)::methods) = 
      findBadMethodResults methods

    | findBadMethodResults ((name,flags,argtys,SOME resty)::methods) =
      if List.exists (fn f => f = Method.PRIVATE) flags
      then findBadMethodResults methods
      else
        if List.exists (fn f => f = Method.FINAL) flags
        then 
          if isJava isExternalClass resty
          then findBadMethodResults methods
          else SOME ("non-private, final method " ^ 
            valOf(JavaString.toString name) ^ 
                     " does not have Java type")
        else
          if isExportable isExternalClass resty
          then findBadMethodResults methods
          else SOME ("non-private, non-final method " ^ 
            valOf(JavaString.toString name) ^ 
                     " does not have exportable type")

  fun findBadField [] = findBadMethodResults methods
    | findBadField ((name,flags,ty,value)::fields) =
      if List.exists (fn f => f = Field.PRIVATE) flags
      orelse (if List.exists (fn f => f = Field.FINAL) flags
              then isJava isExternalClass ty
              else isExportable isExternalClass ty)
      then findBadField fields
      else SOME 
        ("non-private field " ^ valOf(JavaString.toString name) 
           ^ " does not have exportable type")

  fun findBadInterface [] = findBadField fields
    | findBadInterface (int::ints) = 
      if isExportableClassType isExternalClass int
      then findBadInterface ints
      else SOME ("interface " ^ SMLTy.toString int ^ " is not exportable")

  val result =
  case superopt of
    NONE => 
    findBadInterface ints

  | SOME super => 
    if isExportableClassType isExternalClass super
    then findBadInterface ints
    else SOME ("superclass " ^ SMLTy.toString super ^ " is not exportable")
in
  case result of
    NONE => NONE
  | SOME reason => SOME ("Class " ^ SMLTy.toString classty ^ 
    " cannot be exported: " ^ reason)
end


end
