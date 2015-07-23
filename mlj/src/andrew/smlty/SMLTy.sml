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
(* ML types								*)
(*======================================================================*)
structure SMLTy :> SMLTY =
struct

(*----------------------------------------------------------------------*)
(* Source types:							*)
(*    Var r								*)
(*      type variable, possibly substituted for 			*)
(*    Con(c, tys)							*)
(*      type constructor parameterised on tys				*)
(*    Rec r								*)
(*      possibly open record type 					*)
(*    Fun(ty1, ty2)							*)
(*      function type							*)
(*    Ref ty								*)
(*      reference type (special: always admits equality)         	*)
(*    Array ty                                                          *)
(*      array type (special: always admits equality)                    *)
(*----------------------------------------------------------------------*)
datatype Type = 
  Var of TyVarOrType ref
| Con of TyName.TyName * Type list
| Rec of Row ref
| Fun of Type * Type
| Ref of Type
| Array of Type

and RowVarOrRow = 
  RowVar of TyVar.TyVar 
  (* really a row variable: only sensible sorts are Any and Eq *)
| Row of Row

(*----------------------------------------------------------------------*)
(* A free type variable *or* an instantiated one.			*)
(*----------------------------------------------------------------------*)
and TyVarOrType =
  TyVar of TyVar.TyVar
| Type of Type

(*----------------------------------------------------------------------*)
(* A record row								*)
(*----------------------------------------------------------------------*)
withtype Row = Type Symbol.OrdMap.map * RowVarOrRow ref option

(*----------------------------------------------------------------------*)
(* Java field: name, modifiers, type and final static value info.       *)
(*----------------------------------------------------------------------*)
and FieldInfo = 
  JavaString.t * Field.flag list * Type * Constants.constant option

(*----------------------------------------------------------------------*)
(* Java method: name, modifiers, argument types and (optional) result   *)
(* type.                                                                *)
(*----------------------------------------------------------------------*)
and MethodInfo = 
  JavaString.t * Method.flag list * Type list * Type option

(*----------------------------------------------------------------------*)
(* Java class: modifiers, (optional) superclass and list of interfaces. *)
(*----------------------------------------------------------------------*)
and ClassInfo = 
  Class.flag list * Type option * Type list

(*----------------------------------------------------------------------*)
(* The full class definition.						*)
(*----------------------------------------------------------------------*)
and ClassDef =
  ClassInfo * FieldInfo list * MethodInfo list

(*----------------------------------------------------------------------*)
(* Datatype environment: the integer is a stamp unique to this module   *)
(* associated with the type constructor.                                *)
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

structure ExNameOrd =
  struct
    type ord_key = ExName
    fun compare ((e1,i1), (e2,i2)) = 
      (case Entity.Map.Key.compare(e1, e2) of
        EQUAL => Int.compare(i1, i2)
      | other => other)
  end

structure ExMap = MapFn(ExNameOrd)

fun exNameToString (e,i) = EntityOps.description e ^ "(" ^ Int.toString i ^ ")"

type TypeFcn = TyVar.TyVar list * Type
type Realisation = TypeFcn TyName.Map.map

(*----------------------------------------------------------------------*)
(* Functions to construct type variable types, arrow types, closed 	*)
(* and open record types and parameterised types.			*)
(* Also a shorthand for constructing tuple types.			*)
(*----------------------------------------------------------------------*)
fun recType fields = 
  Rec (ref
  (foldr (fn ((lab,ty),row) => 
    Symbol.OrdMap.insert(row,lab,ty)) Symbol.OrdMap.empty fields,
    NONE))

fun tyVarType tyvar = Var(ref (TyVar tyvar))
fun funType (ty1, ty2) = Fun(ty1, ty2)
fun consType (tys, tyname) = Con(tyname, tys)
fun baseType tyname = Con(tyname, [])
fun arrayType ty  = Array ty
fun refType ty = Ref ty
fun tupleType tys =
let 
  fun make n [] = []
    | make n (ty::tys) = 
      (Symbol.symbol (JavaString.fromString (Int.toString n)), ty) :: 
      make (n+1) tys
in
  recType (make 1 tys)
end

fun classToString s =
    "\"" ^ 
    String.map (fn #"/" => #"." | c => c) (valOf(JavaString.toString s)) ^
    "\""

(*----------------------------------------------------------------------*)
(* Remove indirections if possible					*)
(*----------------------------------------------------------------------*)
fun normTy ty = 
  case ty of
    Var(r as ref (Type ty')) =>
      let val ty'' = normTy ty'
      in
        (r := Type ty''; ty'')
      end

  | Rec (r as ref row) =>
    (r := normRow row; ty)

  | _ => ty

and normRow (fixed, SOME (r as ref (Row variable))) =
    let 
      val variable' as (fixed', r') = normRow variable
    in
      r := Row variable'; 
      (Symbol.OrdMap.unionWith #2 (fixed, fixed'), r')
    end

  | normRow r = r

(*----------------------------------------------------------------------*)
(* Determine whether a record type is of the form			*)
(*   { 1 : ty_1, 2 : ty_2, ..., n : ty_n }				*)
(* for n != 1.                                                           *)
(*----------------------------------------------------------------------*)
fun fromProd ty =
  case normTy ty of
    Rec (ref (fixed, NONE)) =>
    let
      val n = Symbol.OrdMap.numItems fixed
      fun lookup (0, result) = 
          SOME result

        | lookup (i, result) =
          case Symbol.OrdMap.find(fixed, Ids.symbol (Int.toString i)) of
            NONE => 
            NONE

          | SOME ty => 
            lookup (i-1, ty :: result)
    in
      lookup (n, [])
    end

  | _ => 
    NONE

val isProd = isSome o fromProd

fun fromTyVar ty =
  case normTy ty of
    Var (ref (TyVar tyvar)) => SOME tyvar
  | _ => NONE

fun fromArrayType ty =
  case normTy ty of
    Array ty => SOME ty
  | _ => NONE

fun isFun ty =
  case normTy ty of
    Fun _ => true
  | _ => false

(*----------------------------------------------------------------------*)
(* Pretty-print a type for the purposes of unification errors etc.	*)
(*----------------------------------------------------------------------*)
and toString ty = 
case normTy ty of

  Var(ref (TyVar v)) => 
  TyVar.toString v

| Con(tyname, [ty]) => 
  parens (isProd ty orelse isFun ty) (toString ty)    
  ^ " " ^ TyName.toString tyname

| Con(tyname, tys) => 
  Pretty.vec ("", "", "", "(", ") ", ",") toString tys ^ TyName.toString tyname

| Fun(ty1, ty2) =>
  parens (isFun ty1) (toString ty1) ^ "->" ^ toString ty2

| Rec (ref (fixed, rest)) => 
  let 
    val fields = Symbol.OrdMap.listItemsi fixed
  in
    if null fields andalso not (isSome rest) then "unit"
    else
    if isProd ty 
    then Pretty.vec ("unit", "", "", "", "", "*")
         (fn ty => parens (isFun ty orelse isProd ty) (toString ty))
         (Symbol.OrdMap.listItems fixed)
    else "{" ^ rowToString fields ^ 
      (case rest of
        NONE => "}"
      | SOME _ => ",...}")
  end

| Ref ty => 
  parens (isFun ty orelse isProd ty) (toString ty) ^ " ref"

| Array ty => 
  parens (isFun ty orelse isProd ty) (toString ty) ^ " array"

| _ => Debug.fail "SMLTy.toString: non-normalised type"

and rowToString [] = ""
  | rowToString [(lab,ty)] =
    JavaString.toMLString (Symbol.toJavaString lab) ^ ":" ^ toString ty 
  | rowToString ((lab,ty)::rest) = 
    JavaString.toMLString (Symbol.toJavaString lab) ^ ":" ^ toString ty 
    ^ "," ^ rowToString rest

and parens b s = if b then "(" ^ s ^ ")" else s

(*----------------------------------------------------------------------*)
(* Type variables in a type						*)
(*----------------------------------------------------------------------*)
fun tyvars ty =
  case normTy ty of
    Var (ref (TyVar tyvar)) =>
    TyVar.Set.singleton tyvar 

  | Con(_, tys)	=> 
    foldr (fn (ty,tvs) => TyVar.Set.union(tyvars ty, tvs)) TyVar.Set.empty tys

  | Ref ty => 
    tyvars ty

  | Array ty => 
    tyvars ty

  | Fun(ty1,ty2) => 
    TyVar.Set.union (tyvars ty1, tyvars ty2)

  | Rec (ref (fixed,varopt)) =>
    Symbol.OrdMap.foldr (fn(ty,tvs) => TyVar.Set.union (tyvars ty, tvs)) 
      (case varopt of
        SOME (ref (RowVar rowvar)) => TyVar.Set.singleton rowvar
      | _ => TyVar.Set.empty) fixed

  | _ => 
    TyVar.Set.empty

(*----------------------------------------------------------------------*)
(* Substitute tys for vs in ty						*)
(*----------------------------------------------------------------------*)
fun appSubst [] ty = normTy ty
  | appSubst (S as ((tyvar',ty')::S')) ty =
    case normTy ty of
      Var(ref (TyVar tyvar)) =>
      if TyVar.eq(tyvar, tyvar') then ty' else appSubst S' ty
    | Con(c, tys) => Con(c, map (appSubst S) tys)
    | Fun(ty1, ty2) =>
      Fun(appSubst S ty1, appSubst S ty2)
    | Rec(ref (fixedrow, rest))=> 
      Rec(ref (Symbol.OrdMap.map (appSubst S) fixedrow, rest))
    | Ref ty => Ref (appSubst S ty)
    | Array ty => Array (appSubst S ty)
    | _ => Debug.fail "SMLTy.appSubst: non-normalised type"

(*----------------------------------------------------------------------*)
(* Identity substitution						*)
(*----------------------------------------------------------------------*)
val idSubst = []

(*----------------------------------------------------------------------*)
(* Pretty-print a type with sensible names for type variables and 	*)
(* overloading info.                                                    *)
(*----------------------------------------------------------------------*)
fun openTypeToString ty =
let
  val tvs = TyVar.Set.listItems (tyvars ty)
  val otvs = List.filter (fn tyvar =>
    case TyVar.sort tyvar of
      TyVar.Overloaded tynames => true
    | _ => false) tvs
in
  if null otvs then toString ty
  else toString ty ^ " [for " ^ 
  Pretty.simpleVec " and " 
    (fn tyvar =>
      case TyVar.sort tyvar of
        TyVar.Overloaded tynames =>
        TyVar.toString tyvar ^ "={" ^ Pretty.simpleVec "," TyName.toString 
          (TyName.Set.listItems tynames) ^ "}"
      | TyVar.Normal s =>
        TySort.toString s ^ " " ^ TyVar.toString tyvar) otvs ^ "]"
end

fun tyvarsToString [] = ""
  | tyvarsToString [tyvar] = TyVar.toString tyvar ^ " "
  | tyvarsToString (tyvar::tyvars) = 
    "(" ^ 
    foldr (fn (tyvar,s) => TyVar.toString tyvar ^ "," ^ s)  
    (TyVar.toString tyvar) tyvars ^ ") "

fun realisationToString r =
  Pretty.bigVec 0 (fn (tyname, (tyvars, ty)) =>
    tyvarsToString tyvars ^ TyName.toString tyname ^ 
      " |-> " ^ toString ty) (TyName.Map.listItemsi r)

(*----------------------------------------------------------------------*)
(* Pretty-print a datatype environment					*)
(*----------------------------------------------------------------------*)
fun DEtoString (DE : DatEnv) =
let
  fun defnToString (tyvars, stamp, CE) =
      tyvarsToString tyvars ^ TyName.toString stamp ^ 
      " = " ^ 
      Pretty.vec ("","","","",""," | ") 
        (fn (id,NONE) => JavaString.toMLString (Symbol.toJavaString id)
          | (id,SOME ty) =>
             JavaString.toMLString (Symbol.toJavaString id) 
             ^ " of " ^ toString ty) 
        (Symbol.OrdMap.listItemsi CE)
in
  Pretty.bigVec 0 defnToString (List.concat (map #2 DE)) 
end

(*----------------------------------------------------------------------*)
(* Pretty-print an exception stamp environment				*)
(*----------------------------------------------------------------------*)
fun EEtoString (EE : ExEnv) =
let
  fun defnToString (stamp, (exn,longid)) =
    Pretty.longidToString longid ^ "(" ^ Int.toString stamp ^ ")" ^
    (case exn of
      MLExn (tyopt, dyn) =>
      (case tyopt of NONE => "" | SOME ty => " of " ^ toString ty)
      ^ (if dyn then " (generative)" else "")
    | JavaExn ty => " = " ^ toString ty)
in
  Pretty.bigVec 0 defnToString (IMap.listItemsi EE)
end

(*----------------------------------------------------------------------*)
(* Return the type and position of a field in a record type		*)
(*----------------------------------------------------------------------*)
fun fieldType (ty, label) =
  case normTy ty of
    Rec (ref(m,_)) =>
    let fun find [] n = Debug.fail "SMLTy.fieldType: label not found"
          | find ((label',fldty)::rest) n =
            if Symbol.equal(label,label') 
            then (fldty, n, Symbol.OrdMap.numItems m) 
            else find rest (n+1)
    in
      find (Symbol.OrdMap.listItemsi m) 0
    end

  | _ => 
    Debug.fail ("SMLTy.fieldType: not a record type: " ^ toString ty)

(*----------------------------------------------------------------------*)
(* Return the content type of a reference type				*)
(*----------------------------------------------------------------------*)
fun fromRefType ty =
  case normTy ty of
    Ref ty => SOME ty
  | _ => NONE

fun sortTyNames tynames = 
  TyName.Set.foldl  
  (fn (tyname,s) => TySort.glb (TyName.sort tyname, s))
  TySort.anySort
  tynames

(*----------------------------------------------------------------------*)
(* Determine the sort of a type...					*)
(*----------------------------------------------------------------------*)
fun sort ignore ty = 
  case normTy ty of
    Var(ref (TyVar tyvar))    => 
    if ignore then TySort.eqSort
    else 
    (case TyVar.sort tyvar of
      TyVar.Normal s => s
    | TyVar.Overloaded tynames => sortTyNames tynames)

  | Con(tyname, tys)	      => sortConsType ignore (tyname, tys)
  | Rec(ref(fixed,varopt))    => 
    let
      val s = sortList ignore (Symbol.OrdMap.listItems fixed)
    in
      case varopt of
        SOME (ref (RowVar rowvar)) => 
        (case TyVar.sort rowvar of
          TyVar.Normal { eq = true, ... } =>
          if TySort.<= (s, TySort.eqSort)
          then TySort.eqSort
          else TySort.anySort
        | _ => 
          TySort.anySort)

      | _ => s    
    end
  | (Ref _ | Array _)	      => TySort.eqSort
  | Fun _                     => TySort.anySort
  | _                         => Debug.fail "SMLTy.sort: non-normalised type"

(*----------------------------------------------------------------------*)
(* ...and of a parameterised type constructor...			*)
(*----------------------------------------------------------------------*)
and sortConsType ignore (tyname, []) = TyName.sort tyname
  | sortConsType ignore (tyname, tys) =
  if #eq (TyName.sort tyname)
  then sortList ignore tys
  else TySort.anySort

(*----------------------------------------------------------------------*)
(* ...and of a list of types.						*)
(*----------------------------------------------------------------------*)
and sortList ignore tys =
  if List.all #eq (map (sort ignore) tys)
  then TySort.eqSort
  else TySort.anySort


type ErrorArg = string * Type
type Errors   = (Error.Error * ErrorArg list) list

(*----------------------------------------------------------------------*)
(* State used during elaboration.					*)
(*----------------------------------------------------------------------*)
structure ElabState =
struct

val entity = ref (Entity.Str, Ids.dummySym)

(* Variable supplies *)
val tyvarsupply = ref TyVar.initial
val varsupply = ref 0
val tynamesupply = ref (TyName.initial (!entity))
val exnamesupply = ref 0

val errors = ref ([] : Errors)
val constraints = ref ([] : (Type * Type list) list)
val DE = ref ([] : DatEnv)
val EE = ref (IMap.empty : ExEnv)
val CE = ref (TyName.Map.empty : ClassEnv)
val psi = ref (TyName.Map.empty : Realisation)

end


type 'a ElabResult = 
  'a * Error.Error list * DatEnv * ExEnv * ClassEnv * Realisation

(*----------------------------------------------------------------------*)
(* Generate a new type variable with the specified sort			*)
(*----------------------------------------------------------------------*)
fun freshTyVar sort = 
let
  val (tyvar, ts) = TyVar.fresh sort (!ElabState.tyvarsupply)
in
  ElabState.tyvarsupply := ts;
  tyvar
end

fun freshType () = tyVarType (freshTyVar (TyVar.Normal TySort.anySort))

fun freshConstrainedType tys =
  let
    val tyvar = freshTyVar (TyVar.Normal TySort.constrainedSort)
    val ty = tyVarType tyvar   
  in
    ElabState.constraints := (ty,tys) :: !ElabState.constraints;
    ty
  end
  

fun openRecType fields = 
let
  val rowvar = freshTyVar (TyVar.Normal TySort.anySort)
in
  Rec (ref 
  (foldr (fn ((lab,ty),row) => 
    Symbol.OrdMap.insert(row,lab,ty)) Symbol.OrdMap.empty fields,
    SOME (ref (RowVar rowvar))))
end

(*----------------------------------------------------------------------*)
(* Generate a new term variable						*)
(*----------------------------------------------------------------------*)
fun freshVar () = 
let
  val x = !ElabState.varsupply
in
  ElabState.varsupply := x+1;
  Symbol.symbol (JavaString.fromString ("$" ^ Int.toString x))
end

(*----------------------------------------------------------------------*)
(* Generate a new type name						*)
(*----------------------------------------------------------------------*)
fun freshTyName args =
let
  val (tyname, ts) = TyName.fresh args (!ElabState.tynamesupply)
in
  ElabState.tynamesupply := ts;
  tyname
end

(*----------------------------------------------------------------------*)
(* Generate a set of new recursive type names				*)
(*----------------------------------------------------------------------*)
fun freshRecTyNames args =
let
  val (tyname, ts) = TyName.freshRec args (!ElabState.tynamesupply)
in 
  ElabState.tynamesupply := ts;
  tyname
end

(*----------------------------------------------------------------------*)
(* Generate new type names for each element of a set of type names      *)
(*----------------------------------------------------------------------*)
fun makeRenaming (longid, tynames) =
let  
  fun make [] = 
      (TyName.Map.empty, TyName.Set.empty)

    | make (tyname::tynames) =
      let 
        val (r,T) = make tynames
        val (tyname', ts) = 
          TyName.freshen longid (tyname,!ElabState.tynamesupply) 
      in
        ElabState.tynamesupply := ts;
        (TyName.Map.insert(r, tyname, tyname'), TyName.Set.add(T,tyname'))
      end
in
  make (TyName.Set.listItems tynames)
end

fun getStamp () = !ElabState.tynamesupply

fun getEntity () = !ElabState.entity

fun getClassEnv () = !ElabState.CE

(*----------------------------------------------------------------------*)
(* Generate a new exception name and add it to the environment		*)
(*----------------------------------------------------------------------*)
fun freshExName (longid, tyopt : Type option, isdynamic) =
  let
    val es = !ElabState.exnamesupply
  in
    ElabState.exnamesupply := es+1;
    ElabState.EE := IMap.insert(!ElabState.EE, es, 
      (MLExn(tyopt,isdynamic),longid));
    (!ElabState.entity, es)
  end

(*----------------------------------------------------------------------*)
(* Generate a new Java exception name and add it to the environment	*)
(*----------------------------------------------------------------------*)
fun freshJavaExName (longid, ty) =
  let
    val es = !ElabState.exnamesupply
  in
    ElabState.exnamesupply := es+1;
    ElabState.EE := IMap.insert(!ElabState.EE, es, (JavaExn ty, longid));
    (!ElabState.entity, es)
  end

(*----------------------------------------------------------------------*)
(* Add an error message to the list					*)
(*----------------------------------------------------------------------*)
fun error e = 
  ElabState.errors := e :: !ElabState.errors

(*----------------------------------------------------------------------*)
(* Extend the datatype environment					*)
(*----------------------------------------------------------------------*)
fun addDE DE' =
  ElabState.DE := DE' @ !ElabState.DE

(*----------------------------------------------------------------------*)
(* Extend the class environment 					*)
(*----------------------------------------------------------------------*)
fun addClass (name,def) =
  ElabState.CE := TyName.Map.insert(!ElabState.CE, name, def)

(*----------------------------------------------------------------------*)
(* Extend the realisation              					*)
(*----------------------------------------------------------------------*)
fun addRealisation psi' =
  ElabState.psi := TyName.Map.unionWith #2 (!ElabState.psi, psi')

(*----------------------------------------------------------------------*)
(* Run an elaborator with the state initialised.			*)
(*----------------------------------------------------------------------*)
fun runelab entity CE elaborator = 
  (ElabState.tyvarsupply := TyVar.initial;
   ElabState.tynamesupply := TyName.initial entity;
   ElabState.varsupply := 1;
   ElabState.exnamesupply := 1;
   ElabState.DE := [];
   ElabState.CE := CE;
   ElabState.EE := IMap.empty;
   ElabState.psi := TyName.Map.empty;
   ElabState.entity := entity;
   ElabState.errors := [];
   ElabState.constraints := [];

  let 
    val v = elaborator ()

    val DE = !ElabState.DE
    val EE = !ElabState.EE
    val errors = !ElabState.errors
    val CE' = !ElabState.CE
    val psi = !ElabState.psi
   
    fun convertError (err, args) =
    let
      val tyvars = TyVar.Set.filter (not o TyVar.isExplicit) (foldl 
        (fn ((_,ty), tvs) => TyVar.Set.union(tyvars ty, tvs)) 
         TyVar.Set.empty args)
      val (S,_) = foldl (fn (tyvar, (S,supply)) =>
        let val (tyvar', supply') = TyVar.fresh (TyVar.sort tyvar) supply
        in
          ((tyvar, tyVarType tyvar')::S, supply')
        end) ([], TyVar.initial) (TyVar.Set.listItems tyvars)
    in
      Error.append(err, String.concat 
        (map (fn (tag, ty) => "\n    " ^ tag ^ ": " ^ openTypeToString
          (appSubst S ty)) args))
    end
    val CE' = TyName.Map.filteri 
      (fn (name,_) => not (isSome (TyName.Map.find(CE,name)))) CE'
 
    val _ = app
      (fn (ty,tys) => print ("\n" ^ toString ty ^ " is one of " ^ 
        Pretty.simpleVec "," toString tys)) (!ElabState.constraints)
  in 
    (v, map convertError errors, rev DE, EE, CE', psi)
  end)

(*----------------------------------------------------------------------*)
(* Does var occur in ty?                                                *)
(*----------------------------------------------------------------------*)
fun occurs r ty =
let
  val occ = occurs r
in
  case normTy ty of
    Var r' => r=r'

  | Con(c, tys) => 
    List.exists occ tys

  | Fun(ty1, ty2) => 
    occ ty1 orelse occ ty2

  | Rec(ref(fixed,_)) =>
    List.exists occ (Symbol.OrdMap.listItems fixed)

  | Ref ty => 
    occ ty

  | Array ty => 
    occ ty

end

val unifyCount = ref 0
(*----------------------------------------------------------------------*)
(* Unify two types imperatively and return the unified type.		*)
(* A type name environment is required so that sorts can be determined. *)
(* The location is used for error reporting.				*)
(*----------------------------------------------------------------------*)
fun unify ((loc1,tag1,ty1),(loc2,tag2,ty2)) = 
let
  val _ = if !unifyCount mod 200 = 0 then PrintManager.print "." else ()
  val _ = unifyCount := !unifyCount + 1
(*
val _ = Debug.print ("\nunify (" ^ openTypeToString ty1 ^ " , " ^ 
  openTypeToString ty2 ^ ")")
*)
fun unify' (ty1, ty2) =
  let 
    val (ty1, ty2) = (normTy ty1, normTy ty2) 
(*
    val _ = Debug.print ("\n  unify' (" ^ toString ty1 ^ " , " ^ toString ty2 ^ ")")
*)
    fun unifyError () = error 
      (Error.error (valOf loc1, "type mismatch"), [(tag1,ty1),(tag2,ty2)])

    fun circError () = error 
      (Error.error (valOf loc1, "type circularity"), [(tag1,ty1),(tag2,ty2)])

    fun eqError () = error 
      (Error.error (valOf loc1, "type does not admit equality"), [(tag1,ty1)])

    fun forceEquality ty =
    case normTy ty of
      Var (r as ref (TyVar tyvar)) =>
      (case TyVar.sort tyvar of
        TyVar.Normal {eq=true, ...} =>
        ()

      | TyVar.Normal s => 
        r := TyVar (freshTyVar (TyVar.Normal (TySort.glb (s, TySort.eqSort))))

      | TyVar.Overloaded tynames =>
        let
          val tynames' = 
            TyName.Set.filter (fn tyname => TySort.<= (TyName.sort tyname, 
              TySort.eqSort))
            tynames
        in
          if TyName.Set.numItems tynames = TyName.Set.numItems tynames'
          then ()
          else if TyName.Set.isEmpty tynames'
          then eqError ()
          else r := TyVar (freshTyVar (TyVar.Overloaded tynames'))
        end
      )
    | Con(tyname, tys) =>
      if TySort.<= (TyName.sort tyname, TySort.eqSort)
      then app forceEquality tys
      else eqError ()
    | Rec (ref r) => forceEqualityRow r
    | Ref ty => ()      (* Already is an equality type *)
    | Array ty => ()    (* Already is an equality type *)
    | Fun(ty1, ty2) =>
      eqError ()
    | _ => Debug.fail "SMLTy.forceEquality: non-normalise type"

    and forceEqualityRow (fixed, varopt) =
        (app forceEquality (Symbol.OrdMap.listItems fixed);
        case varopt of
          NONE => ()
        | SOME (r as ref (RowVar rowvar)) => 
          (case TyVar.sort rowvar of
            TyVar.Normal { eq = true, ... } => ()
          | _ => 
            r := RowVar (freshTyVar (TyVar.Normal TySort.eqSort))
          )
        | _ => Debug.fail "SMLTy.forceEqualityRow: non-normalised type")

    fun unifyRow ((fixed1, variable1), (fixed2, variable2)) =
        let
          fun unifyFixed (result1, result2) ([], []) = 
              (case (variable1, variable2) of
                (NONE,NONE) => 
                ()

              | (SOME (r1 as ref(RowVar rowvar)), NONE) => 
                (case TyVar.sort rowvar of
                  TyVar.Normal { eq = true, ... } =>
                  (app forceEquality (Symbol.OrdMap.listItems result1);
                  r1 := Row (result1, NONE))
                | _ => 
                  r1 := Row (result1, NONE))

              | (NONE, SOME (r2 as ref(RowVar rowvar))) =>
                (case TyVar.sort rowvar of
                  TyVar.Normal { eq = true, ... } =>
                  (app forceEquality (Symbol.OrdMap.listItems result2);
                  r2 := Row (result2, NONE))
                | _ => 
                  r2 := Row (result2, NONE))

              | (SOME (r1 as ref(RowVar rowvar1)), 
                 SOME (r2 as ref(RowVar rowvar2))) =>
                let
                  val eq = case (TyVar.sort rowvar1, TyVar.sort rowvar2) of
                           (TyVar.Normal { eq = true, ... }, 
                            TyVar.Normal { eq = true, ... }) =>
                           true | _ => false
                in
                  if eq 
                  then app forceEquality 
                    (Symbol.OrdMap.listItems result1 @ 
                     Symbol.OrdMap.listItems result2)
                  else ();
                  let
                    val rowvar = freshTyVar (TyVar.Normal 
                    (if eq then TySort.eqSort
                           else TySort.anySort))
                   val r = ref (RowVar rowvar)
                  in 
                    r1 := Row(result1, SOME r); 
                    r2 := Row(result2, SOME r)
                  end
                end

              | _ => Debug.fail "SMLTy.unifyFixed: non-normalised type"
              )

            | unifyFixed (result1, result2) ((lab1,ty1)::fixed1, []) =
              if isSome variable2 
              then unifyFixed (result1,Symbol.OrdMap.insert(result2,lab1,ty1)) 
                              (fixed1, [])
              else unifyError ()

            | unifyFixed (result1, result2) ([], (lab2,ty2)::fixed2) =
              if isSome variable1
              then unifyFixed (Symbol.OrdMap.insert(result1, lab2, ty2),result2) 
			      ([], fixed2)
              else unifyError ()

            | unifyFixed (result1, result2) 
                         ((lab1,ty1)::fixed1, (lab2,ty2)::fixed2) =
              case Symbol.OrdKey.compare (lab1, lab2) of
                EQUAL =>
                (unify' (ty1, ty2); 
                 unifyFixed (result1, result2) (fixed1, fixed2))

              | LESS =>
                if isSome variable2
                then unifyFixed (result1,Symbol.OrdMap.insert(result2, lab1, ty1)) 
                                (fixed1, (lab2,ty2)::fixed2)
                else unifyError ()

              | GREATER =>
                if isSome variable1
                then unifyFixed (Symbol.OrdMap.insert(result1, lab2, ty2), result2)
                                ((lab1,ty1)::fixed1, fixed2)
                else unifyError ()
        in
          unifyFixed (fixed1, fixed2) 
                     (Symbol.OrdMap.listItemsi fixed1, 
                      Symbol.OrdMap.listItemsi fixed2)
        end       
  in 
    case (ty1, ty2) of
    (Var (r1 as ref (TyVar tyvar1)), Var (r2 as ref (TyVar tyvar2))) =>
    if r1 = r2 then ()
    else
      (case (TyVar.sort tyvar1, TyVar.sort tyvar2) of
        (TyVar.Normal s1, TyVar.Normal s2) =>
        if TySort.<= (s1, s2) then r2 := Type ty1
        else
        if TySort.<= (s2, s1) then r1 := Type ty2
        else 
          let 
            val tyvar = freshTyVar (TyVar.Normal (TySort.glb (s1, s2)))
            val _ = print ("\n unif tyvar " ^ TyVar.toString tyvar)
          in
            r1 := TyVar tyvar; r2 := TyVar tyvar
          end

      | (TyVar.Overloaded tynames1, TyVar.Overloaded tynames2) =>
        if TyName.Set.isSubset (tynames1, tynames2) then r2 := Type ty1
        else
        if TyName.Set.isSubset (tynames2, tynames1) then r1 := Type ty2
        else
        let
          val tynames = TyName.Set.intersection (tynames1, tynames2)
        in
          if TyName.Set.isEmpty tynames
          then unifyError ()
          else 
            let val tyvar = freshTyVar (TyVar.Overloaded tynames)
            in 
              r1 := TyVar tyvar; r2 := TyVar tyvar 
            end
        end

      | (TyVar.Normal s, TyVar.Overloaded tynames) =>
        let
          val tynames' = 
            TyName.Set.filter (fn tyname => TySort.<= (TyName.sort tyname, s))
            tynames
        in
          if TyName.Set.numItems tynames = TyName.Set.numItems tynames'
          then r1 := Type ty2
          else if TyName.Set.isEmpty tynames'
          then unifyError ()
          else 
          let
            val tyvar = freshTyVar (TyVar.Overloaded tynames')
          in
            r1 := TyVar tyvar; r2 := TyVar tyvar
          end
        end

      | (TyVar.Overloaded tynames, TyVar.Normal s) =>
        let
          val tynames' = 
            TyName.Set.filter (fn tyname => TySort.<= (TyName.sort tyname, s))
            tynames
        in
          if TyName.Set.numItems tynames = TyName.Set.numItems tynames'
          then r2 := Type ty1
          else if TyName.Set.isEmpty tynames'
          then unifyError ()
          else 
          let
            val tyvar = freshTyVar (TyVar.Overloaded tynames')
          in
            r1 := TyVar tyvar; r2 := TyVar tyvar
          end
        end)

  | (Var (r as ref (TyVar tyvar)), _) => 
    if occurs r ty2
    then circError ()
    else 
    (case TyVar.sort tyvar of
      TyVar.Normal s =>
      if TySort.<= (sort false ty2, s)
      then r := Type ty2
      else 
      if not (#eq s) then unifyError ()
      else 
        (forceEquality ty2; r := Type ty2)

    | TyVar.Overloaded tynames =>
      case ty2 of
        Con(tyname, []) =>
        if TyName.Set.member(tynames, tyname)
        then r := Type ty2
        else unifyError ()

      | _ => unifyError ()
    )

  | (_, Var r) =>
    unify' (ty2, ty1)

  | (Con(tyname1, tys1), Con(tyname2, tys2)) =>
    if TyName.eq(tyname1, tyname2)
    then app unify' (ListPair.zip (tys1, tys2))
    else unifyError ()

  | (Rec (r1 as ref row1), Rec (r2 as ref row2)) => 
    if r1=r2 then ()
    else unifyRow (row1, row2)

  | (Ref ty1, Ref ty2) => unify' (ty1, ty2)

  | (Array ty1, Array ty2) => unify' (ty1, ty2)

  | (Fun(ty1, ty2), Fun(ty3, ty4)) =>
    (unify' (ty1,ty3); unify' (ty2,ty4))

  | (Con(tyname, _), _) =>
    unifyError ()

  | (_, Con(tyname, _)) =>
    unifyError ()

  | _ => unifyError ()

  end
in
  unify' (ty1, ty2); (* Debug.print ("\n  = " ^ openTypeToString ty1); *)
  ty1
end

(*----------------------------------------------------------------------*)
(* Type names in a type							*)
(*----------------------------------------------------------------------*)
fun tynames ty =
  case normTy ty of
    Con(tyname, tys) =>
    TyName.Set.union (TyName.Set.singleton tyname, 
      foldr (fn (ty, ns) => TyName.Set.union(tynames ty, ns)) 
        TyName.Set.empty tys)

  | Fun(ty1, ty2) =>
    TyName.Set.union (tynames ty1, tynames ty2)

  | Ref ty =>
    tynames ty

  | Array ty =>
    tynames ty

  | Rec(ref (fixed, _)) =>
    Symbol.OrdMap.foldr (fn (ty, ns) => TyName.Set.union (tynames ty, ns)) 
      TyName.Set.empty fixed

  | Var (ref (TyVar tyvar)) =>
    (case TyVar.sort tyvar of
      TyVar.Overloaded tynames => tynames
    | _ => TyName.Set.empty)

  | _ =>
    TyName.Set.empty

(*----------------------------------------------------------------------*)
(* Resolve overloaded type variables in a type and check that no	*)
(* open record types remain.						*)
(* Return the free type variables remaining				*)
(*----------------------------------------------------------------------*)
fun resolve loc excludeset ty =
(
  case normTy ty of
    Var(r as ref (TyVar tyvar)) =>
    (case TyVar.sort tyvar of
    (* Hack for Java constrained types *)
      TyVar.Normal { constrained = true, ... } =>
      TyVar.Set.empty

    | TyVar.Normal _ => 
      TyVar.Set.singleton tyvar

    | TyVar.Overloaded tynames =>
      if TyVar.Set.member (excludeset, tyvar)
      then TyVar.Set.singleton tyvar

      else
      let
        val tyname = TyName.default tynames
      in
        r := Type (Con(tyname, [])); 
        (if TyName.Set.numItems tynames > 1 
        andalso Controls.isOn "defaultsWarning"
        then error (Error.warning (loc, "default type assumed: " ^ 
          TyName.toString tyname), [])
        else ());
        TyVar.Set.empty
      end
    )

  | Con(c, tys) => 
    resolveList loc excludeset tys

  | Fun(ty1, ty2) => 
    TyVar.Set.union 
    (resolve loc excludeset ty1, resolve loc excludeset ty2)

  | Rec (ref row) => 
    resolveRow loc excludeset row

  | (Ref ty | Array ty) => 
    resolve loc excludeset ty

  | _ => 
    Debug.fail "SMLTy.resolve: non-normalised type"
)

and resolveRow loc excludeset (fixed,NONE) = 
    resolveList loc excludeset (Symbol.OrdMap.listItems fixed)

  | resolveRow loc excludeset (r as (fixed,SOME (ref (RowVar rowvar)))) =
    if TyVar.Set.member(excludeset, rowvar)
    then 
      TyVar.Set.add(resolveList loc excludeset 
        (Symbol.OrdMap.listItems fixed), rowvar)
    else
      (error (Error.error (loc, "unresolved flex record type"),
        [("type", Rec (ref r))]);
      TyVar.Set.empty)

  | resolveRow loc excludeset row =
    Debug.fail "SMLTy.resolveRow: non-normalised type"
    

and resolveList loc excludeset [] = TyVar.Set.empty

  | resolveList loc excludeset (ty::tys) = 
    TyVar.Set.union 
    (resolve loc excludeset ty, resolveList loc excludeset tys)

(*----------------------------------------------------------------------*)
(* Test two types for equality                                          *)
(*----------------------------------------------------------------------*)
fun eq (ty1,ty2) = 
case (normTy ty1, normTy ty2) of
  
    (Var (ref (TyVar tyvar1)), Var (ref (TyVar tyvar2))) =>
    TyVar.eq(tyvar1,tyvar2)

  | (Con(c1, tys1), Con(c2, tys2)) => 
    TyName.eq(c1, c2) andalso Eq.list eq (tys1, tys2)

  | (Fun(ty1, ty1'), Fun(ty2, ty2')) => 
    eq(ty1,ty2) andalso eq(ty1',ty2')

  | (Rec (ref (fixed1,open1)), Rec (ref (fixed2,open2))) =>
    (isSome open1 = isSome open2)
    andalso Eq.list (fn ((lab1,ty1), (lab2,ty2)) =>
      Symbol.equal(lab1,lab2) andalso eq(ty1,ty2)) 
      (Symbol.OrdMap.listItemsi fixed1, Symbol.OrdMap.listItemsi fixed2)

  | ((Ref ty1, Ref ty2) | (Array ty1, Array ty2)) => 
    eq (ty1, ty2)

  | _ => 
    false

(*----------------------------------------------------------------------*)
(* Anti-unify two types							*)
(* Currently limited to dealing with base type clashes only.            *)
(*----------------------------------------------------------------------*)
fun antiunify (ty1, ty2) =
let
  exception AntiUnify
  fun replace (S, ty1, ty2) =  
  let
    fun loop [] =
        let
          val v = freshTyVar (TyVar.Normal TySort.anySort)
        in
          ((ty1, ty2, v)::S, tyVarType v)
        end

      | loop ((ty1', ty2', v)::S') =
        if eq(ty1, ty1') andalso eq(ty2, ty2')
        then (S, tyVarType v) else loop S'
  in
    loop S
  end
      
  fun au (S, ty1, ty2) =
    case (normTy ty1, normTy ty2) of
      (Fun(lty1, rty1), Fun (lty2, rty2)) => 
      let
        val (S, lty) = au (S, lty1, lty2)
        val (S, rty) = au (S, rty1, rty2)
      in
        (S, Fun(lty, rty))
      end

    | (Ref ty1, Ref ty2) => 
      let
        val (S, ty) = au (S, ty1, ty2)
      in
        (S, Ref ty)
      end

    | (Array ty1, Array ty2) => 
      let
        val (S, ty) = au (S, ty1, ty2)
      in
        (S, Array ty)
      end

    | (Con(tyname1, []), Con(tyname2, [])) =>
      if TyName.eq(tyname1, tyname2) 
      then (S, ty1)
      else replace (S, ty1, ty2)

    | ((Con(_, []), Var _) | (Var _, Con(_, []))) =>
      replace (S, ty1, ty2)
      
    | (Con(tyname1, tys1), Con(tyname2, tys2)) =>
      if TyName.eq(tyname1, tyname2)
      then
      let
        val (S, tys) = aus (S, tys1, tys2)
      in
        (S, Con(tyname1, tys))
      end else raise AntiUnify

    | (Rec (ref (fixed1, NONE)), Rec (ref (fixed2, NONE))) =>
      let
        val (labs1, tys1) = ListPair.unzip (Symbol.OrdMap.listItemsi fixed1)
        val (labs2, tys2) = ListPair.unzip (Symbol.OrdMap.listItemsi fixed2)
      in
        if Eq.list Symbol.equal (labs1, labs2)
        then 
        let
          val (S, tys) = aus (S, tys1, tys2)
          val fixed = ListPair.foldr (fn (lab, ty, f) =>
            Symbol.OrdMap.insert(f, lab, ty)) Symbol.OrdMap.empty (labs1, tys)
        in
          (S, Rec (ref (fixed, NONE)))
        end       
        else raise AntiUnify
      end

    | (Var (ref (TyVar tyvar1)), Var (ref (TyVar tyvar2))) =>
      if TyVar.eq(tyvar1, tyvar2) then (S, ty1)
      else replace (S, ty1, ty2)
      
    | _ => 
      raise AntiUnify

  and aus (S, [], []) = 
      (S, [])

    | aus (S, ty1::tys1, ty2::tys2) =
      let
        val (S, ty) = au (S, ty1, ty2)
        val (S, tys) = aus (S, tys1, tys2)
      in
        (S, ty::tys)
      end

    | aus _ = raise AntiUnify
in
  let
    val (S, ty) = au ([], ty1, ty2)
  in
    SOME (foldr (fn ((ty1, ty2, v), m) => TyVar.Map.insert(m, v, (ty1,ty2)))
      TyVar.Map.empty S, ty)
  end handle AntiUnify => NONE
end

fun antiunifylist [] = NONE
  | antiunifylist [ty] = SOME (TyVar.Map.empty, ty)
  | antiunifylist (ty::tys) =
    case antiunifylist tys of
      NONE => NONE
    | SOME (S1, ty') =>
      case antiunify (ty, ty') of
        NONE => NONE
      | SOME (S2, ty') =>
        let
          val S3 = TyVar.Map.mapi
            (fn (tyvar, (ty1,ty2)) =>
              case normTy ty2 of
                Var (ref (TyVar tyvar)) =>
                (case TyVar.Map.find(S1, tyvar) of
                  NONE => ty1 :: map (fn _ => ty2) tys
                | SOME tys => ty1 :: tys)
              | _ => ty1 :: map (fn _ => ty2) tys) S2              
        in
          SOME (S3, ty')
        end

(*----------------------------------------------------------------------*)
(* Partial order on types.                                              *)
(*----------------------------------------------------------------------*)
fun compare (ty1,ty2) = 
let
  val ty1 = normTy ty1
  val ty2 = normTy ty2
  fun num ty =  
  case ty of
    Var _ => 0
  | Con _ => 1
  | Fun _ => 2
  | Rec _ => 3
  | Ref _ => 4
  | Array _ => 5
in
  case Int.compare(num ty1, num ty2) of
    EQUAL =>
    (case (ty1, ty2) of  
      (Var (ref (TyVar tyvar1)), Var (ref (TyVar tyvar2))) =>
      TyVar.Set.Key.compare(tyvar1,tyvar2)

    | (Con(c1, tys1), Con(c2, tys2)) => 
      (case TyName.Set.Key.compare(c1, c2) of
        EQUAL => Compare.list compare (tys1,tys2)
      | other => other)

    | (Fun(ty1, ty1'), Fun(ty2, ty2')) => 
      Compare.list compare ([ty1,ty1'], [ty1',ty2'])

    | (Rec (ref (fixed1,open1)), Rec (ref (fixed2,open2))) =>
      (case Compare.option (fn _ => EQUAL) (open1,open2) of
        EQUAL => 
        Compare.list (fn ((lab1,ty1),(lab2,ty2)) =>
          case Symbol.OrdKey.compare(lab1,lab2) of
            EQUAL => compare (ty1,ty2)
          | other => other) 
            (Symbol.OrdMap.listItemsi fixed1, Symbol.OrdMap.listItemsi fixed2)
      | other => other)

    | ((Ref ty1, Ref ty2) | (Array ty1, Array ty2)) => 
      compare (ty1, ty2)

    | _ => Debug.fail "SMLTy.compare")

  | other =>
    other
end

(*----------------------------------------------------------------------*)
(* Apply a realisation psi to the tynames in ty.              		*)
(*----------------------------------------------------------------------*)
fun appRealisation psi ty =
  case normTy ty of
    Con(tyname, tys) => 
    (case TyName.Map.find(psi, tyname) of
      NONE => 
      Con(tyname, map (appRealisation psi) tys)
    | SOME (tyvars, ty) =>
      appSubst (ListPair.zip(tyvars, map (appRealisation psi) tys)) ty)
  | Fun(ty1, ty2) =>
    Fun(appRealisation psi ty1, appRealisation psi ty2)
  | Rec (ref row) => 
    Rec (ref (appRealisationRow psi row))
  | Ref ty => 
    Ref (appRealisation psi ty)
  | Array ty => 
    Array (appRealisation psi ty)
  | Var (ref (TyVar tv)) =>
    Var (ref (TyVar tv))
  | _ => 
    Debug.fail "SMLTy.appRealisation: non-normalised type"

and appRealisationRow psi (fixed,openrec) =
    (Symbol.OrdMap.map (appRealisation psi) fixed, openrec)

(*----------------------------------------------------------------------*)
(* Rename the type names in ty.              		                *)
(*----------------------------------------------------------------------*)
fun renameType rn ty =
  case normTy ty of
    Var (ref (TyVar tv)) => ty
  | Con(tyname, tys) => 
    Con(TyName.rename rn tyname, map (renameType rn) tys)
  | Fun(ty1, ty2) =>
    Fun(renameType rn ty1, renameType rn ty2)
  | Rec (ref row) => 
    Rec (ref (renameRow rn (normRow row)))
  | Ref ty => 
    Ref (renameType rn ty)
  | Array ty => 
    Array (renameType rn ty)
  | _ => 
    Debug.fail "SMLTy.renameType: non-normalised type"

and renameRow rn (fixed,openrec) = 
    (Symbol.OrdMap.map (renameType rn) fixed, openrec)

fun appRenamingDE r =
  (ElabState.DE := map (fn (b, dds) =>
      (b, map (fn (tyvars, tyname, CE) =>
        (tyvars, TyName.rename r tyname, 
           Symbol.OrdMap.map (Option.map (renameType r))
           CE)) dds)) (!ElabState.DE);

   ElabState.CE := 
    TyName.Map.foldli
      (fn (tyname, ((flags,superopt,ints),fieldinfo,methodinfo), CE) =>
      TyName.Map.insert(CE, TyName.rename r tyname, 
        ((flags, Option.map (renameType r) superopt, map (renameType r) ints),
         map (fn (name, flags, ty, v) => (name, flags, renameType r ty, v))
           fieldinfo,
         map (fn (name, flags, tys, tyopt) => (name, flags, 
           map (renameType r) tys, Option.map (renameType r) tyopt)) 
           methodinfo))) TyName.Map.empty (!ElabState.CE)
  )
  

(*----------------------------------------------------------------------*)
(* Match two types against each other: given two types                  *)
(* ty1 and ty2 return a substitution such that S(ty1) = ty2.            *)
(* If update is set then actually apply the matching substitution.      *)
(*----------------------------------------------------------------------*)
fun match update (ty1,ty2) = 
let 
  fun matchList S ([], []) = SOME S

    | matchList S (ty1::tys1, ty2::tys2) = 
      (case matchList S (tys1, tys2) of
        NONE => NONE
      | SOME S1 => matchTy S1 (ty1, ty2))

    | matchList S _ = Debug.fail "SMLTy.matchList: unequal lengths"

  and matchRow S ([], []) = SOME S

    | matchRow S ((lab1,ty1)::row1, (lab2,ty2)::row2) = 
      if Symbol.equal(lab1,lab2)
      then case matchRow S (row1, row2) of
        NONE => NONE
      | SOME S1 => matchTy S1 (ty1, ty2)
      else NONE

   | matchRow S _ = NONE

  and matchTy S (ty1, ty2) =
  case (normTy ty1, normTy ty2) of

    (Var (tyref as ref (TyVar tyvar)), ty) =>
    (case TyVar.Map.find(S, tyvar) of
      SOME ty' => if eq(ty, ty') then SOME S else NONE
    | NONE =>
      (case TyVar.sort tyvar of
        TyVar.Overloaded tynames => 
        (case ty of
          Con(tyname, []) =>
          if TyName.Set.member(tynames, tyname)
          then 
          (if update then tyref := Type ty else ();
           SOME (TyVar.Map.insert(S, tyvar, ty)))
          else NONE
         | _ => NONE)

      | TyVar.Normal s => 
        if TySort.<=(sort false ty, s)
        then 
          (if update then tyref := Type ty else ();
           SOME (TyVar.Map.insert(S, tyvar, ty)))
        else NONE
      )
    )

  | (Con(t1, tys1), Con(t2, tys2)) => 
    if TyName.eq(t1, t2) 
    then matchList S (tys1, tys2)
    else NONE

  | (Fun(ty1, ty1'), Fun(ty2, ty2')) => 
    matchList S ([ty1,ty1'], [ty2,ty2'])

  | (Rec (ref (fixed1,_)), Rec (ref (fixed2,_))) =>
    matchRow S (Symbol.OrdMap.listItemsi fixed1, 
      Symbol.OrdMap.listItemsi fixed2)

  | ((Ref ty1, Ref ty2) | (Array ty1, Array ty2)) => 
    matchTy S (ty1, ty2)
    
  | _ => 
    NONE
in
  matchTy TyVar.Map.empty (ty1, ty2)
end

fun fromConsType ty =
  case normTy ty of 
    Con(tyname, tys) => SOME (tys, tyname)
  | _ => NONE

fun fromFunType ty =
  case normTy ty of 
    Fun(ty1, ty2) => SOME (ty1, ty2)
  | _ => NONE

(*----------------------------------------------------------------------*)
(* Translate a type 							*)
(* Note: we `close up' open record types as any remaining open types    *)
(* must be internal and hence can be arbitrarily instantiated.          *)
(*----------------------------------------------------------------------*)
fun trans (p as (var, con, tuple, funty, refty, arrayty)) ty =
let val t = trans p
in
  case normTy ty of 
    Var(ref (TyVar tyvar)) => var tyvar
  | Con(tyname, tys) => 
    con(tyname, map t tys)
  | Fun(ty1, ty2) => funty (t ty1, t ty2)
  | Ref ty => refty (t ty)
  | Array ty => arrayty (t ty)
  | Rec(ref (fixed,_)) => 
    tuple (map (fn (l,ty) => (l,t ty)) (Symbol.OrdMap.listItemsi fixed))
  | _ => Debug.fail "SMLTy.trans: non-normalised type"
end

(*----------------------------------------------------------------------*)
(* Translate a realisation						*)
(*----------------------------------------------------------------------*)
fun transRealisation f psi =
  map (fn (tyname, (tyvars, ty)) => f (tyvars, tyname, ty)) 
      (TyName.Map.listItemsi psi)


(*----------------------------------------------------------------------*)
(* Look up a constructor in a constructor environment.			*)
(* Hack up bools so that false |-> 0 and true |-> 1.                    *)
(*----------------------------------------------------------------------*)
fun findCon (tyname,CE,con) =
let
  fun find ([], n) = 
      Debug.fail 
      ("SMLTy.findCon: constructor " ^ JavaString.toMLString (Symbol.toJavaString con) ^ " not found")

    | find ((con', tyopt)::CE, n) =
      if Symbol.equal(con,con') then (n, tyopt)
      else find (CE, n+1)
in
  if TyName.eq(tyname, TyName.boolTyName)
  then
    if Symbol.equal(con, Ids.falseSym)
    then (0, NONE)
    else if Symbol.equal(con, Ids.trueSym)
    then (1, NONE)
    else Debug.fail "SMLTy.findCon: invalid bool"
  else
    find (Symbol.OrdMap.listItemsi CE, 0)
end

end (* of struct *)


