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
(* MIL types and operations.         	                                *)
(*======================================================================*)
structure MILTy :> MILTY =
struct

(*----------------------------------------------------------------------*)
(* A hash cell is a reference cell with an associated hash number.	*)
(*----------------------------------------------------------------------*)
type 'a HashCell = (word * Var.Set.set * ISet.set * 'a) ref

(*----------------------------------------------------------------------*)
(* Types								*)
(*----------------------------------------------------------------------*)
datatype TypeC =     
  Var       of Var.Var                  (* Term-bound tyvar (from supply) *)
| Deb       of int                      (* Type-bound tyvar (de Brujn index)*)
| Java      of Types.base_type          (* Java base type *)
| Prod      of Type list                (* Product *)
| Sum       of Type list list           (* Arity-raised sum *)
| Con       of Type list                (* Universal constructor *)
| Exn       of MILExn.Exn * Type list   (* Exception *)
| Refty     of Type                     (* Mutable type *)
| Vector    of Type                     (* Immutable vector type *)
| Array     of Type                     (* Mutable array type *)
| Arrow     of Type list * CmpType      (* Arity-raised arrow type *)
| Forall    of Kind list * Type         (* Polymorphic type *)
| Mu        of int * Type list          (* i'th projection from fixpoint *)
| Closure   of int option * Type list   (* Closure type *)
| App       of Type * Type list         (* Type constructor application *)
| Abs       of Kind list * Type         (* Type constructor abstraction *)
| Tyname    of TyName.TyName            (* Imported type *)

(*----------------------------------------------------------------------*)
(* Kinds								*)
(*----------------------------------------------------------------------*)
and Kind = 
  Any                                   (* All type *)
| Eq                                    (* All types admitting equality *)
| Bound of Type                         (* All types <= bound *)

(*----------------------------------------------------------------------*)
(* Arity-raised computation type with effect info.			*)
(*----------------------------------------------------------------------*)
and CmpType = CmpType of Effect.Effect * Type list

(*----------------------------------------------------------------------*)
(* External, hash-consed view of a type.				*)
(*----------------------------------------------------------------------*)
withtype Type = TypeC HashCell

(*----------------------------------------------------------------------*)
(* Internal equality on types...					*)
(*----------------------------------------------------------------------*)
fun eqTyc (tyc1, tyc2) =
case (tyc1, tyc2) of
  (Var i1, Var i2) => i1=i2
| (Deb i1, Deb i2) => i1=i2
| (Java j1, Java j2) => Types.base_type_equal(j1,j2)
| ((Prod tys1, Prod tys2) | (Con tys1, Con tys2)) => tys1=tys2
| (Sum tyss1, Sum tyss2) => tyss1=tyss2
| (Exn(e1, _), Exn(e2, _)) => MILExn.eq (e1,e2)
| ((Refty ty1, Refty ty2) | (Array ty1, Array ty2) | (Vector ty1, Vector ty2))
  => ty1=ty2
| (Arrow a1, Arrow a2) => a1=a2
| (Forall a1, Forall a2) => a1=a2
| (Mu a1, Mu a2) => a1=a2
| (Closure a1, Closure a2) => a1=a2
| (App a1, App a2) => a1=a2
| (Abs a1, Abs a2) => a1=a2
| (Tyname tn1, Tyname tn2) => TyName.eq(tn1, tn2)
| _ => false

(*----------------------------------------------------------------------*)
(* Term-bound type variables in a type					*)
(*----------------------------------------------------------------------*)
fun tyvars (ty as ref (_,tyvars,_,_)) = tyvars
and tyvarsTyc tyc =
case tyc of
  Var x => Var.Set.singleton x

| (Deb _ | Java _ | Tyname _) => Var.Set.empty

| (Prod tys | Con tys | Mu(_,tys) | Exn(_,tys) | Closure(_,tys)) => 
  foldl Var.Set.union Var.Set.empty (map tyvars tys)

| Sum tyss => 
  foldl Var.Set.union Var.Set.empty (map tyvars (List.concat tyss))

| (Refty ty | Array ty | Vector ty) => 
  tyvars ty

| (Arrow(tys, CmpType(_,tys'))) =>
  foldl Var.Set.union (foldl Var.Set.union Var.Set.empty (map tyvars tys))
  (map tyvars tys')

| (Forall(kinds,ty) | Abs(kinds,ty)) =>
  foldl Var.Set.union (tyvars ty) (map tyvarsKind kinds)

| (App(ty,tys)) =>
  foldl Var.Set.union (tyvars ty) (map tyvars tys)

and tyvarsKind (Bound ty) = tyvars ty
  | tyvarsKind _ = Var.Set.empty

(*----------------------------------------------------------------------*)
(* Type-bound type variables in a type					*)
(*----------------------------------------------------------------------*)
fun dtyvars (ty as ref (_,_,tyvars,_)) = tyvars
and dtyvarsTyc tyc =
case tyc of
  Deb i => ISet.singleton i

| (Var _ | Java _ | Tyname _) => ISet.empty

| (Prod tys | Con tys | Exn(_,tys) | Closure(_,tys)) => 
  foldl ISet.union ISet.empty (map dtyvars tys)

| Sum tyss => 
  foldl ISet.union ISet.empty (map dtyvars (List.concat tyss))

| (Refty ty | Array ty | Vector ty) => 
  dtyvars ty

| (Arrow(tys, CmpType(_,tys'))) =>
  foldl ISet.union (foldl ISet.union ISet.empty (map dtyvars tys))
  (map dtyvars tys')

| (Forall(kinds,ty) | Abs(kinds,ty)) =>
  let
    val n = length kinds
    val s = ISet.map (fn i => i-n) (ISet.filter (fn i => i >= n) (dtyvars ty))
  in
    foldl ISet.union s (map dtyvarsKind kinds)
  end

| (Mu(_,tys)) =>
  let
    val n = length tys
    fun dtyvars' ty = 
    let
      val s = ISet.filter (fn i => i >= n) (dtyvars ty)
    in
      ISet.map (fn i => i-n) s
    end
  in
    foldl ISet.union ISet.empty (map dtyvars' tys) 
  end

| (App(ty,tys)) =>
  foldl ISet.union (dtyvars ty) (map dtyvars tys)

and dtyvarsKind (Bound ty) = dtyvars ty
  | dtyvarsKind _ = ISet.empty

(*----------------------------------------------------------------------*)
(* Hash-consing functions						*)
(*----------------------------------------------------------------------*)
structure Weak = SMLofNJ.Weak
val itow = Word.fromInt
val wtoi = Word.toIntX
val andb = Word.andb

val N = 16384
val P = 0w509

(* The hash table itself *)
val tyTable : Type (* Weak.weak *) list Array.array = Array.array(N, [])

fun combine (x,y) = x + y * P

(*----------------------------------------------------------------------*)
(* Extract the hash code from a cell.					*)
(*----------------------------------------------------------------------*)
fun code ((ref(i, _, _, _)) : Type) = i

(*----------------------------------------------------------------------*)
(* outTy : Type -> TypeC						*)
(*----------------------------------------------------------------------*)
fun outTy (r : Type as ref (_, _, _, tyc)) = tyc

(*----------------------------------------------------------------------*)
(* Return a hash code for a Java base type				*)
(*----------------------------------------------------------------------*)
fun hashJava ty =
case ty of
  Types.BOOLEAN => 0w1
| Types.BYTE => 0w2
| Types.CHAR => 0w3
| Types.DOUBLE => 0w4
| Types.FLOAT => 0w5
| Types.INT => 0w6
| Types.LONG => 0w7
| Types.SHORT => 0w8
| Types.CLASS c => JavaString.hash.hashVal (ClassHandle.name c)

(*----------------------------------------------------------------------*)
(* Return a hash code for a MIL type					*)
(*----------------------------------------------------------------------*)
fun hashTy tyc =
case tyc of
  Var i => combine (0w10, itow (i+1))
| Java j => combine (0w11, hashJava j)
| Prod tys => combine (0w12, hashTys tys)
| Sum tyss => combine (0w13, hashTyss tyss)
| Con tys => combine (0w14, hashTys tys)
| Exn (exn, tys) => combine (0w15, MILExn.hash exn)
| Refty ty => combine (0w16, code ty)
| Vector ty => combine (0w17, code ty)
| Array ty => combine (0w18, code ty)
| Arrow (tys, CmpType(eff, tys')) => 
  let
    val h1 = hashTys tys
    val h2 = hashTys' tys'
  in
    combine (0w19, combine (eff + 0w1, combine(h1,h2)))
  end
| Forall(kinds, ty) => combine (0w20, combine (code ty, hashKinds kinds))
| Closure(NONE, tys) => combine (0w21, hashTys tys)
| Closure(SOME i, tys) => combine (0w22, combine (itow i, hashTys tys))
| App(ty, tys) => combine (0w23, hashTys (ty::tys))
| Abs(kinds, ty) => combine (0w24, combine (code ty, hashKinds kinds))
| Mu(i, defs) => combine(0w25, combine (itow (i+1), hashTys defs))
| Tyname tn => combine(0w26, TyName.hash tn)
| Deb i => combine (0w27, itow (i+1))

and hashTys [] = 0w50
  | hashTys (ty::tys) = combine (0w51, combine (code ty, hashTys tys))

and hashTys' [] = 0w87
  | hashTys' (ty::tys) = combine (0w74, combine (code ty, hashTys' tys))

and hashTyss [] = 0w52
  | hashTyss (tys::tyss) = combine (0w53, combine (hashTys tys, hashTyss tyss))

and hashKinds [] = 0w54
  | hashKinds (Any::kinds) = combine (0w55, hashKinds kinds)
  | hashKinds (Eq::kinds) = combine (0w56, hashKinds kinds)
  | hashKinds (Bound ty::kinds) = combine (0w57, 
    combine (code ty, hashKinds kinds))

(*----------------------------------------------------------------------*)
(* injTy : TypeC -> Type						*)
(*----------------------------------------------------------------------*)
fun injTy tyc =
let
  val h = hashTy tyc
  val i = wtoi(andb(h, itow(N-1)))
  val l = Array.sub(tyTable, i)

  fun lookup [] =
      let 
        val r = ref (h, tyvarsTyc tyc, dtyvarsTyc tyc, tyc)
      in
        Array.update (tyTable, i, r :: l); 
        r
      end
 
    | lookup ((r as ref(h', _, _, tyc'))::rest) =
        if h'=h andalso eqTyc (tyc', tyc)
        then r
        else lookup rest      
in
  lookup l : Type
end

(*----------------------------------------------------------------------*)
(* Comparison function used to construct maps on types.			*)
(*----------------------------------------------------------------------*)
fun compare (ty1:Type as ref(h1,_,_,_), ty2:Type as ref(h2,_,_,_)) =
  if ty1=ty2 then EQUAL
  else
  case Word.compare(h1,h2) of
    EQUAL =>
    let
      val i = wtoi(andb(h1, itow(N-1)))
      fun loop [] = Debug.fail "MILTy.compare"
        | loop (w::rest) =
          case (* Weak.strong *) w of
            (* NONE => loop rest
          | SOME *) r =>
            if ty1=r then LESS
            else if ty2=r then GREATER
            else loop rest
    in
      loop (Array.sub(tyTable, i))
    end

  | other => other

(*----------------------------------------------------------------------*)
(* Ordered map on types							*)
(*----------------------------------------------------------------------*)
structure Map = MapFn(struct type ord_key = Type val compare = compare end)
structure PairMap = MapFn(struct type ord_key = Type*Type
  val compare = fn ((ty1,ty2),(ty1',ty2')) =>
    case compare(ty1,ty1') of
      EQUAL => compare(ty2,ty2')
    | other => other
  end)

(*----------------------------------------------------------------------*)
(* Return details of the hash table.         				*)
(*----------------------------------------------------------------------*)
fun stats () = 
  Array.foldl (fn (l, a) => map (fn ty => (code ty, ty)) l :: a) 
  [] tyTable

(*----------------------------------------------------------------------*)
(* Constructors								*)
(*----------------------------------------------------------------------*)
val prod = injTy o Prod 
and sum = injTy o Sum 
and arrow = injTy o Arrow 
and vector = injTy o Vector
and java = injTy o Java 
and tyvar = injTy o Var 
and refty = injTy o Refty 
and array = injTy o Array
and mu = injTy o Mu 
and con = injTy o Con 
and exn = injTy o Exn 
and closure = injTy o Closure
and cmp = CmpType 
and tyname = injTy o Tyname
and deb = injTy o Deb

fun debforall ([],ty) = ty
  | debforall p = injTy (Forall p)

fun abs ([], ty) = ty
  | abs (tyvars, ty) = injTy (Abs(tyvars, ty))

fun app (ty, []) = ty
  | app (ty, tys) = injTy (App(ty, tys))

fun noeffect tys = CmpType(Effect.none, tys)

(*----------------------------------------------------------------------*)
(* Deconstructors							*)
(*----------------------------------------------------------------------*)
fun fromProd ty =
  case outTy ty of
    Prod a => SOME a
  | _ => NONE

fun fromSum ty =
  case outTy ty of
    Sum a => SOME a
  | _ => NONE

fun fromArrow ty =
  case outTy ty of
    Arrow a => SOME a
  | _ => NONE

fun fromRefty ty =
  case outTy ty of
    Refty a => SOME a
  | _ => NONE

fun fromVector ty =
  case outTy ty of
    Vector a => SOME a
  | _ => NONE

fun fromArray ty =
  case outTy ty of
    Array a => SOME a
  | _ => NONE

fun fromJava ty =
  case outTy ty of
    Java a => SOME a
  | _ => NONE

fun fromTyvar ty =
  case outTy ty of
    Var a => SOME a
  | _ => NONE

fun fromDeb ty =
  case outTy ty of
    Deb a => SOME a
  | _ => NONE

fun fromForall ty =
  case outTy ty of
    Forall a => SOME a
  | _ => NONE

fun fromMu ty =
  case outTy ty of
    Mu a => SOME a
  | _ => NONE

fun fromCon ty =
  case outTy ty of
    Con a => SOME a
  | _ => NONE

fun fromExn ty =
  case outTy ty of
    Exn a => SOME a
  | _ => NONE

fun fromClosure ty =
  case outTy ty of
    Closure a => SOME a
  | _ => NONE

fun fromTyname ty =
  case outTy ty of
    Tyname a => SOME a
  | _ => NONE

fun fromCmp (CmpType(effect,tys)) = (effect,tys)

(*----------------------------------------------------------------------*)
(* Test for various kinds of type for the purposes of deciding where    *)
(* to put disambiguating parentheses.                                   *)
(*----------------------------------------------------------------------*)
fun isProd ty = 
  case outTy ty of
    Prod(_::_) => true
  | _ => false

fun isSum ty = isSome (fromSum ty)

fun isFun ty = isSome (fromArrow ty)

fun tyVarToString i = "'" ^ Var.toString i

(*----------------------------------------------------------------------*)
(* Pretty-print a type.							*)
(*----------------------------------------------------------------------*)
local 
  open Pretty 

  fun bt depth (s,kind) =
  case kind of
    Any => "'" ^ s
  | Eq => "''" ^ s
  | Bound ty => "'" ^ s ^ "<" ^ t depth ty

  and t depth ty =
  case outTy ty of
    Closure (SOME i, tys) =>
    vec("", "(", ") ", "(", ") ", ",") (t depth) tys ^ 
    "closure_" ^ Int.toString i  

  | Closure (NONE,_) =>
    "fun"

  | Prod tys =>
    vec("unit", "(", ")", "", "", "*") 
    (fn ty => parens (isSum ty orelse isProd ty orelse isFun ty) (t depth ty))
    tys

  | Con tys =>
    "con" ^ vec("", " <", ">", " <", ">", ",") (t depth) tys

  | Exn (exname, tys) =>
    "exn[" ^ MILExn.toString exname ^ "]" ^ 
      vec("", " <", ">", " <", ">", ",") (t depth) tys

  | Sum tyss =>
    if List.all null tyss
    then
    let 
      val n = length tyss
    in
      if n=2 then "bool" else "[1.." ^ Int.toString n ^ "]"
    end
    else
    vec("zero", "{", "}", "", "", "+") 
    (fn [ty] => parens (isSum ty orelse isFun ty) (t depth ty)
      | tys  => vt depth tys) tyss

  | Arrow([ty], cty) =>
    parens (isFun ty) (t depth ty) ^ "->" ^ ct depth cty

  | Arrow(tys, cty) =>
    vt depth tys ^ "->" ^ ct depth cty

  | Java (Types.CLASS c) =>
    ClassHandle.class_handle_toString c

  | Java b =>
    Types.base_type_toString b

  | Var i => 
    tyVarToString i

  | Deb i =>
    "'" ^ Int.toString (depth-i-1)

  | (ty' as Abs(kinds, ty) | ty' as Forall(kinds, ty)) =>
    let
      val prefix = case ty' of Forall _ => "All " | _ => "Fn "
    in
      vec("(", prefix, "(", prefix, "(", ",") 
      (fn (i,kind) => bt depth (Int.toString i,kind))
      (Gen.mapi Gen.id kinds) ^ 
      t (depth + length kinds) ty ^ ")"
    end

  | Tyname tyname =>
    TyName.toString tyname

  | Mu(i, defs) =>
    "mu_" ^ Int.toString i ^ "(" ^
    Pretty.simpleVec "," 
    (fn (i,ty) => "'" ^ Int.toString (depth+length defs - i - 1) ^ "=" ^ 
      t (depth + length defs) ty) (Gen.mapi Gen.id defs) ^ ")"

  | (tyc as Refty ty' | tyc as Vector ty' | tyc as Array ty') =>
    parens (isFun ty' orelse isProd ty' orelse isSum ty') (t depth ty') ^ 
    (case tyc of 
      Refty _ => " ref" | Array _ => " array" | _ => " vector")

  | App(ty, tys) =>
    t depth ty ^ " " ^ vt depth tys

  and vt depth tys =
    vec ("<>", "", "", "<", ">", ",") (t depth) tys

  and ct depth (CmpType(eff, tys)) =
  (if Controls.isOn "showEffects"
   then 
     if eff = Effect.any then "#"
     else if eff = Effect.none then ""
     else "[" ^ Effect.toString eff ^ "]"
   else ""
  ) ^ vt depth tys

in
  val toString = t 0
  val cmpToString = ct 0
  fun boundTyVarToString (tyvar,kind) = 
    bt 0 (Var.toString tyvar, kind)
end


(*----------------------------------------------------------------------*)
(* Failure error messages dumped to the log.                            *)
(*----------------------------------------------------------------------*)
fun failTy ty message = 
  (Debug.print ("Fail: " ^ message ^ " in:\n" ^ toString ty);
  raise Fail message)

fun adjust (ty,0) = ty
  | adjust (ty,offset) =
let
  fun a depth ty =
  if ISet.isEmpty (dtyvars ty)
  then ty 
  else
  case outTy ty of
    (Java _ | Tyname _ | Var _) => ty
  | Deb i => 
    if i >= depth 
    then deb (i+offset)
    else ty
  | Closure(iopt, tys) => closure(iopt, map (a depth) tys)
  | Prod tys => prod (map (a depth) tys)
  | Sum tyss => sum (map (map (a depth)) tyss)
  | Con tys => con (map (a depth) tys)
  | Exn(exname, tys) => exn (exname, map (a depth) tys)
  | Refty ty => refty (a depth ty)
  | Vector ty => vector (a depth ty)
  | Array ty => array (a depth ty)
  | Arrow(tys, CmpType(eff,tys')) => 
    arrow(map (a depth) tys, CmpType(eff, map (a depth) tys'))
  | Forall(kinds, ty) => debforall(kinds, a (depth+length kinds) ty)
  | Mu(i, defs) => mu(i, map (a (depth+length defs)) defs)
  | Abs(kinds, ty) => abs(kinds, a (depth+length kinds) ty)
  | App(ty, tys) => app(a depth ty, map (a depth) tys)
in
  a 0 ty
end


(*----------------------------------------------------------------------*)
(* Apply a type abstraction (represented by its open body) to a list of *)
(* types that do not contain any free de Brujn variables.               *)
(*----------------------------------------------------------------------*)
fun applyTy (ty,arity,S) =
let
  fun a depth (ty as ref (_,_,dtyvars,tyc)) =
  if ISet.isEmpty dtyvars then ty
  else
    case tyc of
    (Java _ | Tyname _ | Var _) => ty
  | Deb i => 
    if i >= depth andalso i < depth+arity
    then adjust (S(i-depth), depth)
    else if i >= depth then deb (i-arity) else ty
  | Closure(iopt, tys) => closure(iopt, map (a depth) tys)
  | Prod tys => prod (map (a depth) tys)
  | Sum tyss => sum (map (map (a depth)) tyss)
  | Con tys => con (map (a depth) tys)
  | Exn(exname, tys) => exn (exname, map (a depth) tys)
  | Refty ty => refty (a depth ty)
  | Vector ty => vector (a depth ty)
  | Array ty => array (a depth ty)
  | Arrow(tys, CmpType(eff,tys')) => 
    arrow(map (a depth) tys, CmpType(eff, map (a depth) tys'))
  | Forall(kinds, ty) => debforall(kinds, a (depth+length kinds) ty)
  | Mu(i, defs) => mu(i, map (a (depth+length defs)) defs)
  | Abs(kinds, ty) => abs(kinds, a (depth+length kinds) ty)
  | App(ty, tys) => app(a depth ty, map (a depth) tys)
in
  a 0 ty
end

fun app (ty, []) = ty
  | app (ty, tys) =
    case outTy ty of
      Abs(kinds, ty') => 
      let 
        val arity = length kinds
      in
        applyTy (ty', arity, fn i => List.nth(tys, i))
      end

    | _ => 
      injTy (App(ty, tys))

(*----------------------------------------------------------------------*)
(* Apply a substitution for term-bound type variables.                  *)
(*----------------------------------------------------------------------*)
fun subst S ty =
let
  fun a depth (ty as ref (_,tyvars,_,tyc)) =
  if Var.Set.isEmpty tyvars then ty
  else
  case tyc of
    (Java _ | Tyname _ | Deb _) => ty
  | Var i => 
    (case Var.Map.find(S, i) of
      NONE => ty
    | SOME ty => adjust(ty,depth))
  | Closure(iopt, tys) => closure(iopt, map (a depth) tys)
  | Prod tys => prod (map (a depth) tys)
  | Sum tyss => sum (map (map (a depth)) tyss)
  | Con tys => con (map (a depth) tys)
  | Exn(exname, tys) => exn (exname, map (a depth) tys)
  | Refty ty => refty (a depth ty)
  | Vector ty => vector (a depth ty)
  | Array ty => array (a depth ty)
  | Arrow(tys, CmpType(eff,tys')) => 
    arrow(map (a depth) tys, CmpType(eff, map (a depth) tys'))
  | Forall(kinds, ty) => debforall(kinds, a (depth+length kinds) ty)
  | Mu(i, defs) => mu(i, map (a (depth + length defs)) defs)
  | Abs(kinds, ty) => abs(kinds, a (depth + length kinds) ty)
  | App(ty, tys) => app(a depth ty, map (a depth) tys)
in
  a 0 ty
end

(*----------------------------------------------------------------------*)
(* Given an initial map from types to types (with no free type-bound    *)
(* tyvars), construct a memoizing function that applies this            *)
(* recursively to a type.                                               *)
(*----------------------------------------------------------------------*)
fun replace (initialm : Type Map.map) =
let 
  val m = ref initialm

  fun a depth ty =
  case Map.find(!m, ty) of
    SOME ty' => ty'
  | NONE =>
    let
      val ty' = 
      case outTy ty of
        (Java _ | Var _ | Deb _ | Tyname _) => ty
      | Closure(iopt, tys) => closure(iopt, map (a depth) tys)
      | Prod tys => prod (map (a depth) tys)
      | Sum tyss => sum (map (map (a depth)) tyss)
      | Con tys => con (map (a depth) tys)
      | Exn(exname, tys) => exn (exname, map (a depth) tys)
      | Refty ty => refty (a depth ty)
      | Vector ty => vector (a depth ty)
      | Array ty => array (a depth ty)
      | Arrow(tys, CmpType(eff,tys')) => 
        arrow(map (a depth) tys, CmpType(eff, map (a depth) tys'))
      | Forall(kinds, ty) => debforall(kinds, a (depth+length kinds) ty)
      | Mu(i, defs) => mu(i, map (a (depth + length defs)) defs)
      | Abs(kinds, ty) => abs(kinds, a (depth + length kinds) ty)
      | App(ty, tys) => app(a depth ty, map (a depth) tys)
    in
      m := Map.insert(!m, ty, ty');
      ty'
    end
in
  a 0 
end

(*----------------------------------------------------------------------*)
(* Memoize a function whose domain is types. WHAT ABOUT DE BRUJN?       *)
(*----------------------------------------------------------------------*)
fun memoize (f : (Type -> 'a) -> (Type -> 'a)) =
let 
  val m = ref (Map.empty : 'a Map.map)

  fun mf ty =
  let
    fun a depth ty =
    case Map.find(!m, ty) of
      SOME ty' => ty'
    | NONE => 
      let
        val ty' = f mf ty
      in
        m := Map.insert(!m, ty, ty');
        ty'
      end
  in
    a 0 ty
  end
in
  mf
end

fun forall ([],ty) = ty
  | forall (tyvars,ty) =
    let
      val (_,S) = 
        foldl 
        (fn ((v,_), (i,S)) => (i+1,Var.Map.insert(S, v, deb i)))
        (0,Var.Map.empty) tyvars     
    in
      injTy (Forall(map #2 tyvars, subst S ty))
    end


fun tyvarsToString tyvars =
  Pretty.vec ("", "", " ", "(", ") ", ",") 
  (fn v => "'" ^ Var.toString v) tyvars

(*----------------------------------------------------------------------*)
(* Unfold a recursive type.                                             *)
(*----------------------------------------------------------------------*)
fun unfold (args as (i, defs)) =
  let
    val n = length defs
  in
    applyTy (List.nth(defs, i), n, fn i => mu (i, defs))
  end

fun unionCmpTypes (CmpType(eff1, tys1), CmpType(eff2, tys2)) =
  CmpType(Effect.union(eff1,eff2), tys2)

fun cmpTypePlus (CmpType(eff1, tys), eff2) =
  CmpType(Effect.union(eff1, eff2), tys)

(*----------------------------------------------------------------------*)
(* are two types equal wrt a given type environment?		        *)
(*----------------------------------------------------------------------*)
fun eq (ty1, ty2) = ty1=ty2

fun equiv (ty1, ty2) =
case (outTy ty1, outTy ty2) of
  ((Array ty1, Array ty2) | (Vector ty1, Vector ty2) | (Refty ty1, Refty ty2)) 
  => equiv (ty1,ty2) 
| ((Prod tys1, Prod tys2) | (Con tys1, Con tys2)) => Eq.list equiv (tys1,tys2)
| (Exn (n1,_), Exn (n2,_)) => MILExn.eq (n1,n2)
| (Sum tyss1, Sum tyss2) => Eq.list (Eq.list equiv) (tyss1, tyss2)
| (Arrow(tys1,CmpType(_,tys1')), Arrow(tys2,CmpType(_,tys2'))) => 
  Eq.list equiv (tys1,tys2) andalso Eq.list equiv (tys1', tys2')
| (Java j1, Java j2) => Types.BaseKey.compare(j1,j2) = EQUAL
| (Mu(i1,defs1),Mu(i2,defs2)) => i1=i2 andalso Eq.list equiv (defs1, defs2)
| (Var tyvar1, Var tyvar2) => tyvar1=tyvar2
| (Deb tyvar1, Deb tyvar2) => tyvar1=tyvar2
| (Forall(tyvars1,ty1), Forall(tyvars2,ty2)) => 
  length tyvars1 = length tyvars2 andalso equiv (ty1,ty2)
| (Closure(i1,tys1), Closure(i2,tys2)) => 
  i1=i2 andalso Eq.list equiv (tys1,tys2)
| (Tyname tn1, Tyname tn2) => TyName.eq (tn1,tn2)
| _ => false


(*----------------------------------------------------------------------*)
(* Return true if this type is represented by a primitive type:		*)
(*   non-class Java types                                               *)
(*   enumerations     <> + ... + <>                                     *)
(*----------------------------------------------------------------------*)
fun isPrimitive ty =
case outTy ty of
  Java (Types.CLASS _) => false
| Java _ => true
| Sum tyss => List.all List.null tyss
| _ => false

(*----------------------------------------------------------------------*)
(* Return true if this is a Java class type that cannot be instantiated *)
(*----------------------------------------------------------------------*)
fun isUninstantiableClass ty =
case outTy ty of
  Java (Types.CLASS c) =>
  not (ClassHandle.equal(c, ClassHandle.string) orelse 
  ClassHandle.equal(c, ClassHandle.Exception) orelse 
  ClassHandle.equal(c, ClassHandle.object) orelse   
  ClassHandle.equal(c, ClassHandle.throwable))
| Tyname tn =>
  #class (TyName.sort tn)
| _ => false  

fun instTyvar env ty =
  case outTy ty of
    Var x => 
    (case Var.Map.find(env, x) of
      NONE => 
      Debug.fail  
      ("MILTy.instTyvar: type variable missing: " ^ Var.toString x
        ^ " in {" ^ Pretty.simpleVec "," (fn (x,_) => Var.toString x)
          (Var.Map.listItemsi env) ^ "}")

    | SOME (Bound ty) => ty
    | SOME _ => ty
    )

  | _ => ty


(*----------------------------------------------------------------------*)
(* Given a type ty, determine whether inl <> : 1+ty is represented by	*)
(* Java's null pointer.                                                 *)
(*----------------------------------------------------------------------*)
fun noneIsNull env ty =
case outTy (instTyvar env ty) of
  (Java _ | Prod _ | Con _ | Exn _ | Arrow _
  | Refty _ | Array _ | Vector _ | Closure _) => 
  true

| Tyname tn =>
  #class (TyName.sort tn)

| Forall(_,ty) => 
  noneIsNull env ty

| Sum ([[], [ty]] | [[ty], []]) =>
  isPrimitive ty orelse isUninstantiableClass ty

| Sum tyss =>
  not (List.all List.null tyss)

| Mu a =>
  noneIsNull env (unfold a)

| _ =>
  false
       
(*----------------------------------------------------------------------*)
(* Given a type ty, determine whether inr <v> : 1+ty is a no-op.	*)
(* If not, then v must be wrapped.                                      *)
(*----------------------------------------------------------------------*)
fun someIsNop env ty =
case outTy (instTyvar env ty) of
  (Java (Types.CLASS _) | Prod _ | Con _ | Exn _ | Arrow _
  | Refty _ | Array _ | Vector _ | Closure _) => 
  true

| Tyname tn =>
  #class (TyName.sort tn)

| Java _ => 
  false

| Sum ([[], [ty]] | [[ty], []]) =>
  true

| Sum tyss =>
  not (List.all List.null tyss)

| Mu a =>
  someIsNop env (unfold a)

| _ =>
  false
  
(*----------------------------------------------------------------------*)
(* Return true if this type is represented by an integer:		*)
(*   Java integer primitive type                                        *)
(*   enumerations     <> + ... + <>                                     *)
(*----------------------------------------------------------------------*)
fun isInt ty =
case outTy ty of
  Java Types.INT => true
| Sum tyss => List.all List.null tyss
| _ => false

fun forceBounds env ty = subst 
  (Var.Map.mapPartial (fn Bound ty => SOME ty | _ => NONE) env) ty

(*----------------------------------------------------------------------*)
(* Given two types ty1 and ty2, determine if ty1 <= ty2 in the subtype  *)
(* ordering.                                                            *)
(*----------------------------------------------------------------------*)
fun leq env (ty1, ty2) =
(* First check if they're identical as this is cheap *)
ty1=ty2 orelse

let
  fun leqhs env ([], []) = true
    | leqhs env (ty1::tys1, ty2::tys2) =
      leqh env (ty1, ty2) andalso leqhs env (tys1, tys2)
    | leqhs env _ = false

  and leqhss env ([], []) = true
    | leqhss env (tys1::tyss1, tys2::tyss2) =
      leqhs env (tys1, tys2) andalso leqhss env (tyss1, tyss2)
    | leqhss env _ = false

  and leqh (hyp,env) (ty1, ty2) =
  let
    val ty1 = instTyvar env ty1
    val ty2 = instTyvar env ty2
  in
    case (outTy ty1, outTy ty2) of

  (*..................................................................*)
  (* <>+ ..n.. +<>   <=   int              			      *)
  (*..................................................................*)
    (Sum tyss, Java Types.INT) => 
    List.all List.null tyss

  (*..................................................................*)
  (* <> + T1   <=   <> + T2                  if T1 <= T2	      *)
  (*..................................................................*)
  | (Sum ([[],[ty1']] | [[ty1'],[]]), Sum ([[],[ty2']] | [[ty2'],[]])) =>
    leqh (hyp,env) (ty1', ty2')

  (*..................................................................*)
  (* T <= <> + T                             if T not primitive or    *)
  (*                                         non-concrete class       *)
  (*..................................................................*)
  | (_, Sum ([[], [ty2']] | [[ty2'], []])) =>
    if isPrimitive ty2' then false
    else if isUninstantiableClass ty2'
    then ty1=ty2'
    else leqh (hyp,env) (ty1, ty2')
    
  (*..................................................................*)
  (* <>+ ..m.. +<>   <=   <>+ ..n.. +<>      if m <= n		      *)
  (* otherwise pointwise                                              *)
  (*..................................................................*)
  | (Sum tyss1, Sum tyss2) =>
    let
      fun enumSub ([], tyss2) = List.all List.null tyss2
        | enumSub (tys1::tyss1, tys2::tyss2) =
          List.null tys1 andalso List.null tys2 andalso enumSub (tyss1, tyss2)
        | enumSub (_, []) = false
    in
      enumSub (tyss1, tyss2) orelse leqhss (hyp,env) (tyss1, tyss2)
    end

  (*..................................................................*)
  (* T1 + ... + Tn   <=   S                  if some Ti non empty     *)
  (*..................................................................*)
  | (Sum tyss, Con []) =>
    not (List.all List.null tyss)

  (*..................................................................*)
  (* Contra-co subtyping on functions.                                *)
  (*..................................................................*)
  | (Arrow(tys1,cty1), Arrow(tys2,cty2)) =>    
    let 
      val (eff1,restys1) = fromCmp cty1
      val (eff2,restys2) = fromCmp cty2
      (* Effects should sub but we don't treat these properly yet *)
    in 
      (* Effect.sub(eff1, eff2) andalso *) 

              leqhs (hyp,env) (restys1, restys2)
      andalso leqhs (hyp,env) (tys2, tys1)
    end

  (*..................................................................*)
  (* T1 -> T2         <=    F                                         *)
  (*..................................................................*)
  | (Arrow _, Closure (NONE,[])) =>
    true

  | (Closure (NONE, []), Closure(NONE, [])) =>
    true

  (*..................................................................*)
  (* Pointwise on products				              *)
  (*..................................................................*)
  | (Prod tys1, Prod tys2) =>
    leqhs (hyp,env) (tys1, tys2)

  (*..................................................................*)
  (* Pointwise on vectors				              *)
  (*..................................................................*)
  | (Vector ty1, Vector ty2) =>
    leqh (hyp,env) (ty1, ty2)

  (*..................................................................*)
  (* Reference and array types are *only* subtypes of themselves.     *)
  (* Note that the equivalence check is not simply syntactic.         *)
  (*..................................................................*)
  | ((Refty ty1, Refty ty2) | (Array ty1, Array ty2)) =>
    leqh (hyp,env) (ty1, ty2) andalso leqh (hyp,env) (ty2,ty1)

  (*..................................................................*)
  (* For two recursive types, first check to see if they're in the    *)
  (* list of hypotheses; if not, add to hypotheses and unfold.        *)
  (*..................................................................*)
  | (Mu a1, Mu a2) =>
    let
      fun lookup [] = 
          leqh ((ty1,ty2)::hyp, env) (unfold a1, unfold a2)

        | lookup ((ty1',ty2')::hyp) =
          (ty1=ty1' andalso ty2=ty2') orelse lookup hyp
    in
      lookup hyp
    end

  (*..................................................................*)
  (* For a recursive type against a non-recursive type,               *)
  (* just unfold the recursive type and repeat.                       *)
  (*..................................................................*)
  | (Mu a, _) =>
    leqh (hyp,env) (unfold a, ty2)

  | (_, Mu a) =>
    leqh (hyp,env) (ty1, unfold a)
    
  (*..................................................................*)
  (* Pointwise on constructors				              *)
  (*..................................................................*)
  | (Con (_::_), Con []) =>
    true

  | (Con [], Con (_::_)) =>
    false

  | (Con tys1, Con tys2) =>
    leqhs (hyp,env) (tys1, tys2)

  (*..................................................................*)
  (* No subtyping on exceptions					      *)
  (*..................................................................*)
  | (Exn (e1,_), Exn (e2,_)) =>
    MILExn.eq (e1,e2)

  (*..................................................................*)
  (* No subtyping on Java types (they're all distinct, even classes). *)
  (*..................................................................*)
  | (Java j1, Java j2) => 
    Types.BaseKey.compare(j1,j2) = EQUAL 

  (*..................................................................*)
  (* No subtyping on internal class types.			      *)
  (*..................................................................*)
  | (Tyname t1, Tyname t2) =>
    TyName.eq(t1,t2)

  (*..................................................................*)
  (* Subtyping not implemented for polymorphic types.		      *)
  (*..................................................................*)
  | (Forall _, Forall _) =>
    Debug.fail ("MILTy.leqh: polymorphic types")
  
  | _ => 
    false
end

in
  leqh ([],env) (ty1,ty2)
end

and leqs env ([], []) = true
  | leqs env (ty1::tys1, ty2::tys2) =
    leq env (ty1, ty2) andalso leqs env (tys1, tys2)
  | leqs env _ = false

(*----------------------------------------------------------------------*)
(* Given two closed types ty1 and ty2, return				*)
(*   SOME ty    if ty is the least upper bound / greatest lower bound   *)
(*              of ty1 and ty2 wrt the subtype ordering;                *)
(*   NONE       if no lub/glb exists.                                   *)
(*----------------------------------------------------------------------*)
val (lub,lubs) =
let
  val memo = ref PairMap.empty
  fun lg (args as (islub,hyp)) (ty1, ty2) =
  let

(*
    val _ = print("\nlub: " ^ toString ty1 ^ " and " ^ toString ty2 ^ "...")
*)

    val tyc1 = outTy ty1
    val tyc2 = outTy ty2
  in
    case (tyc1, tyc2) of
  (*..................................................................*)
  (* Look up type variables in hypotheses                             *)
  (*..................................................................*)
    (Deb i, Deb j) =>
    let
      fun lookup [] = NONE
        | lookup ((i',j',k')::hyp) =
          if i = i' andalso j = j' 
          then SOME (deb k')
          else lookup hyp
    in
      lookup hyp
    end

  (*..................................................................*)
  (* <>+ ..n.. +<>   \/   int     =     int			      *)
  (*                                                                  *)
  (* <>+ ..n.. +<>   /\   int     =     <>+ ..n.. +<>                 *)
  (*..................................................................*)
  | ( (lty as Sum tyss, uty as Java Types.INT) 
    | (uty as Java Types.INT, lty as Sum tyss)) =>
   if List.all List.null tyss 
    then SOME (if islub then injTy uty else injTy lty)
    else NONE

  (*..................................................................*)
  (* <> + T1  \/  <> + T2	  =     <> + (T1 \/ T2)		      *)
  (* <> + T1  /\  <> + T2	  =     <> + (T1 /\ T2)		      *)
  (*..................................................................*)
  | (Sum ([[], [ty1]]), Sum ([[], [ty2]] | [[ty2], []])) =>
    (case lg args (ty1, ty2) of
      NONE => NONE
    | SOME ty => SOME (sum [[], [ty]]))

  | (Sum ([[ty1], []]), Sum ([[], [ty2]] | [[ty2], []])) =>
    (case lg args (ty1, ty2) of
      NONE => NONE
    | SOME ty => SOME (sum [[ty], []]))

  (*..................................................................*)
  (* <> + T1  \/  T2              =     <> + (T1 \/ T2)		      *)
  (*                                    if T1 not prim/nonconc class  *)
  (* <> + T1  /\  T2              =     T1 /\ T2                      *)
  (*                                    if T1 not prim/nonconc class  *)
  (*..................................................................*)
  | ((Sum ([[], [ty1]] | [[ty1], []]), ty2) |
      (ty2, Sum ([[], [ty1]] | [[ty1], []]))) =>
     if isPrimitive ty1 then NONE
     else 
     if isUninstantiableClass ty1
     then 
       if eq (ty1,injTy ty2) 
       then (if islub then SOME (sum [[], [ty1]]) else NONE)
       else NONE
     else (case lg args (ty1, injTy ty2) of
       NONE => NONE
     | SOME ty => if islub then SOME (sum [[], [ty]]) else SOME ty)

  (*..................................................................*)
  (* <>+ ..m.. +<>   \/   <>+ ..n.. +<>   =    <>+ ..max(m,n).. +<>   *)
  (*                                                                  *)
  (* <>+ ..m.. +<>   /\   <>+ ..n.. +<>   =    <>+ ..min(m,n).. +<>   *)
  (*                                                                  *)
  (* T1+ ... +Tn     \/   T1'+ ... + Tm'  =    S                      *)
  (*..................................................................*)
  | (Sum tyss1, Sum tyss2) =>
    let
      val enum1 = List.all List.null tyss1
      val enum2 = List.all List.null tyss2
    in
      if enum1 andalso enum2
      then 
        let val n1 = length tyss1
            val n2 = length tyss2
        in
          if islub then 
            if n1 < n2 then SOME ty2 else SOME ty1
          else
            if n1 < n2 then SOME ty1 else SOME ty2
        end
      else 
      if enum1 orelse enum2 
      then NONE
      else 
      case lgss args (tyss1, tyss2) of
        NONE => if islub then SOME (con []) else NONE
      | SOME tyss => SOME (sum tyss)
    end

  | ((uty as Con [], lty as Sum tyss) | (lty as Sum tyss, uty as Con [])) =>
    if List.all List.null tyss then NONE
    else if islub then SOME (injTy uty) else SOME (injTy lty)

  (*..................................................................*)
  (* If possible, do contra-co lub/glb on arrow types.		      *)
  (* Otherwise,                                                       *)
  (*    T1 -> T2   \/   T3 -> T4    =    F                            *)
  (*..................................................................*)
  | (Arrow(tys1,cty1), Arrow(tys2,cty2)) =>
    let 
      val (eff1,restys1) = fromCmp cty1
      val (eff2,restys2) = fromCmp cty2
    in 
      case (lgs (islub,     hyp) (restys1, restys2), 
            lgs (not islub, hyp) (tys1, tys2)) of
        (SOME restys, SOME tys) =>
        SOME (arrow (tys, CmpType(if islub then Effect.union(eff1,eff2) 
                        else Effect.intersection(eff1,eff2), restys)))

      | _ =>
        if islub then SOME (closure (NONE, [])) else NONE
    end

  (*..................................................................*)
  (* F  \/  F   =   F       					      *)
  (* F  /\  F   =   F       					      *)
  (*..................................................................*)
| (Closure (NONE,[]), Closure (NONE,[])) => 
    SOME (closure (NONE,[]))

  (*..................................................................*)
  (* T1->T2  \/  F    =     F					      *)
  (*                                                                  *)
  (* T1->T2  /\  F    =     T1->T2                                    *)
  (*..................................................................*)
  | ((uty as Closure (NONE,[]), lty as Arrow _) | 
     (lty as Arrow _, uty as Closure (NONE,[]))) =>
    if islub then SOME (injTy uty)
    else SOME (injTy lty)

  (*..................................................................*)
  (* Pointwise lub/glb on products				      *)
  (*..................................................................*)
  | (Prod tys1, Prod tys2) =>
    (case lgs args (tys1, tys2) of
      NONE => NONE
    | SOME tys => SOME (prod tys))

  (*..................................................................*)
  (* Pointwise lub/glb on vectors 				      *)
  (*..................................................................*)
  | (Vector ty1, Vector ty2) =>
    (case lg args (ty1, ty2) of
      NONE => NONE
    | SOME ty => SOME (vector ty))

  (*..................................................................*)
  (* Reference and array types are *only* subtypes of themselves.     *)
  (*..................................................................*)
  | ((Refty ty1', Refty ty2') | (Array ty1', Array ty2')) =>
    if leq Var.Map.empty (ty1', ty2') andalso leq Var.Map.empty (ty2',ty1')
    then SOME ty1 else NONE

  (*..................................................................*)
  (* COMPLICATED!                                                     *)
  (*..................................................................*)
  | (Mu(i,D), Mu(j,E)) =>
    let
      val (m,n) = (length D, length E)
      val mn = m*n

      (* Map (0,0) |-> 0, (1,0) |-> 1, ..., (m-1,n-1) |-> mn-1 *)
      fun makepairs (i,j,k) = 
        if j=n then []
        else if i=m then makepairs(0,j+1,k)
        else (i, j, k) :: makepairs(i+1,j,k+1)

      (* Adjust the current hypotheses to allow for the corresponding
         depths in the left and right arg; the target is adjusted by mn
         so that it isn't confused with the new targets *)
      val hyp' = makepairs (0,0,0) @ 
        map (fn (i,j,k) => (i+m,j+n,k+mn)) hyp

(*
      val _ = List.app (fn (i,j,k) => print ("\n" ^ Int.toString i ^ "," ^
        Int.toString j ^ " |-> " ^ Int.toString k)) hyp'
*)

      (* Calculate the lubs/glbs pairwise; D advances faster than E *)
      val solutions = Gen.mapi Gen.id (List.concat (
        map (fn ty2 => map (fn ty1 => 
          lg (islub,hyp') (ty1, ty2)) D) E))

(*
      val _ = List.app (fn (i, tyopt) =>
        print ("\n" ^ Int.toString i ^ " |-> " ^ (case tyopt of
          NONE => "NONE" | SOME ty => "SOME " ^ toString ty))) solutions
*)

      (* For which pairs does the lub/glb exist? *)
      val valid = List.mapPartial 
        (fn (i,SOME _) => SOME i | _ => NONE) solutions

      (* Set up a list for calculating strongly connected components *)
      (* Ignore tyvars >= mn as these are from a higher-level calc *)
      val refs = List.mapPartial 
        (fn (i,NONE) => NONE
          | (i,SOME ty) => SOME (i, 
            List.filter (fn i => i < mn)
            (ISet.listItems (dtyvars ty)))) solutions

(*
      val _ = List.app (fn (i, js) =>
        print ("\n" ^ Int.toString i ^ " |-> " ^ 
          Pretty.simpleVec "," Int.toString js)) refs
*)

      fun check (i,js) = List.all 
        (fn j => List.exists (fn i => i=j) valid) js

    in
      if List.all check refs
      then
      let
        val sccs = Dep.scc (refs, op=)

(*
        val _ = print ("\nscc: " ^ Pretty.simpleVec "," 
          (fn Dep.NonRec i => Int.toString i | Dep.Rec js =>
            "{" ^ Pretty.simpleVec "," Int.toString (NList.toList js) ^ "}")
          sccs)
*)

        fun trans (S, []) = S
          | trans (S, Dep.NonRec i :: sccs) = 
            let
              val (_,SOME ty) = List.nth(solutions, i)
              val ty' = applyTy (ty, 2000, 
                fn i => if i < mn then valOf(IMap.find(S,i))
                        else deb (i-mn))
            in
              trans (IMap.insert(S, i, ty'), sccs)
            end

          | trans (S, Dep.Rec js :: sccs) =
            let
              fun apply j = 
              let
                val (_,SOME ty) = List.nth(solutions, j)
                val len = NList.length js
                val S' = Gen.foldri
                  (fn (i, j, S) => IMap.insert(S, j, deb i)) S
                  (NList.toList js)
(*
                val _ = print ("\n Dep.Rec: ty = " ^ toString ty)
*)
              in
                applyTy (ty, 2000, fn i => 
                  if i < mn then valOf(IMap.find(S',i))
                  else deb (i-mn+len))
              end
              val defs = map apply (NList.toList js)
              val S = Gen.foldri 
                (fn (i, j, S) => IMap.insert(S, j, mu(i,defs))) S 
                (NList.toList js)
            in
              trans (S, sccs)
            end

        val S = trans (IMap.empty, sccs)          
        val result = IMap.find(S, j*m + i)
(*
        val _ = print ("\n..." ^ (case result of NONE => "NONE" |
          SOME ty => toString ty))
*)
      in
        result
      end
      else (NONE)
    end

  (*..................................................................*)
  (* For the lub/glb of a recursive type against a non-recursive type,*)
  (* just unfold the recursive type and repeat.                       *)
  (*..................................................................*)
  | (Mu a, _) =>
    lg args (unfold a, ty2)

  | (_, Mu a) =>
    lg args (ty1, unfold a)
    
  (*..................................................................*)
  (* Pointwise lub/glb on constructors				      *)
  (*..................................................................*)
  | (Con tys1, Con tys2) =>
    (case lgs args (tys1, tys2) of
      NONE => NONE
    | SOME tys => SOME (con tys))

  (*..................................................................*)
  (* No subtyping on exceptions					      *)
  (*..................................................................*)
  | (Exn (e1,_), Exn (e2,_)) =>
    if MILExn.eq (e1,e2) then SOME ty1 else NONE

  (*..................................................................*)
  (* No subtyping on Java types (they're all distinct, even classes). *)
  (*..................................................................*)
  | (Java j1, Java j2) => 
    if Types.BaseKey.compare(j1,j2) = EQUAL then SOME ty1 else NONE

  (*..................................................................*)
  (* No subtyping on internal class types. 			      *)
  (*..................................................................*)
  | (Tyname t1, Tyname t2) =>
    if TyName.eq(t1,t2) then SOME ty1 else NONE

  (*..................................................................*)
  (* lub/glb not implemented for polymorphic types.		      *)
  (*..................................................................*)
  | (Forall _, Forall _) =>
    Debug.fail ("MILTy.lg: polymorphic types")
  
  | _ => NONE
end

and lgs _ ([], []) = SOME []

  | lgs args (ty1::tys1, ty2::tys2) =
    (case lg args (ty1,ty2) of
      NONE => NONE
    | SOME ty => 
      case lgs args (tys1, tys2) of
        NONE => NONE
      | SOME tys => SOME (ty::tys))

  | lgs _ _ = NONE

and lgss _ ([], []) = SOME []

  | lgss args (tys1::tyss1, tys2::tyss2) =
    (case lgs args (tys1,tys2) of
      NONE => NONE
    | SOME tys => 
      case lgss args (tyss1, tyss2) of
        NONE => NONE
      | SOME tyss => SOME (tys::tyss))

  | lgss _ _ = NONE
in
  (fn (ty1,ty2) =>
  case PairMap.find(!memo, (ty1,ty2)) of
    SOME tyopt => tyopt
  | NONE =>       
    let
      val result = lg (true, []) (ty1,ty2)
    in
      (*print ("\n lub:" ^ toString ty1 ^ " and " ^ toString ty2); *)
      memo := PairMap.insert(!memo, (ty1,ty2), result);
      result
    end,
   lgs (true, []))
end

fun fromProdCon ty =
  case outTy ty of
    Prod tys => SOME tys
  | Con tys => SOME tys
  | Exn (_, tys) => SOME tys
  | Closure (SOME i, tys) => SOME tys
  | _ => NONE

(*----------------------------------------------------------------------*)
(* Subclassing test (used for exceptions). This is *not* the same as    *)
(* subtyping. Returns NONE for "don't know".                            *)
(*----------------------------------------------------------------------*)
fun subClass (ty1,ty2) =
case (outTy ty1, outTy ty2) of
  (Exn (exn1,_), Exn (exn2,_)) =>
  SOME (MILExn.eq (exn1,exn2))

| (Exn _, Java (Types.CLASS c)) =>
  SOME (ClassHandle.equal(c, ClassHandle.throwable) orelse 
        ClassHandle.equal(c, ClassHandle.Exception))

| (Java (Types.CLASS c1), Java (Types.CLASS c2)) =>
  if ClassHandle.equal(c1,c2) then SOME true else NONE

| _ =>
  NONE

(*----------------------------------------------------------------------*)
(* Deconstruct a MIL type by case.					*)
(*----------------------------------------------------------------------*)
fun deconstruct ty
  (funs as { tyvar, java, refty, array, vector, prod, con, exn,  
             sum, arrow, mu, forall, closure, deb, tyname }) =
case outTy ty of
  Var v       => tyvar v
| Java j      => java j
| Refty p     => refty p
| Vector p    => vector p
| Array p     => array p
| Prod p      => prod p
| Con p       => con p
| Exn n       => exn n
| Sum s       => sum s
| Arrow a     => arrow a
| Forall a    => forall a
| Mu a        => mu a
| Closure i   => closure i
| Deb i       => deb i
| Tyname i    => tyname i
| _           => Debug.fail ("MILTy.deconstruct: " ^ toString ty)

end
