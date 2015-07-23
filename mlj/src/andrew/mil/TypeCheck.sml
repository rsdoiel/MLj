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
(* Type check a MIL term.                                               *)
(*======================================================================*)
structure TypeCheck :> TYPECHECK =
struct

local 
  open MILTerm 
in

val term = ref (NONE : Cmp option)

fun check closedBlocks {kindenv,tyenv,funenv,globenv} wholece =
let

fun fail ve message = 
  (term := SOME wholece; 
  MILPretty.failVal ve ("TypeCheck.checkVal: " ^ message))

fun failCmp ce message = 
  (term := SOME wholece; 
  MILPretty.failCmp ce ("TypeCheck.checkCmp: " ^ message))

type Env = 
  MILTy.Type Var.Map.map * MILTy.Type Var.Map.map * 
  MILTy.Type Var.Map.map * MILTy.Type Var.Map.map * 
  MILTy.Kind Var.Map.map

fun extendTyEnv (tyenv, funenv, blockenv, globenv, kindenv) typedvars =
  (Var.extend(tyenv, typedvars), funenv, blockenv, globenv, kindenv)

fun replaceTyEnv (tyenv, funenv, blockenv, globenv, kindenv) typedvars =
  (Var.extend(Var.Map.empty, typedvars), funenv, blockenv, globenv, kindenv)

fun extendGlobEnv (tyenv, funenv, blockenv, globenv, kindenv) (v,ty) =
  (tyenv, funenv, blockenv, Var.Map.insert(globenv, v, ty), kindenv)

fun extendFunEnv (tyenv, funenv, blockenv, globenv, kindenv) typedvars =
  (tyenv, Var.extend(funenv, typedvars), blockenv, globenv, kindenv)

fun extendBlockEnv (tyenv, funenv, blockenv, globenv, kindenv) typedvars =
  (tyenv, funenv, Var.extend(blockenv, typedvars), globenv, kindenv)

fun extendKindEnv (tyenv, funenv, blockenv, globenv, kindenv) tyvars =
  (tyenv, funenv, blockenv, globenv, Var.extend(kindenv, tyvars))

val emptyEnv = 
  (tyenv, funenv, Var.Map.empty, globenv, kindenv)

fun tyenvOf (tyenv, funenv, blockenv, globenv, kindenv) = tyenv
fun kindenvOf (tyenv, funenv, blockenv, globenv, kindenv) = kindenv
fun funenvOf (tyenv, funenv, blockenv, globenv, kindenv) = funenv
fun globenvOf (tyenv, funenv, blockenv, globenv, kindenv) = globenv
fun blockenvOf (tyenv, funenv, blockenv, globenv, kindenv) = blockenv

fun killBlockEnv (tyenv, funenv, blockenv, globenv, kindenv) =
  (tyenv, funenv, Var.Map.empty, globenv, kindenv)

fun killTyEnv (tyenv, funenv, blockenv, globenv, kindenv) =
  (Var.Map.empty, funenv, blockenv, globenv, kindenv)

(* Check that type variables in type are in environment *)
fun checkTy (env : Env) (ty : MILTy.Type) =
let
  fun checkTy' env ty' =
  MILTy.deconstruct ty'
  {
  tyvar = fn x => 
    if isSome (Var.Map.find(kindenvOf env, x))
    then () 
    else Debug.fail 
      ("TypeCheck.checkTy: type variable missing: " ^ Var.toString x ^
       " in " ^ MILTy.toString ty),

  deb = fn _ => (),
  tyname = fn _ => (),
  forall = fn (kinds, ty) => checkTy' env ty,
  java = fn _ => (),
  exn = fn _ => (),
  refty = fn ty => checkTy' env ty,
  array = fn ty => checkTy' env ty,
  vector = fn ty => checkTy' env ty,
  prod = fn tys => app (checkTy' env) tys,
  closure = fn _ => (),
  con = fn tys => app (checkTy' env) tys,
  arrow = fn (tys, cty) => 
    (app (checkTy env) tys; checkCmpTy' env cty),
  mu = fn (i, defs) => app (checkTy' env) defs,
  sum = fn tyss => app (app (checkTy' env)) tyss
  }

  and checkCmpTy' env cty =
  let
    val (effect, tys) = MILTy.fromCmp cty
  in
    app (checkTy' env) tys
  end

in
  checkTy' env ty
end

and checkTyVar env (_, MILTy.Bound ty) = checkTy env ty
  | checkTyVar env _ = ()

fun checkCmpTy env cty =
let
  val (effect, tys) = MILTy.fromCmp cty
in
  app (checkTy env) tys
end

fun checkTy' env ty = (checkTy env ty; ty)

fun checkKinds x env ([],[]) = ()
  | checkKinds x env (ty::tys, MILTy.Bound ty'::kinds) =
    if MILTy.leq (kindenvOf env) (ty,ty')
    then checkKinds x env (tys,kinds)
    else Debug.fail 
      ("TypeCheck.checkKinds: " ^ MILPretty.valToString x ^ " {" ^
         MILTy.toString ty ^ "}" ^  
       " not less than bound " ^ MILTy.toString ty')
  | checkKinds x env (ty::tys, _::kinds) = checkKinds x env (tys,kinds)
  | checkKinds x env _ =
    Debug.fail "TypeCheck.checkKinds: tyvar arity mismatch"


(*----------------------------------------------------------------------*)
(* Type check the value term ve, return a value type, under:            *)
(*   a type environment tyenv (mapping variables to types);             *)
(*   a kind environment kindenv (mapping type variables to kinds);      *)
(*   a global function environment funenv (mapping variables to types); *)
(*   a global value environment globenv (mapping variables to types).   *)
(*----------------------------------------------------------------------*)
fun checkValMaybeGlobal (env : Env) ve =
  case ve of
    Var v =>
    (case Var.Map.find(tyenvOf env, v) of
      SOME ty => checkTy' env ty
    | NONE =>    
      case Var.Map.find(globenvOf env, v) of
        SOME ty => checkTy' env ty
      | NONE => fail ve "(global) variable not in environment")
  | _ => checkVal env ve

and checkAtom (env : Env) ve =
  if not (SimplifyOps.isAtom (kindenvOf env) ve)
  then fail ve "value is not atomic"
  else checkVal env ve

and checkVal (env : Env) ve =
case ve of

  SCon (ty, jcon) => 
  checkTy' env ty

| Var v => 
  (case Var.Map.find(tyenvOf env, v) of
    SOME ty => ty
  | NONE => 
    case Var.Map.find(funenvOf env, v) of
      SOME ty => fail ve "escaping function variable"
    | NONE => fail ve "variable not in environment")

| Inj(ty, i, velist) => 
  let 
    val ty = checkTy' env ty
  in
  case MILTy.fromSum ty of
    SOME tyss =>
    if i < 0 orelse i >= length tyss 
    then fail ve "invalid constructor number"
    else 
    let 
      val tys = List.nth(tyss, i)
      fun checkArgs [] = ty
        | checkArgs ((ve, ty2)::rest) =
          let 
            val ty1 = checkVal env ve
          in
            if MILTy.equiv (ty1, ty2) then checkArgs rest
            else fail ve 
            ("type mismatch (" ^ 
             MILTy.toString ty1 ^ " against " ^
             MILTy.toString ty2 ^ 
             ") in argument to constructor")
          end
    in
      if length velist <> length tys
      then fail ve "wrong number of arguments to constructor"
      else checkArgs (ListPair.zip(velist, tys))
    end

  | NONE => 
    fail ve "expected sum type"
  end

| ExCon(exname, velist) => 
  (map (checkVal env) velist; MILTys.topExn)

| Tuple velist => 
  MILTy.prod (map (checkVal env) velist)

| Proj(i, ve') => 
  (case MILTy.fromProdCon (checkVal env ve') of
    SOME tys => 
    ((List.nth(tys, i)) handle Subscript =>
      fail ve "projection out of range")
  | NONE => fail ve "expected product/constructor/exn type")

| TApp(ve', tys) => 
  let
    val polyty = checkVal env ve'
  in
    app (checkTy env) tys;
    case MILTy.fromForall polyty of
      SOME (a as (kinds, ty)) => 
      (checkKinds ve' env (tys, kinds); MILTy.app (MILTy.abs a, tys))
  
    | _ => fail ve' "expected polymorphic type"
  end

| TAbs(tyvars, ve) =>
  let
    val ty = checkVal (extendKindEnv env tyvars) ve
  in
    app (checkTyVar env) tyvars;
    MILTy.forall (tyvars, ty)
  end

| Coerce(ve, ty2) =>
  let     
    val ty1 = checkVal env ve 
  in 
    checkTy env ty2;
    ty2 (* Should check that type structures match *)
  end

| Fold(ve', ty) => 
  let 
    val ty1 = checkVal env ve'
    val a =
      case MILTy.fromMu ty of
        SOME a => a
      | _ => fail ve "expected explicit recursive type in fold"
    val ty2 = MILTy.unfold a
  in
    checkTy env ty;
    if MILTy.equiv (ty1,ty2) 
    then ty
    else 
      fail ve ("type mismatch (" ^ 
             MILTy.toString ty1 ^ " against " ^
             MILTy.toString ty2 ^ ") in fold")
  end

| Unfold ve => 
  let 
    val ty = checkVal env ve
  in
    case MILTy.fromMu ty of	  
      SOME a => MILTy.unfold a
    | _ => fail ve ("expected " ^ MILTy.toString ty ^ 
           " to be a recursive type")
  end

| Closure(i, vs) =>
  let
    val vtys = map (checkVal env) vs
(*
    val (tyvars,fvtys,_) = 
      (List.nth(#2 B, i)) handle Subscript => 
      fail ve "closure number out of range"
    fun inst ty = MILTy.instantiate ty (ListPair.zip(map #1 tyvars, tys))
    val fvstys = map inst fvtys
*)
  in
    MILTy.closure (SOME i, vtys)
(*
    if Eq.list MILTy.equiv (vtys,tys) then MILTy.closure (SOME i, tys)
    else fail ve 
      "types of arguments do not match closure's free variable types"
*)
  end

| Null ty => 
  checkTy' env ty    


(*----------------------------------------------------------------------*)
(* Type check the computation term ce, return a computation type.       *)
(*----------------------------------------------------------------------*)
and checkCmp (env : Env) ce =
let 
  fun checkCases checkTy (ve, _, cases, optdefault) =
  let
    val ty = checkAtom env ve
    val typesFor = checkTy ty
    fun checkCases (SOME cty) [] = 
        (case optdefault of 
          NONE => 
          cty

        | SOME ce => 
          let val cty' = checkCmp env ce
              val (_,tys') = MILTy.fromCmp cty'
              val (_,tys) = MILTy.fromCmp cty
          in
            if Eq.list MILTy.equiv (tys,tys') then cty'
            else failCmp ce 
              "type of default case does not match other cases"
          end              
        )
      | checkCases NONE [] =
        (case optdefault of
          NONE => failCmp ce "no cases and no default"
        | SOME ce => checkCmp env ce)

      | checkCases ctyopt ((i, (vs, ce'))::cases) =
        let 
          val tys = typesFor i
        in
          if length tys <> length vs 
          then failCmp ce "wrong number of arguments in branch"
          else 
            let 
              val cty = 
                checkCmp (extendTyEnv env (ListPair.zip(vs,tys))) ce'
              val (_,tys) = MILTy.fromCmp cty
            in
              case ctyopt of
                NONE => checkCases (SOME cty) cases
              | SOME cty'  =>         
                if Eq.list MILTy.equiv (tys,#2 (MILTy.fromCmp cty'))
                then checkCases ctyopt cases
                else failCmp ce ("rhs types do not match in case: "
                ^ MILTy.cmpToString cty ^ " against " ^ 
                  MILTy.cmpToString cty')
            end
          end
  in
    checkCases NONE cases
  end                          

  fun checkLetFun (tyvars, kind, def, ce) =
  let    
    val extend = 
      case kind of 
        AnyFun => extendTyEnv 
      | KnownFun => extendFunEnv
      | LocalFun => extendBlockEnv

    (* The environment under which the functions are checked *)
    val defnenv = 
      case def of 
        Fun _ => env
      | RecFun recbinds => extend env (map (fn (f, g, (vs,ce), cty) => 
        (g, MILTy.arrow(map #2 vs, cty))) recbinds)

    val defnenv = extendKindEnv defnenv tyvars

    val defnenv = 
      if MILTermOps.isLocal kind 
      then (if closedBlocks then killTyEnv defnenv else defnenv) 
      else killBlockEnv defnenv

    fun checkRecBinds ([], bodyenv) = 
        checkCmp bodyenv ce

      | checkRecBinds ((f, g, (vs, ce), cty)::rest, bodyenv) = 
        let 
          val tys = map #2 vs          
          val cty' = checkCmp (extendTyEnv defnenv vs) ce
        in
          app (checkTy defnenv) tys; checkCmpTy defnenv cty;
          if MILTy.equiv (MILTy.arrow(tys, cty), MILTy.arrow(tys, cty'))
          then checkRecBinds 
            (rest, extend bodyenv [(f, 
            MILTy.forall (tyvars, MILTy.arrow(tys,cty)))])
          else failCmp ce
           ("recursive function definition does not have correct type (" ^
            MILTy.cmpToString cty ^ " against " ^
            MILTy.cmpToString cty' ^ ")")      
        end

  in
    app (checkTyVar env) tyvars;
    case def of 
      Fun(f, (vs, ce')) =>
      let 
        val cty = checkCmp (extendTyEnv defnenv vs) ce'
      in
        app (checkTy defnenv o #2) vs;
        checkCmp (extend env [(f, 
          MILTy.forall (tyvars, MILTy.arrow(map #2 vs, cty)))]) ce
      end

    | RecFun recbinds =>
      checkRecBinds (recbinds, env)
  end


in
case ce of

  App(ve, velist) =>
  let val ty = 
    case ve of
      Var v =>
      (case Var.Map.find(tyenvOf env, v) of
        SOME ty => checkTy' env ty
      | NONE =>    
        case Var.Map.find(funenvOf env, v) of
          SOME ty => checkTy' env ty
        | NONE => 
          case Var.Map.find(blockenvOf env, v) of     
            SOME ty => checkTy' env ty
          | NONE => fail ve "variable not in environment")

    | TApp(ve' as Var v, tys) =>
      let 
        val _ = app (checkTy env) tys
        val polyty = 
        case Var.Map.find(tyenvOf env, v) of
          SOME ty => checkTy' env ty
        | NONE =>    
          case Var.Map.find(funenvOf env, v) of
            SOME ty => checkTy' env ty
          | NONE => 
            case Var.Map.find(blockenvOf env, v) of     
              SOME ty => checkTy' env ty
            | NONE => fail ve "variable not in environment"
      in
        case MILTy.fromForall polyty of
          SOME (a as (kinds, ty)) => 
          (checkKinds ve' env (tys, kinds); MILTy.app (MILTy.abs a, tys))

        | _ => fail ve "expected polymorphic type"
      end

    | ve => checkAtom env ve
  in
    case MILTy.fromArrow ty of
    SOME (tys, cty) =>
    let
      fun checkArgs [] = cty
        | checkArgs ((ve, ty2)::rest) =
          let 
            val ty1 = checkAtom env ve
          in
            if MILTy.equiv (ty1, ty2) then checkArgs rest
            else failCmp ce 
            ("type mismatch (" ^ MILTy.toString ty1 ^ " against "
             ^ MILTy.toString ty2 ^ ") in argument to function")
          end
    in
      if length velist <> length tys
      then failCmp ce "wrong number of arguments to function"
      else checkArgs (ListPair.zip(velist, tys))
    end

  | NONE => 
    fail ve ("expected function type: got " ^ MILTy.toString ty)
  end

| Java(j, ves, cty) =>
  let val _ = map (checkAtom env) ves
  in
    checkCmpTy env cty;
    cty
  end

| Let(Alloc(GlobalRef, v), ([(x,ty)], e)) =>
  let
    val env' = extendGlobEnv env (x,ty)
  in
    checkAtom env v;
    checkCmp env' e
  end

| Let(ce1, (vs, ce2)) =>
  let 
    val (_,tys) = MILTy.fromCmp (checkCmp (killBlockEnv env) ce1)
  in
    app (checkTy env) (map #2 vs);
    if Eq.list MILTy.equiv (tys,map #2 vs) 
    then checkCmp (extendTyEnv env vs) ce2
    else
      failCmp (Let(ce1, (vs, ce2))) 
      ("type of term in let does not match given bound variable types:"
      ^ Pretty.simpleVec "," MILTy.toString tys ^ " against " ^
        Pretty.simpleVec "," MILTy.toString (map #2 vs))
  end

| Triv ves =>
  MILTy.cmp(Effect.none, map (checkVal env) ves)

| Case (cases as (_,bindargs,_,_)) =>
  checkCases 
  (fn ty =>
   case MILTy.fromSum ty of 
     NONE => failCmp ce "expected sum type in case"
   | SOME tyss =>    
     fn i => 
       if i < 0 orelse i >= length tyss
       then failCmp ce "invalid constructor number in case" 
       else 
       let
         val tys = List.nth(tyss, i)
       in
         if bindargs then tys else [MILTy.con tys]
       end) cases

| CaseSCon cases =>
  checkCases 
  (fn ty =>
   case MILTy.fromJava ty of
     NONE => 
     failCmp ce "expected Java type in case"
   | _ => (fn jcon => [])) cases

| CaseExCon (cases as (_,bindargs,_,_)) =>
  checkCases
  (fn ty => 
   if MILTys.isTopExn ty 
   then (fn exnty => if bindargs then MILTys.exnTys exnty else [exnty])
   else failCmp ce "expected exception type in case") cases

| Throw(ve, tys, _) =>
  (map (checkTy env) tys;
  if MILTys.isTopExn (checkAtom env ve)
  then MILTy.cmp(Effect.throws, tys)
  else fail ve "expected exception type in throw")

| TryLet(ce0, tabss, abs as (vs, ce)) =>
  let
    val cty0 = checkCmp (killBlockEnv env) ce0
    val (_,tys0) = MILTy.fromCmp cty0
    val cty2 = checkCmp (extendTyEnv env vs) ce
    val (_,tys2) = MILTy.fromCmp cty2

    fun checkHandler tabs =
    case tabs of
      ([(v,ty)], ce) =>
      let 
        val cty1 = checkCmp (extendTyEnv env ([(v,ty)])) ce
        val (_,tys1) = MILTy.fromCmp cty1
      in
        if Eq.list MILTy.equiv (tys1,tys2) 
        then ()
        else failCmp ce "type mismatch between handler and body"
      end

    | _ => failCmp ce "expected single argument to handler"
 
    val _ = map checkHandler tabss
  in
    cty2
  end

| Alloc (GlobalRef,ve) =>
  failCmp ce "non-first-class reference allocation in first-class position"

| Alloc (AnyRef, ve) =>
  MILTy.cmp (Effect.allocs, [MILTy.refty (checkAtom env ve)])

| Deref ve =>
  (case MILTy.fromRefty (checkValMaybeGlobal env ve) of
    SOME ty => MILTy.cmp (Effect.reads, [ty])
  | NONE => fail ve "expected reference type in !")

| Assign(ve1,ve2) =>
  (case MILTy.fromRefty (checkValMaybeGlobal env ve1) of
    SOME ty1 =>
    let val ty2 = checkAtom env ve2
    in
      if MILTy.equiv (ty1, ty2)
      then MILTy.cmp(Effect.writes, [])
      else failCmp ce
         ("type mismatch (" ^ MILTy.toString ty1 ^ " against "
             ^ MILTy.toString ty2 ^ ") in assignment")
    end
  | NONE => fail ve1 "expected reference type in :=")

| Cond(t, ve1, ve2, ce1, ce2) =>
  let 
    val ty1 = checkAtom env ve1
    val ty2 = checkAtom env ve2
    val cty1 = checkCmp env ce1
    val cty2 = checkCmp env ce2
    val (_, tys1) = MILTy.fromCmp cty1
    val (_, tys2) = MILTy.fromCmp cty2
  in
(*
    if MILTy.equiv (ty1, ty2)
    then
*)
      if Eq.list MILTy.equiv (tys1, tys2)
      then cty1
      else failCmp ce "type mismatch between then and else clauses"
(*
    else failCmp ce
         ("type mismatch (" ^ MILTy.toString ty1 ^ " against "
             ^ MILTy.toString ty2 ^ ") in conditional")
*)
  end 

| LetFun a =>
  checkLetFun a

| LetClass(classname, classinfo, fields, methods, e) =>
  let
    val defnenv = killBlockEnv env
    fun checkMethod (n, ms, tys, tyopt, SOME (f,(vars, e))) =
        let 
          val argtys = 
            if List.exists (fn Method.STATIC => true | _ => false) ms
            then tys
            else classname::tys
        in
          ignore 
          (checkCmp (extendTyEnv defnenv (ListPair.zip(vars,argtys))) e)
        end

      | checkMethod m = ()

  in
    app checkMethod methods;
    checkCmp env e  
  end

| LetVal(v, ve, ce) =>
  let 
    val ty = checkVal env ve
  in
    checkCmp (extendTyEnv env [(v,ty)]) ce
  end

| Init(x,i,v,e) =>
  let
    val ty = checkVal env v
  in
    checkCmp env e
  end

| Encap e =>
  let 
    val cty = checkCmp (killBlockEnv env) e
    val (eff, tys) = MILTy.fromCmp cty
  in
    MILTy.cmp(Effect.allocs, tys)
  end

end

in
  checkCmp emptyEnv wholece
end

end (* of local open *)
end (* of struct *)

