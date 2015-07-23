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
(* Translate away polymorphic equality.                                 *)
(* (1) Translate uses of a equality-test conditional into either        *)
(*     (a) inline MIL code to test for equality; or                     *)
(*     (b) a boolean-valued function definition + application.          *)
(* (2) Convert type abstractions over one or more equality type         *)
(*     variable into functions taking an equality function argument for *)
(*     each equality type variable (cf dictionary-passing in Haskell).  *)
(* (3) Convert type applications with one or more equality type         *)
(*     variable into function applications, passing in equality         *)
(*     functions as arguments.                                          *)
(* The choice of when to inline an equality-test is crucial:            *)
(*   * always inline primitive equality tests;                          *)
(*   * always inline equality tests corresponding to tyvars (functions  *)
(*     that have been passed in as arguments)                           *)
(*   * always inline any test occurring inside a recursive              *)
(*     equality-test function if the type includes type variables (and  *)
(*     so needs the functions passed in as parameters) or references to *)
(*     the recursive type or other members of its scc (if it were not   *)
(*     inlined then the equality test fun would have to be put in the   *)
(*     same scc as the recursive type's equality test fun; this is      *)
(*     tricky).                                                         *)
(* Equality functions for recursive types are generated in scc's        *)
(* matching the scc's of the recursive definition. This should be the   *)
(* only source of cycles in the dependency graph for equality functions,*)
(* so we can get away with a topological sort for the final ordering.   *)
(*                                                                      *)
(* Some MIL types admit `primitive' Java equality tests. These are:     *)
(*   * primitive Java types and MIL base types;                         *)
(*   * ref types and array types                                        *)
(*   * 1+ref, 1+array to any nesting of 1+                              *)
(*======================================================================*)
structure Equality :> TRANSFORMER =
struct

local 
  open MILTerm
in

val insts = ref (Var.Map.empty : (MILTy.Type * Var.Set.set) Var.Map.map)
val code = ref (Var.Map.empty : (Var.Var list * MILTerm.TAbstr) Var.Map.map)

fun isEq (_,MILTy.Eq) = true
  | isEq _ = false

(*----------------------------------------------------------------------*)
(* Main function: requires pervasive environments, computation term	*)
(* and fresh variable supply.                                           *)
(*----------------------------------------------------------------------*)
fun transform (tyenv) ce = 

let

(*----------------------------------------------------------------------*)
(* Fill in definition of a particular equality function			*)
(*----------------------------------------------------------------------*)
fun addEqCode (var,info) =
  if isSome (Var.Map.find(!code, var))
  then Debug.fail "Equality.addEqCode: code already exists"
  else (code := (Var.Map.insert(!code, var, info)))

(*----------------------------------------------------------------------*)
(* Add a new equality function (or recursive set of functions).		*)
(*----------------------------------------------------------------------*)
fun addEqVars (var,varinfo) =
  if isSome (Var.Map.find(!insts, var))
  then Debug.fail "Equality.addEqVars: variable already exists"
  else (insts := Var.Map.insert(!insts, var, (varinfo,Var.Set.empty)))
  
(*----------------------------------------------------------------------*)
(* Add a dependency.					                *)
(*----------------------------------------------------------------------*)
fun addDep (f,f') =
    case Var.Map.find(!insts, f) of
      NONE => Debug.fail "Equality.addDep: variable missing"
    | SOME (varinfo, vs) => 
      (insts := Var.Map.insert(!insts, f, (varinfo,Var.Set.add(vs, f'))))

(*----------------------------------------------------------------------*)
(* Can we inline the equality code?					*)
(*----------------------------------------------------------------------*)
fun inlineEq ty =
MILTy.deconstruct ty 
{
  (* Type variables are implemented by applying an equality function arg *)
  tyvar = fn tyvar => true,

  deb = fn _ => Debug.fail "Equality.inlineEq: type-bound type variable",

  (* It must be an internal Java class *)
  tyname = fn _ => true,

  (* We shouldn't have embedded quantifiers in types *)
  forall = fn _ => Debug.fail "Equality.inlineEq: polymorphic type",

  (* Functions do not admit equality *)
  arrow  = fn _ => Debug.fail "Equality.inlineEq: function type",
  closure = fn _ => Debug.fail "Equality.inlineEq: closure type",
  con = fn _ => Debug.fail "Equality.inlineEq: constructor type",
  exn = fn _ => Debug.fail "Equality.inlineEq: exception type",

  (* All the following can be implemented primitively *)
  java = fn _ => true,
  refty = fn _ => true,
  array = fn _ => true,

  vector = fn _ => false,

  (* All products are put in their own functions *)
  prod = fn tys => false,

  (* All sums likewise *)
  sum = fn tyss => false,

  (* Recursive types just involve trivial unfoldings *)
  mu = fn _ => true 
}

(*----------------------------------------------------------------------*)
(* Look up a (non-recursive) type's equality function variable.   	*)
(* If it doesn't already exist then generate a variable and create code *)
(* for it.                                                              *)
(* For type variables, just look up the function argument.              *)
(*----------------------------------------------------------------------*)
fun getEqVar' eqenv defopt ty =
let
(*
  val _ = print ("\ngetEqVar': " ^ MILTy.toString ty)
*)
  fun find [] = 
      let
        val x = Census.freshVar 0
        val x1 = Census.freshVar 0
        val x2 = Census.freshVar 0
      in
        addEqVars (x, ty);
        let
          val e = makeEqCodeInline (eqenv,SOME x,0) 
            (Var x1,Var x2,
            Triv [MILTermOps.trueVal], Triv [MILTermOps.falseVal]) ty
        in
          addEqCode (x, (Var.Set.listItems (MILTy.tyvars ty),
            ([(x1, ty), (x2, ty)], e)));
          x
        end
      end

    | find ((var, (ty',_))::rest) =
      if MILTy.equiv (ty, ty')
      then var    
      else find rest

  val f = find (Var.Map.listItemsi (!insts))
in
  case defopt of 
    NONE => f 
  | SOME f' => (addDep(f', f); f)
end

(*----------------------------------------------------------------------*)
(* Generate code to apply an ML equality test for the type ty.          *)
(*----------------------------------------------------------------------*)
and makeEqCodeInline (eqenv,defopt,n) (args as (v1,v2,e1,e2)) ty =
let
  fun default () = Cond(MLEq, v1, v2, e1, e2)
(*
  val _ = print "\nmakeEqCodeInline"
*)
in
  MILTy.deconstruct ty 
  {

  (*..................................................................*)
  (* For type variables look up the function argument variable	      *)
  (*..................................................................*)
  tyvar = fn tyvar => 
    case Var.Map.find(eqenv, tyvar) of
      NONE => 
      MILPretty.failVal v1 
      ("Equality.makeEqCodeInline: missing equality type variable " ^ 
       Var.toString tyvar)

    | SOME var =>
      let
        val boolvar = Census.freshVar 0
      in
        Let(App(Var var, [v1,v2]), ([(boolvar, MILTys.bool)],
        Case(Var boolvar, true, [(0, ([], e2)), (1, ([], e1))], NONE)))
      end,

  (*..................................................................*)
  (* Polymorphic types and type-bound tyvars shouldn't be present     *)
  (*..................................................................*)
  forall = fn _ => Debug.fail "Equality.makeEqCodeInline: polymorphic type",
  deb = fn _ => Debug.fail "Equality.makeEqCodeInline: type-bound tyvar",

  (*..................................................................*)
  (* Functions do not admit equality, and low-level types are illegal *)
  (*..................................................................*)
  arrow  = fn _ => Debug.fail "Equality.makeEqCodeInline: function type",
  closure = fn _ => Debug.fail "Equality.makeEqCodeInline: closure type",
  con = fn _ => Debug.fail "Equality.makeEqCodeInline: constructor type",
  exn = fn _ => Debug.fail "Equality.makeEqCodeInline: exception type",

  (*..................................................................*)
  (* Java types, reference/array types and internal class types	      *)
  (* can all be implemented primitively.                              *)
  (*..................................................................*)
  java = fn _ => default (), 
  refty = fn _ => default (),
  array = fn _ => default (),
  tyname = fn _ => default (),

  (*..................................................................*)
  (* Vector equality is pointwise.           			      *)
  (*..................................................................*)
  vector = fn _ => Debug.fail "vector equality not implemented yet",
  
  (*..................................................................*)
  (* Product equality is also pointwise				      *)
  (*..................................................................*)
  prod = fn tys => 
    let
      val failvar = Census.freshVar 0
      fun make _ [] = 
          e1

        | make i (ty::tys) =
          let
            val x1 = Census.freshVar 0
            val x2 = Census.freshVar 0
            val succeed = make (i+1) tys
            val cont = makeEqCode (eqenv,defopt,n)
              (Var x1, Var x2, succeed, App(Var failvar, [])) ty
          in
            LetVal(x1, Proj(i, v1), LetVal(x2, Proj(i, v2), cont))
          end
          
    in
      LetFun([], LocalFun, Fun(failvar, ([], e2)), make 0 tys)
    end,

  (*..................................................................*)
  (* For sums we decompose using case then do pointwise comparison.   *)
  (*..................................................................*)
  sum = fn tyss => 
    let
      val succeedvar = Census.freshVar 0
      val failvar = Census.freshVar 0 
      fun makeEqVec _ ([],[],[]) = 
          App(Var succeedvar, [])

        | makeEqVec i (var::vars, var'::vars', ty::tys) = 
          let
            val succeed = makeEqVec (i+1) (vars, vars', tys)
          in
            makeEqCode (eqenv,defopt,n)
            (Var var, Var var', succeed, App(Var failvar,[])) ty
          end

      fun make _ [] cases =
          Case(v1, true, rev cases, NONE)
 
        | make i (tys::tyss) cases =
          let
            val xs = map (fn _ => Census.freshVar 0) tys
            val ys = map (fn _ => Census.freshVar 0) tys
            val e = makeEqVec 0 (xs, ys, tys)
          in
            make (i+1) tyss ((i, (xs, 
              Case(v2, true, [(i, (ys, e))], 
                SOME (App(Var failvar, [])))))::cases)
          end

      val e = make 0 tyss []
    in
      LetFun([], LocalFun, Fun(succeedvar, ([], e1)),
      LetFun([], LocalFun, Fun(failvar, ([], e2)), e))
    end,

  (*..................................................................*)
  (* Recursive types.						      *)
  (*..................................................................*)
  mu = fn a => 
    if Controls.isOn "pointerEq"
    then
    let
      val succeedvar = Census.freshVar 0
    in
      LetFun([], LocalFun, Fun(succeedvar, ([], e1)), 
        Cond(JavaTest Tests.eq, v1, v2, App(Var succeedvar, []),
          makeEqCode (eqenv,defopt,n) 
          (Unfold v1, Unfold v2, App(Var succeedvar,[]), e2) (MILTy.unfold a)))
    end
    else
    makeEqCode (eqenv,defopt,n) (Unfold v1, Unfold v2, e1, e2) (MILTy.unfold a)
  }
end

(*----------------------------------------------------------------------*)
(* Generate code to apply the conditional (t=Tests.eq) for the type ty.	*)
(* This may be inline (if inlineEq ty = true); otherwise it is a        *)
(* function application.                                                *)
(*----------------------------------------------------------------------*)
and makeEqCode (eqenv, defopt, n) (args as (v1,v2,e1,e2)) ty =
  if inlineEq ty
  then makeEqCodeInline (eqenv, defopt, n) args ty
  else
  let
    val var = getEqVar (eqenv, defopt) ty
    val boolvar = Census.freshVar 0
  in
    Let(App(Var var, [v1,v2]), ([(boolvar, MILTys.bool)],
        Case(Var boolvar, true, [(0, ([], e2)), (1, ([], e1))], NONE)))
  end

and getEqVar (eqenv, defopt) ty =
  ((* print "\ngetEqVar"; *)
  case MILTy.fromTyvar ty of
    SOME tyvar =>
    (case Var.Map.find(eqenv, tyvar) of
      NONE => 
      Debug.fail 
      ("Equality.getEqVar: missing equality type variable " ^ 
       Var.toString tyvar)

    | SOME x => 
      x)

  | NONE => 
    getEqVar' eqenv defopt ty)

(*----------------------------------------------------------------------*)
(* Translate a value term, _not_ including type abstractions or type	*)
(* applications which should only occur in val(v) and let x=v in e      *)
(* constructs.                                                          *)
(*----------------------------------------------------------------------*)
fun monoVal (env as (eqenv,tyenv,kindenv)) v =
let 
  val mc = monoCmp env
  val mv = monoVal env
  val mvs = monoVals env
in
case v of
  Var x => 
  (v, Var.lookup(tyenv, x))

| SCon(ty, jcon) => 
  (v, ty)

| Inj(ty, i, vs) => 
  (Inj(ty, i, #1 (mvs vs)), ty)

| ExCon(excon, ves) => 
  (ExCon(excon, #1 (mvs ves)), MILTys.topExn)

| Tuple vs => 
  let
    val (vs, tys) = mvs vs
  in
    (Tuple vs, MILTy.prod tys)
  end

| Proj(i, v) => 
  let
    val (v, ty) = mv v
  in
    (Proj(i, v), List.nth(valOf (MILTy.fromProdCon ty), i))
  end

| TApp(v, tys) => 
  let
    val (v, polyty) = mv v
    val SOME a = MILTy.fromForall polyty
  in
    (TApp(v,tys), MILTy.app (MILTy.abs a, tys))
  end

| TAbs(tyvars, v) =>
  let
    val eqtyvars = List.filter isEq tyvars
    val pairs = map (fn (tyvar,_) => (tyvar,Census.freshVar 0)) eqtyvars
    val (v, ty) = 
      monoVal (Var.extend(eqenv,pairs),tyenv,Var.extend(kindenv,tyvars)) v
  in
    (TAbs(tyvars, v), MILTy.forall(tyvars,ty))
  end

| Coerce(v, ty) => 
  (Coerce(#1 (mv v), ty), ty)

| Fold(v, ty) => 
  (Fold(#1 (mv v), ty), ty)

| Unfold v => 
  let
    val (v, subty) = mv v

    val SOME a = MILTy.fromMu subty
  in
    (Unfold v, MILTy.unfold a)
  end

| _ =>
  MILPretty.failVal v "Equality.monoVal: illegal value term"

end

(*----------------------------------------------------------------------*)
(* Translate a list of value terms.					*)
(*----------------------------------------------------------------------*)
and monoVals env vs =
case vs of
  [] => 
  ([], [])

| v::vs => 
  let
    val (v,ty) = monoVal env v
    val (vs,tys) = monoVals env vs
  in
    (v::vs, ty::tys)
  end


and monoContext (env as (eqenv,tyenv,kindenv)) v =
case v of
  TAbs(tyvars, v) =>
  let
    val eqtyvars = List.filter isEq tyvars
    val pairs = map (fn (tyvar,_) => (tyvar,Census.freshVar 0)) eqtyvars
    val (v, ty) = 
      monoVal (Var.extend(eqenv,pairs),tyenv,Var.extend(kindenv,tyvars)) v
  in
    if null eqtyvars
    then 
      (TAbs(tyvars, v), MILTy.forall(tyvars, ty), fn e => e)
    else
    let
      val args = map (fn (tyvar,var) => (var, 
        MILTy.arrow([MILTy.tyvar tyvar, MILTy.tyvar tyvar], 
        MILTy.cmp (Effect.none, [MILTys.bool])))) pairs
      val x = Census.freshVar 0
    in
      (Var x, MILTy.forall(tyvars, ty),
        fn e => LetFun(tyvars, AnyFun, Fun (x, (args, Triv [v])), e))
    end
  end

| TApp(v, tys) =>
  let 
    val (v, polyty) = monoVal env v
  in

  case MILTy.fromForall polyty of
    NONE => 
    MILPretty.failVal v "Equality.monoContext: expected polymorphic type"

  | SOME (a as (tyvars, ty)) =>
    let
      val eqtys = ListPair.foldr 
        (fn (MILTy.Eq,ty,tys) => ty::tys | (_,_,tys) => tys) [] (tyvars, tys)
      val resultty = MILTy.app (MILTy.abs a, tys)
      val extraargs = map (fn ty => Var (getEqVar (eqenv,NONE) ty)) eqtys
    in
      if null extraargs 
      then (TApp(v, tys), resultty, fn e => e)
      else 
      let
        val var = Census.freshVar 0
      in
        (Var var, resultty, 
          fn e => Let(App(TApp(v,tys), extraargs), ([(var, resultty)], e)))
      end
    end
  end

| _ =>
  let
    val (v, ty) = monoVal (eqenv, tyenv, kindenv) v
  in
    (v, ty, fn e => e)
  end

and monoContextVals env [] = 
    ([], [], fn e => e)

  | monoContextVals env (v::vs) =
    let
      val (v, ty, C1) = monoContext env v
      val (vs, tys, C2) = monoContextVals env vs
    in
      (v::vs, ty::tys, C1 o C2)
    end


and monoCmp (env as (eqenv,tyenv,kindenv)) ce = 
let 
  val mc = monoCmp env
  val mv = monoVal env
  val mvs = monoVals env

  fun monoCases tysFor (v, bindargs, cases, optdefault) =
    let
      val defresult = Option.map mc optdefault
      fun monoCase (i, (vs, ce)) =
        let
          val tys = tysFor i 
          val tyenv = Var.extend(tyenv, ListPair.zip(vs, tys))
          val (ce, cty) = monoCmp (eqenv, tyenv, kindenv) ce
        in
          ((i, (vs, ce)), cty)
        end
      val (cases, ctys) = ListPair.unzip (map monoCase cases)
      val (optdefault, cty::ctys) = 
        case defresult of NONE => (NONE,ctys) 
                        | SOME (ce,cty) => (SOME ce, cty::ctys)
    in
      ((v, bindargs, cases, optdefault), cty)
    end
in
case ce of
  App(v, vs) => 
  let
    val (v, funty, C1) = monoContext env v
    val (vs, _, C2) = monoContextVals env vs
  in
    (C1(C2(App(v, vs))), 
    #2 (valOf (MILTy.fromArrow funty)))
  end

| Java(jop, ves, cty) =>
  let
    val (ves,_,C) = monoContextVals env ves
  in
    (C(Java(jop, ves, cty)), cty)
  end

| Let(ce1, (vs,ce2)) =>
  let
    val (ce1, cty1) = mc ce1
    val (ce2, cty2) = monoCmp (eqenv, Var.extend(tyenv, vs), kindenv) ce2
  in
    (Let(ce1, (vs, ce2)), MILTy.unionCmpTypes(cty1,cty2))
  end

| Encap ce =>
  let
    val (ce,cty) = mc ce
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap ce, MILTy.cmp(Effect.allocs, tys))
  end

| Triv ves => 
  let
    val (ves, tys, C) = monoContextVals env ves
  in
    (C (Triv ves), MILTy.cmp (Effect.none, tys))
  end

| Case (ve, bindargs, cases, default) => 
  let
    val (ve, ty, C) = monoContext env ve
    val tyss = valOf(MILTy.fromSum ty)
    fun tysFor i =
    let val tys = List.nth(tyss, i)
    in
      if bindargs then tys else [MILTy.con tys]
    end
    val (result, cty) = monoCases tysFor (ve, bindargs, cases, default)
  in
    (C(Case result), cty)
  end

| CaseSCon (ve, bindargs, cases, default) => 
  let
    val (ve, _, C) = monoContext env ve
    val (result, cty) = monoCases (fn _ => []) (ve, bindargs, cases, default)
  in
    (C(CaseSCon result), cty)
  end

| CaseExCon (ve, bindargs, cases, default) => 
  let
    val (ve, _, C) = monoContext env ve
    fun tysFor ty = if bindargs then MILTys.exnTys ty else [ty]
    val (result, cty) = monoCases tysFor (ve, bindargs, cases, default)
  in
    (C(CaseExCon result), cty)
  end

| Cond (t, v1, v2, e1, e2) => 
  let
    val (v1, ty1, C1) = monoContext env v1
    val (v2, ty2, C2) = monoContext env v2
    val (e1, cty1) = mc e1
    val (e2, cty2) = mc e2
    val _ = 
      if Controls.isOn "showEq" 
      then Debug.print ("\n  " ^ MILTy.toString ty1 ^ ": " ^ 
        MILPretty.valToString v1 ^ " = " ^
        MILPretty.valToString v2)
      else ()
  in
  case t of
    MLEq =>
    let 
      val e = makeEqCode (eqenv,NONE,1) (v1, v2, e1, e2) ty1
    in
      (C1(C2 e), cty1)
    end

  | _ =>
    (C1(C2(Cond(t, v1, v2, e1, e2))), cty1)
  end

| Throw(v, tys, loc) =>
  let
    val (v, _, C) = monoContext env v
  in
    (C(Throw(v, tys, loc)), MILTy.cmp(Effect.throws, tys))
  end

| TryLet(ce0, tabss, (vs, ce2)) =>
  let
    val (ce0, cty0) = mc ce0
    val tabss = map (fn (vs,ce) => 
      (vs, #1 (monoCmp (eqenv, Var.extend(tyenv, vs), kindenv) ce))) tabss
    val (ce2, cty2) = monoCmp (eqenv, Var.extend(tyenv, vs), kindenv) ce2
  in
    (TryLet(ce0, tabss, (vs, ce2)), cty2)
  end

(*......................................................................*)
(* Recursive function definition.					*)
(*......................................................................*)
| LetFun(tyvars, kind, RecFun recbinds, ce) => 
  let
    fun makeFunTypes (funvar1,funvar2,(typedvars,ce):MILTerm.TAbstr,cty) =
      ((funvar1, MILTy.forall(tyvars, MILTy.arrow(map #2 typedvars, cty))),
      (funvar2, MILTy.arrow(map #2 typedvars, cty)))

    val numbinds = length recbinds
    val pairs = map makeFunTypes recbinds
    val (bodyfuns,defnfuns) = ListPair.unzip pairs
    val defntyenv = Var.extend(tyenv, defnfuns)
    val defnkindenv = Var.extend(kindenv, tyvars)
    val eqtyvars = List.filter isEq tyvars

    val bodytyenv = Var.extend(tyenv, bodyfuns)

    fun transDef pairs (g', (f, g, (typedvars, ce), cty)) =
      let
        val (args, eqenv) = foldr (fn ((tyvar,_),(args,eqenv)) => 
        let
          val var = Census.freshVar 0
          val arg = (var, MILTy.arrow([MILTy.tyvar tyvar, MILTy.tyvar tyvar], 
            MILTy.cmp(Effect.none, [MILTys.bool])))
          val eqenv = Var.Map.insert(eqenv, tyvar, var)
        in 
          (arg::args, eqenv)
        end) ([], eqenv) eqtyvars

        val (ce,_) = monoCmp (eqenv, 
          Var.extend(defntyenv, typedvars), defnkindenv) ce
      in
        if null eqtyvars 
        then (f, g, (typedvars, ce), cty)
        else 
        let
          val var = Census.freshVar 0
        in
          (f, g', (args,
            foldl (fn ((g',(_,g,(typedvars:MILTerm.TypedVar list,_),cty)),ce) 
              => 
              Let(App(Var g', map (Var o #1) args), ([(g, 
                MILTy.arrow(map #2 typedvars, cty))], ce)))
              (LetFun([], AnyFun, Fun (var, (typedvars, ce)), 
               Triv [Var var])) pairs),
            MILTy.cmp (Effect.none, [MILTy.arrow(map #2 typedvars,cty)]))
        end
      end
    val pairs = map 
      (fn x as (f,g,_,_) => 
        if null eqtyvars then (g,x)
        else (Census.freshVar 0,x)) recbinds
    val recbinds' = map (transDef pairs) pairs
    val (ce, cty) = monoCmp (eqenv, bodytyenv, kindenv) ce
  in
    (LetFun(tyvars, kind, RecFun recbinds', ce), cty)
  end

(*......................................................................*)
(* Non-recursive function definition.					*)
(*......................................................................*)
| LetFun(tyvars, kind, Fun (f, (typedvars, e1)), e2) => 
  let
    val defnkindenv = Var.extend(kindenv, tyvars)
    val eqtyvars = List.filter isEq tyvars
    val (args, eqenv) = foldr (fn ((tyvar,_),(args,eqenv)) => 
      let
        val var = Census.freshVar 0
        val arg = (var, MILTy.arrow([MILTy.tyvar tyvar, MILTy.tyvar tyvar], 
          MILTy.cmp (Effect.none, [MILTys.bool])))
        val eqenv = Var.Map.insert(eqenv, tyvar, var)
      in 
        (arg::args, eqenv)
      end) ([], eqenv) eqtyvars
    val (e1, cty) = monoCmp (eqenv, 
      Var.extend(tyenv, typedvars), defnkindenv) e1

    val def = 
      if null eqtyvars 
      then (typedvars, e1)
      else 
      let
        val var = Census.freshVar 0
      in
        (args,
          (LetFun([], AnyFun, Fun (var, (typedvars, e1)), Triv [Var var])))
      end
    val (e2, cty) = monoCmp (eqenv, 
      Var.Map.insert(tyenv, f, MILTy.forall(tyvars,
        MILTy.arrow(map #2 typedvars, cty))), kindenv) e2
  in
    (LetFun(tyvars, kind, Fun (f, def), e2), cty)
  end

(*......................................................................*)
(* The types of fields and methods should be monomorphic already.	*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, ce) =>
  let
    fun transMethod (name, mods, tys, tyopt, optabs) =
        (name, mods, tys, tyopt, 
          case optabs of 
            NONE => NONE
          | SOME (f,(vs, ce)) =>
            let
              val argtys = 
                if List.exists (fn Method.STATIC => true | _ => false) mods
                then tys
                else classname::tys
              val tyenv = Var.extend(tyenv, ListPair.zip(vs,argtys))
              val (ce, cty) = monoCmp (eqenv, tyenv, kindenv) ce
            in
              SOME (f,(vs, ce))
            end)
    val methods = map transMethod methods
    val (ce, cty) = mc ce
    val (eff,tys) = MILTy.fromCmp cty
  in
    (LetClass(classname, classinfo, fields, methods, ce), 
      MILTy.cmp(Effect.union(eff, Effect.io), tys))
  end
  
| Alloc(f,ve) => 
  let
    val (ve, ty) = mv ve
  in
    (Alloc(f,ve), MILTy.cmp(Effect.allocs, [MILTy.refty ty]))
  end

| Deref ve => 
  let
    val (ve, ty) = mv ve
  in
    (Deref ve, MILTy.cmp (Effect.reads, 
    [valOf(MILTy.fromRefty ty)]))
  end

| Assign(ve1,ve2) => 
  let
    val (ve1, _) = mv ve1
    val (ve2, _) = mv ve2
  in
    (Assign(ve1, ve2), MILTy.cmp (Effect.writes, []))
  end

| LetVal(v, ve, ce) =>
  let
    val (ve,ty,C) = monoContext env ve
    val (ce,cty) = monoCmp (eqenv, Var.Map.insert(tyenv, v, ty), kindenv) ce
  in
    (C (LetVal(v, ve, ce)), cty)
  end

end 

in
  Census.clearCensus ();
  code := Var.Map.empty; 
  insts := Var.Map.empty;
  let
    val (ce, ty) = monoCmp (Var.Map.empty,Var.Map.empty,Var.Map.empty) ce
    val insts' = 
    Dep.scc (
      map (fn (x,(y,z)) => ((x,y), Var.Set.listItems z))
        (Var.Map.listItemsi (!insts)), fn ((x,y),z) => Var.eq(x,z))


    fun lookupCode f =
    case Var.Map.find(!code, f) of
      NONE => Debug.fail ("Equality.lookupCode: cannot find " ^ Var.toString f)
    | SOME info => info

(*
    val _ = 
      if Controls.isOn "showSpecs" 
      then Debug.print ("\nSCCs: " ^ Pretty.simpleVec ","
        (fn (isrec,scc) => (if isrec then "[" else "") ^ 
          Pretty.simpleVec "," (fn (v,_) => Var.toString v) scc ^ 
          (if isrec then "]" else "")) insts')
      else ()
*)

    fun makeBind (scc, e) = 
      case scc of
        Dep.NonRec (f,ty) => 
        let
          val (tyvars, tabs) = lookupCode f
        in
          LetFun(map (fn v => (v, MILTy.Any)) tyvars, AnyFun, Fun (f, tabs), e)
        end

      | Dep.Rec pairs =>
        let
          val (tyvars, _) = lookupCode (#1 (NList.hd pairs))
        in
          LetFun(map (fn v => (v, MILTy.Any)) tyvars, AnyFun, RecFun (NList.toList (NList.map (fn (f,ty) => 
            (f,f,#2 (lookupCode f),MILTy.noeffect [MILTys.bool])) pairs)), e)
        end
  
    val ce = foldl makeBind ce insts'
  in
    Census.renameCmp ce
  end
end


end (* of local open MILTerm *)

end (* of struct *)
