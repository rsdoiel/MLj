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
(* First do a very simple flow analysis of the whole program, enough to *)
(* determine which known functions are `local', which are `first-order' *)
(* and which are `higher-order'.                                        *)
(* Replace letrec by letblock and letknown appropriately.               *)
(* Also move some continuations into letrecs.                           *)
(* Finally, do some function inlining on non-local functions.           *)
(*                                                                      *)
(* Transformations are as follows (C[.] is a context in which [.]       *)
(* is in a continuation position):                                      *)
(*......................................................................*)
(* 1.                                                                   *)
(*     letrec   { f_i as g_i(x_i) = e_i} in e                           *)
(* --> letblock { f_i as g_i(x_i) = e_i} in e                           *)
(* [if all occs of g_i, f_i are tail apps and share the same scope].    *)
(* This is a special case of the next:                                  *)
(*......................................................................*)
(* 2.                                                                   *)
(*     letrec   { f_i as g_i(x_i) = e_i} in C[e]                        *)
(* --> C[letblock { f_i as g_i(x_i) = e_i} in e]                        *)
(* [if all occs of g_i are tail apps and share the same scope;          *)
(*     all occs of f_i are tail apps in e and share the same scope.]    *)
(*                                                                      *)
(*......................................................................*)
(* 3.  For some f in {f_i},                                             *)
(*     letrec   { f_i as g_i(x_i) : T_i = e_i}                          *)
(*     in C[let y <= f(v) in (e : T)]                                   *)
(* --> C[letrec c(y) = e : T in                                         *)
(*       letrec { f_i as g_i(x_i) : T = let z <= e_i in c(z) } in f(v)] *)
(* [if all occs of g_i are tail apps and share the same scope;          *)
(*     f is the only occurrence of any f_i]                             *)
(* Rationale: after simplification & more localisation the letrecs turn *)
(* to letblocks.                                                        *)
(*                                                                      *)
(*......................................................................*)
(* 4.  For some f in {f_i},                                             *)
(*     letrec   { f_i as g_i(x_i) = e_i} in                             *)
(*       C[try f(v) { handle (y_j : ty_j) => H_j } success y => e       *)
(* --> C[letrec c(y) = e in                                             *)
(*       letrec { h_j(y_j : ty_j) = H_j } in                            *)
(*       letrec { f_i as g_i(x_i) =                                     *)
(*                try e_i { handle (y_j : ty_j) => h_j(y_j) }           *)
(*                          success y => c(y) }                         *)
(*       in f(v)]                                                       *)
(* [if all occs of g_i are tail apps and share the same scope;          *)
(*     f is the only occurrence of any f_i]                             *)
(* Rationale as above.                                                  *)
(*                                                                      *)
(*......................................................................*)
(* 5.                                                                   *)
(*     letrec   { f_i as g_i(x_i) = e_i} in e                           *)
(* --> letknown { f_i as g_i(x_i) = e_i} in e                           *)
(* [if all occs of g_i and f_i are applications]                        *)
(*                                                                      *)
(*......................................................................*)
(* 6.                                                                   *)
(*     let x = v in C[e]                                                *)
(* --> C[let x = v in e]                                                *)
(* [if x not free in C[.]]                                              *)
(* This hoisting transformation only works on CC-normal-form terms.     *)
(*                                                                      *)
(*......................................................................*)
(* 7.                                                                   *)
(*     let x <= ref v in e                                              *)
(* --> let x <= varref v in e                                           *)
(* [if x only appears in := and ! positions, and defn is at toplevel    *)
(*======================================================================*)
structure GlobalOpt :> GLOBALOPT =
struct

local 
  open MILTerm Counters 
in

val maxInlineSize = ref 8
val maxUnrollSize = ref 12

(*----------------------------------------------------------------------*)
(* Transform the computation term e.					*)
(* If doInline=false, don't bother inlining or unrolling functions.     *)
(* If doHoist=false, don't bother hoisting value bindings.              *)
(*----------------------------------------------------------------------*)
fun transform {doInline,doHoistFloat} tyenv e =
let
  val { apps, kinds, higherrefs, hoistedvals, hoistedvars, ... } = 
    PrintManager.process ("Analysing flow", false)
    (fn () => Flow.flow doHoistFloat tyenv e)

(*----------------------------------------------------------------------*)
(* Empty environment        						*)
(*----------------------------------------------------------------------*)
val emptyEnv = 
{ 
  (* Functions to be inlined *)
  inlined = Var.Map.empty, 

  hoisted = Var.Map.empty, 

  (* Local function bindings that must be hoisted to a new scope *)
  hoistedfuns = MILPathOps.Map.empty,

  (* Current scope *)
  scope = [],

  (* Types of value variables *)
  tyenv = tyenv,           
  
  (* Kinds of type variables *)
  kindenv = Var.Map.empty
}

(*----------------------------------------------------------------------*)
(* Extend the scope							*)
(*----------------------------------------------------------------------*)
fun envPlusItem 
  { inlined, hoisted, hoistedfuns, scope, tyenv, kindenv } item =
  { inlined = inlined, hoisted = hoisted, 
    hoistedfuns = hoistedfuns, scope = item::scope, 
    tyenv = tyenv, kindenv = kindenv }

(*----------------------------------------------------------------------*)
(* Extend the type environment      					*)
(*----------------------------------------------------------------------*)
fun envPlusTypedVars 
  { inlined, hoisted, hoistedfuns, scope, tyenv, kindenv } xs =
  { inlined = inlined, hoisted = hoisted, hoistedfuns = hoistedfuns,
    scope = scope, tyenv = Var.extend(tyenv, xs), kindenv = kindenv }

(*----------------------------------------------------------------------*)
(* Extend the kind environment      					*)
(*----------------------------------------------------------------------*)
fun envPlusTyVars 
  { inlined, hoisted, hoistedfuns, scope, tyenv, kindenv} tyvars =
  { inlined = inlined, hoisted = hoisted, 
    hoistedfuns = hoistedfuns, 
    scope = scope, tyenv = tyenv, kindenv = Var.extend(kindenv, tyvars) }


(*----------------------------------------------------------------------*)
(* Add a function to the hoisted functions                     		*)
(*----------------------------------------------------------------------*)
fun envPlusHoistedFun
  { inlined, hoisted, hoistedfuns, scope, tyenv, kindenv} (scope', a) =
let
  val hoistedfuns = MILPathOps.Map.insert(hoistedfuns, scope', a)
in
  { inlined = inlined, hoisted = hoisted, scope = scope,
    hoistedfuns = hoistedfuns,
    tyenv = tyenv, kindenv = kindenv }
end

(*----------------------------------------------------------------------*)
(* Add a function to the inlinable functions 				*)
(*----------------------------------------------------------------------*)
fun envPlusInlined 
  { inlined, hoisted, hoistedfuns, scope, tyenv, kindenv} (x,a) =
  { inlined = Var.Map.insert(inlined, x, a), hoistedfuns = hoistedfuns,
    hoisted = hoisted, scope = scope,
    tyenv = tyenv, kindenv = kindenv }

fun envPlusHoisted 
  { inlined, hoisted, hoistedfuns, scope, tyenv, kindenv} (x,a) =
  { hoisted = Var.Map.insert(hoisted, x, a), hoistedfuns = hoistedfuns,
    inlined = inlined, scope = scope,
    tyenv = tyenv, kindenv = kindenv }


(*----------------------------------------------------------------------*)
(* Determine the type of a value term.                                  *)
(*----------------------------------------------------------------------*)
  fun transVal env v =
  case v of

(*......................................................................*)
(* Terms with explicit types                   				*)
(*......................................................................*)
  (SCon (ty, _) | Fold(_, ty) | Coerce(_, ty) | Inj(ty, _, _)) => 
  ty
  
(*......................................................................*)
(* Variables								*)
(*......................................................................*)
| Var v => 
  Var.lookup(#tyenv env, v)

(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold v =>
  let
    val subty = transVal env v

    (* The type of the subterm must be a mu type *)
    val a = case MILTy.fromMu subty of
      SOME a => a
    | NONE => Debug.fail "GlobalOpt.transVal: expected mu type"
  in
    (* Unfold this type one level *)
    MILTy.unfold a
  end

(*......................................................................*)
(* Exception introduction                                               *)
(*......................................................................*)
| ExCon _ =>
  MILTys.topExn
  
(*......................................................................*)
(* Product introduction                                           	*)
(*......................................................................*)
| Tuple vs => 
  MILTy.prod (map (transVal env) vs)

(*......................................................................*)
(* Product elimination                                    		*)
(*......................................................................*)
| Proj(i, v) => 
  let
    val prodty = transVal env v
    val tys = case MILTy.fromProdCon prodty of
      SOME a => a
    | NONE => Debug.fail "GlobalOpt.transVal: expected product type"
  in
    List.nth(tys, i)
  end

(*......................................................................*)
(* Quantifier elimination						*)
(*......................................................................*)
| TApp(v, tys) => 
  let
    val SOME a = MILTy.fromForall (transVal env v)
  in
    MILTy.app (MILTy.abs a, tys)
  end

(*......................................................................*)
(* Quantifier introduction						*)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val ty = transVal (envPlusTyVars env tyvars) v
  in
    MILTy.forall(tyvars, ty)
  end

| Null ty =>
  ty

| _ =>
  MILPretty.failVal v "GlobalOpt.transVal: illegal value term"

(*----------------------------------------------------------------------*)
(* Enter a new scope, hoist value bindings to this scope, transform	*)
(* term.                                                                *)
(*----------------------------------------------------------------------*)
and transNewScope' env e =
let
  val (e,cty) = transCmp env e

  (* Hoist value bindings *)
  val e = 
    if doHoistFloat
    then
      foldl (fn ((x,v),e) => LetVal(x,v,e)) e 
      (MILPathOps.Map.find(hoistedvals, #scope env))
    else e

  (* Hoist local function bindings *)
  val e =
    foldl (fn ((tyvars, kind, def), e) => LetFun(tyvars, kind, def, e)) e
    (MILPathOps.Map.find(#hoistedfuns env, #scope env))

in
  (e,cty)
end

and transNewScope item env e = transNewScope' (envPlusItem env item) e
     
(*----------------------------------------------------------------------*)
(* Transforms a computation term.					*)
(*----------------------------------------------------------------------*)
and transCmp env ce =
let
  fun transCases (tysFor,itemf) (v, bindargs, cases, optdefault) =
    let 
      val defresult = Option.map (transNewScope (itemf NONE) env) optdefault
      fun transCase ((i, (vs, ce)), (result, cty)) =
        let
          val tys = tysFor i 
          val (ce, cty') = transNewScope (itemf (SOME i)) 
            (envPlusTypedVars env (ListPair.zip(vs, tys))) ce
        in
          ((i, (vs, ce))::result, 
            case cty of NONE => SOME cty'
                      | SOME cty => SOME (MILTy.unionCmpTypes(cty, cty')))
        end
      val (cases,SOME cty) = foldr transCase ([],NONE) cases
    in
      case defresult of
        NONE => 
        ((v, bindargs, cases, optdefault), cty)

      | SOME (ce,cty') => 
        ((v, bindargs, cases, SOME ce), MILTy.unionCmpTypes(cty,cty'))      
    end

  fun transHandler ((xs, ce), (result, cty)) =
      let
        val (ce, cty') = transCmp (envPlusTypedVars env xs) ce
      in
        ((xs, ce)::result, MILTy.unionCmpTypes(cty,cty'))
      end
in
  case ce of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(Var v, velist) =>
  let
    val funty = 
    case Var.Map.find(#tyenv env, v) of
      NONE => Debug.fail
      ("GlobalOpt.transCmp: variable not in environment: " ^ Var.toString v)
    | SOME ty => ty

    val (_, cty) = case MILTy.fromArrow funty of
      SOME a => a
    | NONE => Debug.fail "GlobalOpt.transCmp: expected arrow type"
  in
    case Var.Map.find(#inlined env, v) of
      SOME ([], tabs) =>
      let
        val (typedvars, body) = Census.renameTAbstr tabs
      in
        Census.addVar (v, ~1); 
        (ListPair.foldr (fn ((var,ty),arg,e) => LetVal(var,arg,e))
          body (typedvars, velist), cty)
      end

    | _ => 
      (App(Var v, velist), cty)
  end

(*......................................................................*)
(* Arrow elimination: polymorphic variable       			*)
(*......................................................................*)
| App(TApp(Var v, tys), velist) =>
  let
    val polyty = 
    case Var.Map.find(#tyenv env, v) of
      NONE => Debug.fail
      ("GlobalOpt.transCmp: variable not in environment: " ^ Var.toString v)
    | SOME ty => ty

    val SOME a = MILTy.fromForall polyty

    val (_, cty) = case MILTy.fromArrow (MILTy.app (MILTy.abs a, tys)) of
      SOME a => a
    | NONE => Debug.fail "GlobalOpt.transCmp: expected arrow type"
  in
    case Var.Map.find(#inlined env, v) of
      SOME (tyvars, tabs) =>
      let
        val (typedvars, body) = Census.renameTAbstr tabs
        val S = ListPair.foldl (fn ((x,_),ty,S) => Var.Map.insert(S,x,ty))
          Var.Map.empty (tyvars,tys)
      in
        Census.addVar (v, ~1); 
        (ListPair.foldr (fn ((var,ty),arg,e) => LetVal(var,arg,e))
          (MILTermOps.substCmp S body) (typedvars, velist), cty)
      end

    | NONE => 
      (App(TApp(Var v, tys), velist), cty)
  end

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
| App(ve, velist) =>
  let
    val funty = transVal env ve
    val (_, cty) = case MILTy.fromArrow funty of
      SOME a => a
    | NONE => Debug.fail "GlobalOpt.transCmp: expected arrow type"
  in
    (ce, cty)
  end

(*......................................................................*)
(* Java operation							*)
(*......................................................................*)
| Java(j, velist, cty) =>
  (ce, cty)

(*......................................................................*)
(* Special case for allocation.  					*)
(*......................................................................*)
| Let(e1 as Alloc (AnyRef,v), ([(x,refty)], e2)) =>
  let
    val ty = transVal env v
    val SOME ty = MILTy.fromRefty refty
  in
    if not (Var.Set.member(higherrefs, x))
    andalso doHoistFloat andalso enabled globalRef
    then
    let
      val refty' = MILTy.refty ty
      val (e2,cty) = transCmp (envPlusTypedVars env [(x,refty')]) e2
    in
      (Let(Alloc (GlobalRef,v), ([(x,refty')], e2)), cty)
    end
    else 
    let
      val (e2,cty) = transCmp (envPlusTypedVars env [(x,refty)]) e2
    in
      (Let(e1, ([(x,refty)], e2)), cty)
    end
  end

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(ce1, (xs, ce2)) =>
  let
    val (ce1,cty1) = transCmp env ce1
    val (ce2,cty2) = transCmp (envPlusTypedVars env xs) ce2

    fun default () = (Let(ce1, (xs, ce2)), MILTy.unionCmpTypes (cty1, cty2))

    fun getHoisted (Var funvar) = Var.Map.find(#hoisted env, funvar)
      | getHoisted (TApp(Var funvar, tys)) = Var.Map.find(#hoisted env, funvar)
      | getHoisted _ = NONE
 
  in
    case ce1 of
      App(funv, args) =>
      (case getHoisted funv of
        NONE => 
        default ()

(*
      | SOME (tyvars, newkind, RecFun recbinds) =>
        let
          val contvar = Census.freshVar 0
          val recbinds = map (fn (f,g,(typedvars,e),cty) =>
            (f,g,(typedvars,addContinuation (contvar, cty2) e),cty2)) recbinds
        in
          (LetFun([], LocalFun, Fun(contvar, (xs, ce2)),
            LetFun(tyvars, newkind, RecFun recbinds, App(funv, args))),
            MILTy.unionCmpTypes(cty1, cty2))
        end

      | SOME (tyvars, newkind, Fun (f, (typedvars,e))) =>
        let
          val contvar = Census.freshVar 0
          val e = addContinuation (contvar,cty2) e
        in
          (LetFun([], LocalFun, Fun(contvar, (xs, ce2)),
          LetFun(tyvars, newkind, Fun (f, (typedvars, e)), App(funv, args))),
          MILTy.unionCmpTypes(cty1, cty2))
        end
*)
      )

    | _ => 
      default ()
  end
    
(*......................................................................*)
(* Value bindings                                                       *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val ty = transVal env v
    val env = envPlusTypedVars env [(x,ty)]
    val (e, cty) = transCmp env e
  in
    case Var.Map.find(hoistedvars, x) of
      NONE => (LetVal(x, v, e), cty)
    | SOME (_,[]) => (e, cty)
    | SOME (x',tys) => (LetVal(x, TApp(Var x', tys), e), cty)
  end

(*......................................................................*)
(* Initialisation							*)
(*......................................................................*)
| Init(x, i, v, e) =>
  let
    val ty = transVal env v
    val (e, cty) = transCmp env e
  in
    (Init(x, i, v, e), cty)
  end

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  let
    val (e, cty) = transCmp env e
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap e, MILTy.cmp(Effect.allocs, tys))
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  (ce, MILTy.noeffect (map (transVal env) vs))

(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case (v, bindargs, cases, default) =>
  let 
    val ty = transVal env v
    val tyss = 
      case MILTy.fromSum ty of
        SOME tyss => tyss
      | NONE => 
        MILPretty.failVal v ("GlobalOpt.transCmp: expected sum type but found "
        ^ MILTy.toString ty)

    fun tysFor i =
    let
      val tys = List.nth(tyss, i)
    in
      if bindargs then tys else [MILTy.con tys]
    end

    val (result, cty) = 
      transCases (tysFor, MILPath.CaseCon) (v, bindargs, cases, default)
  in
    (Case result, cty)
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (ve, bindargs, cases, default) =>
  let
    val (result, cty) = 
      transCases (fn c => [], MILPath.CaseSCon) (ve, bindargs, cases, default)
  in
    (CaseSCon result, cty)
  end
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| CaseExCon (ve, bindargs, cases, default) =>
  let
    fun tysFor ty =  if bindargs then MILTys.exnTys ty else [ty]
    val (result, cty) = 
      transCases (tysFor, MILPath.CaseExCon) (ve, bindargs, cases,default)
  in
    (CaseExCon result, cty)
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(ve, tys, loc) =>
  (Throw(ve, tys, loc), MILTy.cmp(Effect.throws, tys))

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(ce0, tabss, (vs, ce2)) =>
  let
    val (ce0, cty0) = transCmp env ce0
    val (ce2, cty2) = transCmp (envPlusTypedVars env vs) ce2
    val (tabss, cty1) = foldr transHandler ([],cty2) tabss
  in
    (TryLet(ce0, tabss, (vs, ce2)), MILTy.unionCmpTypes (cty0, cty1))
  end

(*......................................................................*)
(* Conditional                                                          *)
(*......................................................................*)
| Cond(t, ve1, ve2, ce1, ce2) =>
  let
    val (ce1,cty1) = transNewScope (MILPath.Cond true) env ce1
    val (ce2,cty2) = transNewScope (MILPath.Cond false) env ce2
  in
    (Cond(t, ve1, ve2, ce1, ce2), MILTy.unionCmpTypes(cty1,cty2))
  end
  
(*......................................................................*)
(* Allocation       							*)
(*......................................................................*)
| Alloc (f,v) =>
  let   
    val ty = transVal env v
  in
    (ce, MILTy.cmp(Effect.allocs, [MILTy.refty ty]))
  end

(*......................................................................*)
(* Dereferencing							*)
(*......................................................................*)
| Deref ve =>
  let
    val refty = transVal env ve
    val ty = case MILTy.fromRefty refty of
      SOME ty => ty
    | NONE => Debug.fail "GlobalOpt.transCmp: expected reference type"
  in
    (ce, MILTy.cmp(Effect.reads, [ty]))
  end

(*......................................................................*)
(* Assignment      							*)
(*......................................................................*)
| Assign _ =>
  (ce, MILTy.cmp (Effect.writes, []))
    
(*......................................................................*)
(* Internal Java class definition					*)
(*......................................................................*)
| LetClass(classname,classinfo, fields, methods, ce) =>
  let
    fun transMethod (i,(name, mods, tys, tyopt, optabs)) =
        (name, mods, tys, tyopt, 
          case optabs of 
            NONE => NONE
          | SOME (f,(vs, ce)) =>
            let
              val argtys = 
                if List.exists (fn Method.STATIC => true | _ => false) mods
                then tys
                else classname::tys
              val env = envPlusTypedVars env (ListPair.zip(vs,argtys))
            in
              SOME (f,(vs, #1 (transNewScope (MILPath.LetClass f) 
                env ce)))
            end)
    val methods = Gen.mapi transMethod methods
    val (ce,cty) = transCmp env ce
    val (eff, tys) = MILTy.fromCmp cty
  in
    (LetClass(classname,classinfo, fields, methods, ce), 
      MILTy.cmp(Effect.union(eff, Effect.io), tys))
  end

(*......................................................................*)
(* Recursive functions.                                                 *)
(*......................................................................*)
| LetFun(tyvars, kind, RecFun recbinds, e) =>
  let
    fun makeFunTypes (funvar1,funvar2,(typedvars,ce):MILTerm.TAbstr,cty) =
      ((funvar1, MILTy.forall(tyvars, MILTy.arrow(map #2 typedvars, cty))),
      (funvar2, MILTy.arrow(map #2 typedvars, cty)))

    val pairs = map makeFunTypes recbinds
    val (bodyfuns,defnfuns) = ListPair.unzip pairs
    val defnenv = envPlusTyVars (envPlusTypedVars env defnfuns) tyvars

    val bodyenv = envPlusTypedVars env bodyfuns

    val defnenv = foldl (fn ((_,g,(vs,e),_),env) =>
        if doInline 
        andalso not (MILTermOps.cmpBigger (e, !maxUnrollSize))
        andalso enabled unroll
        then envPlusInlined env (g, ([], (vs, e)))
        else env) defnenv recbinds

    fun transDef (funvar1, funvar2, (typedvars, e), cty) =
        let
          val (e, cty) = 
            transNewScope (MILPath.LetFun funvar1) 
            (envPlusTypedVars defnenv typedvars) e
        in
          (funvar1, funvar2, (typedvars, e), cty)
        end

    val representative = #1 (hd recbinds)
    val newkind = 
      case Var.Map.find(kinds, representative) of
        SOME (kind,_) => kind
      | _ => kind

    val newdef = RecFun (map transDef recbinds)
    fun default () =
    let
      val (e,cty) = transCmp bodyenv e
    in
      (LetFun(tyvars, newkind, newdef, e), cty)
    end

  in
(*
    if Var.Set.member(hoist, representative)
    then
      transCmp (foldr (fn ((f,_,_,_),env) => 
        envPlusHoisted env (f,(tyvars,newkind,newdef))) bodyenv
        recbinds) e
    else
*)
    if MILTermOps.isLocal newkind
    then
      case List.concat 
        (List.mapPartial (fn (f,_,_,_) => Var.Map.find(apps, f)) recbinds) of
        (scope'::scopes') => 
        let 
          val scope' = foldl MILPathOps.join (#2 scope') (map #2 scopes')
        in
          if MILPathOps.eq(#scope env,scope')
          then default ()
          else transCmp (envPlusHoistedFun bodyenv 
            (scope',(tyvars,newkind,newdef))) e
        end

      | _ =>
        default ()
    else default ()
  end

(*......................................................................*)
(* Non-recursive function defn: these can be inlined.       		*)
(*......................................................................*)
| LetFun(tyvars, kind, Fun (f, (typedvars,e1)), e2) =>
  let
    val defnenv = envPlusTyVars env tyvars

    val newkind = 
        case Var.Map.find(kinds, f) of
          SOME (kind,_) => kind
        | _ => kind
  
    val (e1, cty) = transNewScope (MILPath.LetFun f) 
      (envPlusTypedVars defnenv typedvars) e1

    val bodyenv = 
      if MILTermOps.isLocal newkind then env
      else 
      if doInline 
        andalso not (MILTermOps.cmpBigger (e1, !maxInlineSize))
        andalso enabled inline
        then 
        (
          if Controls.isOn "showInlining" 
          then Debug.print ("\nInline " ^ Var.toString f ^ " ("
            ^ Pretty.simpleVec "," (fn (x,ty) => Var.toString x ^ ":" 
            ^ MILTy.toString ty) typedvars ^ ")")
          else ();
          envPlusInlined env (f, (tyvars, (typedvars, e1)))
        )
        else env

    val bodyenv = envPlusTypedVars bodyenv [(f,
      MILTy.forall(tyvars, MILTy.arrow(map #2 typedvars, cty)))]

    val newdef = Fun (f, (typedvars, e1))

    fun default () = 
    let
      val (e2, cty) = transCmp bodyenv e2
    in
      (LetFun(tyvars, newkind, newdef, e2), cty)
    end

  in
(*
    if Var.Set.member(hoist, f)
    then
      transCmp (envPlusHoisted env (f, (tyvars,newkind,newdef))) e2
    else
*)
    if MILTermOps.isLocal newkind
    then
      case Var.Map.find(apps, f) of
        SOME (scope'::scopes') => 
        let 
          val scope' = foldl MILPathOps.join (#2 scope') (map #2 scopes')
        in
          if MILPathOps.eq(#scope env,scope')
          then default ()
          else transCmp (envPlusHoistedFun bodyenv 
            (scope',(tyvars,newkind,newdef))) e2        
        end

      | _ =>
        default ()
    else default ()
  end
end

(*----------------------------------------------------------------------*)
(* Do it!								*)
(*----------------------------------------------------------------------*)
val (e, cty) = 
  PrintManager.process ("Applying transformations", false)
  (fn () => transNewScope' emptyEnv e)

in
  (* Should have been reset by Flow.flow *)
  Counters.printCounts (); 

  e
end

end (* of local open *)
end (* of struct *)

