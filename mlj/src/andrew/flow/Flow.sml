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
(* Do a very simple flow analysis of the whole program, enough to       *)
(* determine which known functions are `local', which are `first-order' *)
(* and which are `higher-order'.                                        *)
(*                                                                      *)
(* Also:                                                                *)
(*    *hoist* non-constant values to their smallest scope of usage;     *)
(*    *float* constant values out to top-level;                         *)
(*    determine which refs are *global*.                                *)
(*======================================================================*)
structure Flow :> FLOW =
struct

local 
  open MILTerm FlowTypes FlowOps Counters
in

(*----------------------------------------------------------------------*)
(* Do the flow analysis. Don't bother with val binding hoisting unless 	*)
(* doHoist is set.                                                      *)
(*----------------------------------------------------------------------*)
fun flow doHoistFloat tyenv e =
let

  val doHoistBindings = Controls.isOn "hoistVal" andalso doHoistFloat
  val doFloatBindings = Controls.isOn "floatVal" andalso doHoistFloat

(*----------------------------------------------------------------------*)
(* Function application scopes.						*)
(*----------------------------------------------------------------------*)
  val apps = ref (Var.Map.empty : Scope list Var.Map.map)

(*----------------------------------------------------------------------*)
(* Map from function variables to their `kind', with the normalised     *)
(* scope of the definition.                                             *)
(*----------------------------------------------------------------------*)
  val funkinds = ref (Var.Map.empty : (FunKind*Scope) Var.Map.map)

(*----------------------------------------------------------------------*)
(* Higher-order ref variables.                                     	*)
(*----------------------------------------------------------------------*)
  val higherrefs = ref Var.Set.empty

(*----------------------------------------------------------------------*)
(* Allocation scopes for ref variables.					*)
(*----------------------------------------------------------------------*)
  val refscopes = ref (Var.Map.empty : Scope Var.Map.map)

(*----------------------------------------------------------------------*)
(* Smallest common scope of non-constant value bindings			*)
(*----------------------------------------------------------------------*)
  val hoistedvars = 
    ref (Var.Map.empty : 
    {
      defn  : MILPath.Path,               (* Scope of definition *)
      use   : MILPath.Path option,        (* Smallest scope of use *)
      value : MILTerm.Val                 (* The value itself *)
    } Var.Map.map)

(*----------------------------------------------------------------------*)
(* Constant value bindings that should be floated outwards.		*)
(*----------------------------------------------------------------------*)
  val floatedvars = 
    ref (MILPathOps.Map.empty : (Var.Var * MILTerm.Val) MILPathOps.Map.map)

  val floatedvartyvars =
    ref (Var.Map.empty : (Var.Var * MILTy.Type list) Var.Map.map)

(*----------------------------------------------------------------------*)
(* Set of function variables to be hoisted.				*)
(*----------------------------------------------------------------------*)
  val hoists = ref Var.Set.empty

(*----------------------------------------------------------------------*)
(* Add a variable to the nonlocal set.					*)
(* O(log n).                                                            *)
(*----------------------------------------------------------------------*)
fun addApp (v,scope) =
    let
      val scopes' = 
        case Var.Map.find(!apps, v) of
          NONE => [scope]  
        | SOME scopes => scope::scopes
    in
      apps := Var.Map.insert(!apps, v, scopes')
    end

(*----------------------------------------------------------------------*)
(* Add a variable to the higher-order sets, if it's a function or ref.  *)
(* Otherwise update the binding variable map if necessary.              *)
(*----------------------------------------------------------------------*)
fun addHigherOrder (scope : Scope as (pos,path)) (x,ty) =
let
  val ty = 
    case MILTy.fromForall ty of 
      NONE => ty 
    | SOME (_,ty) => ty
      
  val isFunction = isSome (MILTy.fromArrow ty)
  val isRef = isSome (MILTy.fromRefty ty)
in

(*......................................................................*)
(* Add to function kinds        					*)
(* O(log n)                                                             *)
(*......................................................................*)
  if isFunction 
  then funkinds := Var.Map.insert(!funkinds, x, (AnyFun, topTail))
  else

(*......................................................................*)
(* Add to ref kinds							*)
(* O(log n)                                                             *)
(*......................................................................*)
  if isRef
  then higherrefs := Var.Set.add(!higherrefs, x)
  else

(*......................................................................*)
(* Add to value bindings.						*)
(* O(m log n) where m is length of path                                 *)
(*......................................................................*)
  if doHoistBindings 
  then case Var.Map.find(!hoistedvars, x) of
    NONE => ()
  | SOME { defn, use, value } =>
    if MILPathOps.eq (defn, path)
    then hoistedvars := #1 (Var.Map.remove(!hoistedvars, x))
    else
      let
        val newuse = 
          case use of 
            NONE => path
          | SOME path' => MILPathOps.join (path,path')
      in
        hoistedvars := Var.Map.insert(!hoistedvars, x, 
          { defn=defn, value=value, use=SOME newuse })
      end

  else ()
end

(*----------------------------------------------------------------------*)
(* Add a function variable to the hoist set				*)
(* O(log n)                                                             *)
(*----------------------------------------------------------------------*)
fun addHoistVar v = hoists := Var.Set.add(!hoists, v)

(*----------------------------------------------------------------------*)
(* Add a set of function variables.				        *)
(* In order to determine the function's kind, the uses and definition   *)
(* must have been analysed already.                                     *)
(*----------------------------------------------------------------------*)
fun addFunVars (scope,tyvars) (fs, gs) =
  let
    fun isLocal v = List.exists (fn v' => Var.eq(v,v')) fs orelse 
                      (case Var.Map.find(!funkinds, v) of 
                        SOME (LocalFun,_) => true
                      | _ => false)

    val normaliseScope = FlowOps.normaliseScope (not o isLocal)

    val scope = normaliseScope scope

    val (funkind, newhoists) = 
    if List.exists (fn v => not (Var.isDummy v) andalso 
      (case Var.Map.find(!funkinds,v) of SOME (AnyFun,_) => true | _ => false)) 
      (fs @ gs)
    then (AnyFun, !hoists)
    else
    let
      fun getScope v = 
        if Var.isDummy v then []
        else
        case Var.Map.find(!apps, v) of
          NONE => []
        | SOME scopes => map normaliseScope scopes
  
      val fscopes = List.concat (map getScope fs)
      val gscopes = List.concat (map getScope gs)

      fun sameScope [] = true
        | sameScope [_] = true
        | sameScope (scope1::(rest as (scope2::_))) =
          eq (scope1, scope2) andalso sameScope rest

      (* All recursive applications must be in the same scope as the definition *)
      val gsame = sameScope (scope::gscopes)

      (* All other applications must share the same scope *)
      val fsame = sameScope fscopes

      fun isTail (InTail,_) = true
        | isTail _ = false

      val allfTail = List.all isTail fscopes
      val allgTail = List.all isTail gscopes
    in
      (* Completely local function: notice that defn and non-recursive use 
         might not share scope. At present blockify can cope with this but 
         in future we might want to hoist the defn into the scope of its use *)
      if fsame andalso gsame andalso allfTail andalso allgTail
      andalso enabled localFunctions 
      then (LocalFun, !hoists)
      else 

      (* Completely local use inside defn, but single application outside. 
         We therefore the continuation of the application into the defn.
         Don't bother if it's not recursive as it'll be inlined anyway;
         also don't bother if it's polymorphic. That can wait! *)
      if gsame andalso allgTail andalso null tyvars 
        andalso not (List.all Var.isDummy gs)
        andalso (case fscopes of [(InLet,_)] => true | _ => false)
        andalso enabled hoistLetContinuation
      then (LocalFun, Var.Set.addList(!hoists, fs))
      else 

      (* Otherwise it's compiled as a global method *)
      if enabled knownFunctions 
      then (if doHoistBindings andalso fsame andalso (List.all Var.isDummy gs) 
            andalso Controls.isOn "showSubroutines"
            then Debug.print ("[subroutine " ^ Var.toString (hd fs) ^ "]")
            else (); (KnownFun, !hoists))
      else (AnyFun, !hoists)
    end
  in
    funkinds := foldl (fn (v,kinds) => Var.Map.insert(kinds,v,(funkind,scope)))
      (!funkinds) (fs@gs);
    hoists := newhoists
  end



(*----------------------------------------------------------------------*)
(* Truncate a scope to give just the top-level branching structure.	*)
(*----------------------------------------------------------------------*)
fun truncate scope =
let
  fun tr ([], result) = result
    | tr (MILPath.LetClass _ :: _, result) = result
    | tr ((item as MILPath.LetFun x)::rest, result) =
      (case Var.Map.find(!funkinds, x) of
         SOME (LocalFun,_) => tr (rest, item::result)
       | _ => result)
    | tr (item::rest, result) = tr (rest, item::result)

in
  tr (rev scope, [])
end

(*----------------------------------------------------------------------*)
(* Add a hoisted variable definition scope       			*)
(*----------------------------------------------------------------------*)
fun addHoistedDefn (pos,path) (x,v) =
  if doHoistBindings
  then hoistedvars := 
    Var.Map.insert(!hoistedvars, x, { defn=path, use=NONE, value=v })
  else ()

(*----------------------------------------------------------------------*)
(* Add a floated variable definition scope       			*)
(*----------------------------------------------------------------------*)
fun addFloatedDefn (pos,path) (x,v,tyvars) =
  if doFloatBindings
  then 
    if null tyvars
    then
    (
      floatedvars := MILPathOps.Map.insert(!floatedvars, truncate path, (x,v));
      floatedvartyvars := Var.Map.insert(!floatedvartyvars, x, (x, []))
    )
    else
    let
      val x' = Census.freshVar 1
    in
      floatedvars := 
        MILPathOps.Map.insert(!floatedvars, truncate path,
        (x', MILTermOps.tabs (tyvars, v)));
      floatedvartyvars := 
        Var.Map.insert(!floatedvartyvars,x, (x',map (MILTy.tyvar o #1) tyvars))
    end
  else ()

(*----------------------------------------------------------------------*)
(* Extend a type environment						*)
(*----------------------------------------------------------------------*)
fun tyenvPlusTypedVars tyenv xs =
  List.foldl (fn ((x,ty),tyenv) => Var.Map.insert(tyenv, x, (ty,false)))
    tyenv xs

(*----------------------------------------------------------------------*)
(* Remove some variables from a set.					*)
(* Ugly exception handling could be removed by writing our own sets.    *)
(*----------------------------------------------------------------------*)
fun remove (fvs, vars) =
  foldl (fn (v, m) => (Var.Set.delete(m, v)) handle _ => m) fvs vars

(*----------------------------------------------------------------------*)
(* Given a value term, analyse flow and return the type of the original	*)
(* term with its free variables.                                        *)
(* Also return its free type variables -- required for value hoisting.  *)
(*----------------------------------------------------------------------*)
  fun flowVal (env as (scope,tyenv,kindenv)) v =
  case v of

(*......................................................................*)
(* Constant introduction                    				*)
(*......................................................................*)
  SCon (ty, jcon) => 
  (ty, Var.Set.empty, Var.Set.empty)
  
(*......................................................................*)
(* Variables								*)
(*......................................................................*)
| Var x => 
  (case Var.Map.find(tyenv, x) of
    NONE => Debug.fail   
    ("Flow.flowVal: variable not in environment: " ^ Var.toString x)

  | SOME (ty, isglob) =>
    (addHigherOrder scope (x,ty); 
    (ty, if isglob then Var.Set.empty else Var.Set.singleton x,
     MILTy.tyvars ty))
  )

(*......................................................................*)
(* Mu introduction							*)
(*......................................................................*)
| Fold(v, ty) =>
  let
    val (_, fvs, tvs) = flowVal env v
  in
    (ty, fvs, Var.Set.union(MILTy.tyvars ty, tvs))
  end

(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold v =>
  let
    val (subty,fvs,tvs) = flowVal env v

    (* The type of the subterm must be a mu type *)
    val a = case MILTy.fromMu subty of
      SOME a => a
    | NONE => Debug.fail "Flow.flowVal: expected mu type"

  in
    (MILTy.unfold a, fvs, tvs)
  end

(*......................................................................*)
(* Sum introduction                                			*)
(*......................................................................*)
| Inj(ty, i, vs) => 
  let
    val (tys, fvs, tvs) = flowVals env vs
  in
    (ty, fvs, Var.Set.union(MILTy.tyvars ty, tvs))
  end

(*......................................................................*)
(* Exception introduction                                               *)
(*......................................................................*)
| ExCon(exname, vs) => 
  let
    val (tys, fvs, tvs) = flowVals env vs
  in
    (MILTys.topExn, fvs, tvs)
  end
  
(*......................................................................*)
(* Product introduction                                           	*)
(*......................................................................*)
| Tuple vs => 
  let
    val (tys, fvs, tvs) = flowVals env vs
  in
    (MILTy.prod tys, fvs, tvs)
  end

(*......................................................................*)
(* Product elimination                                    		*)
(*......................................................................*)
| Proj(i, v) => 
  let 
    val (prodty, fvs, tvs) = flowVal env v
    val tys = case MILTy.fromProdCon prodty of
      SOME a => a
    | NONE => Debug.fail "Flow.flowVal: expected product type"
  in
    (List.nth(tys,i), fvs, tvs)
  end

(*......................................................................*)
(* Quantifier elimination						*)
(*......................................................................*)
| TApp(v, tys) => 
  let
    val (polyty,fvs,tvs) = flowVal env v
    val SOME a = MILTy.fromForall polyty
  in
    (MILTy.app (MILTy.abs a, tys), fvs, 
    foldl Var.Set.union tvs (map MILTy.tyvars tys))
  end

(*......................................................................*)
(* Quantifier introduction						*)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val (ty, fvs, tvs) = flowVal (scope, tyenv, Var.extend(kindenv, tyvars)) v
  in
    (MILTy.forall(tyvars, ty), fvs,
     Var.Set.difference (tvs, Var.Set.addList(Var.Set.empty, map #1 tyvars)))
  end

(*......................................................................*)
(* Coercions.                                                		*)
(*......................................................................*)
| Coerce(v, outputty) =>
  let
    val (ty, fvs, tvs) = flowVal env v
  in
    (outputty, fvs, Var.Set.union(tvs, MILTy.tyvars outputty))
  end

(*......................................................................*)
(* Null value								*)
(* Mustn't be treated as constant as it will get updated!               *)
(*......................................................................*)
| Null ty =>
  (ty, Var.Set.singleton Var.dummy, MILTy.tyvars ty)

| _ =>
  MILPretty.failVal v "Flow.flowVal: illegal value term"

(*----------------------------------------------------------------------*)
(* Analyse flow for a vector of terms, accumulating free variables.	*)
(*----------------------------------------------------------------------*)
and flowVals env [] = 
    ([], Var.Set.empty, Var.Set.empty)

  | flowVals env (v::vs) =
    let
      val (ty,fvs,tvs) = flowVal env v
      val (tys,fvs',tvs') = flowVals env vs
    in
      (ty::tys, Var.Set.union(fvs,fvs'), Var.Set.union(tvs,tvs'))
    end

(*----------------------------------------------------------------------*)
(* Given a computation term, analyse flow and return the type of the    *)
(* original term.                                                       *)
(*----------------------------------------------------------------------*)
and flowCmp (env as (scope, tyenv, kindenv)) e =
let
  fun transCases (tysFor,itemf) (cases, optdef) =
    let 
      val defresult = 
        Option.map (flowCmp (newScope(scope,itemf NONE),tyenv,kindenv)) optdef
      fun transCase ((i, (vs, e)), cty) =
        let
          val tys = tysFor i 
          val tyenv = ListPair.foldl (fn (v,ty,tyenv) =>
            Var.Map.insert(tyenv, v, (ty,false))) tyenv (vs,tys)
          val cty' = flowCmp (newScope (scope,itemf(SOME i)), tyenv, kindenv) e
        in
          case cty of NONE => SOME cty'
                    | SOME cty => SOME (MILTy.unionCmpTypes(cty, cty'))
        end
      val SOME cty = foldr transCase NONE cases
    in
      case defresult of
        NONE => 
        cty

      | SOME cty' => 
        MILTy.unionCmpTypes(cty,cty')
    end

in
  case e of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(Var x, vs) =>
  let
    val funty = 
    case Var.Map.find(tyenv, x) of
      NONE => Debug.fail
      ("Flow.flowCmp: variable not in environment: " ^ Var.toString x)
    | SOME (ty,isglob) => ty

    val (_, cty) = case MILTy.fromArrow funty of
      SOME a => a
    | NONE => Debug.fail "Flow.flowCmp: expected arrow type"
  in
    addApp(x, scope);
    flowVals env vs;
    cty
  end

(*......................................................................*)
(* Arrow elimination: polymorphic variable       			*)
(*......................................................................*)
| App(TApp(Var x, tys), vs) =>
  let
    val polyty = 
    case Var.Map.find(tyenv, x) of
      NONE => Debug.fail
      ("Flow.flowCmp: variable not in environment: " ^ Var.toString x)
    | SOME (ty,isglob) => ty

    val SOME a = MILTy.fromForall polyty

    val (_, cty) = case MILTy.fromArrow  (MILTy.app (MILTy.abs a, tys)) of
      SOME a => a
    | NONE => Debug.fail "Flow.flowCmp: expected arrow type"
  in
    addApp(x, scope);
    flowVals env vs;
    cty
  end

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
| App(v, vs) =>
  let
    val (funty, _, _) = flowVal env v
    val (_, cty) = case MILTy.fromArrow funty of
      SOME a => a
    | NONE => Debug.fail "Flow.flowCmp: expected arrow type"
  in
    flowVals env vs;
    cty
  end

(*......................................................................*)
(* Java operation							*)
(*......................................................................*)
| Java(j, vs, cty) =>
  (flowVals env vs; cty)

(*......................................................................*)
(* Allocation                                                           *)
(*......................................................................*)
| Let(Alloc(g,v), ([(x,ty)], e)) =>
  let
    val (ty, _, _) = flowVal env v
    val tyenv = Var.Map.insert(tyenv, x, (MILTy.refty ty, false))
  in
    refscopes := Var.Map.insert(!refscopes, x, scope);
    flowCmp (scope, tyenv, kindenv) e
  end

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, (xs, e2)) =>
  let
    val cty1 = flowCmp (inLet scope, tyenv, kindenv) e1
    val tyenv = tyenvPlusTypedVars tyenv xs

    val cty2 = flowCmp (scope, tyenv, kindenv) e2
  in
    MILTy.unionCmpTypes (cty1, cty2)
  end
    
(*......................................................................*)
(* Value bindings                                                       *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val (ty,fvs,tvs) = flowVal env v
    val isconst = Var.Set.isEmpty fvs
  in
    if isconst
    then addFloatedDefn scope 
      (x,v,map (fn x => (x, valOf(Var.Map.find(kindenv, x)))) 
        (Var.Set.listItems tvs))
    else addHoistedDefn scope (x,v);

    flowCmp (scope, Var.Map.insert(tyenv, x, (ty,isconst)), kindenv) e
  end

(*......................................................................*)
(* Initialisation							*)
(*......................................................................*)
| Init(x, i, v, e) =>
  (flowVal env v; flowCmp (scope, tyenv, kindenv) e)

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  let
    val (tys,_,_) = flowVals env vs
  in
    MILTy.noeffect tys
  end

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  let
    val cty = flowCmp env e
    val (eff,tys) = MILTy.fromCmp cty
  in
    MILTy.cmp(Effect.allocs, tys)
  end

(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case (v,bindargs,cases,default) =>
  let
    val (ty,_,_) = flowVal env v
    val tyss = 
      case MILTy.fromSum ty of
        SOME tyss => tyss
      | NONE => 
        MILPretty.failVal v 
        ("Flow.flowCmp: expected sum type but found " ^ MILTy.toString ty)
    fun tysFor i =
    let
      val tys = List.nth(tyss,i)
    in
      if bindargs then tys else [MILTy.con tys]
    end
  in
    transCases (tysFor, MILPath.CaseCon) (cases,default)
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (v, bindargs, cases, default) =>
  (flowVal env v;
  transCases (fn c => [], MILPath.CaseSCon) (cases, default))
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| CaseExCon (v, bindargs, cases, default) =>
  (flowVal env v;
  transCases (fn ty => if bindargs then MILTys.exnTys ty else [ty],
    fn SOME ty => MILPath.CaseExCon (SOME ty)
     | NONE => MILPath.CaseExCon NONE) (cases,default))

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(v, tys, loc) =>
  (flowVal env v; MILTy.cmp(Effect.throws, tys))

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(e0, handlers, (xs, e2)) =>
  let
    val cty0 = flowCmp (inTry scope, tyenv, kindenv) e0
    val tyenv' = tyenvPlusTypedVars tyenv xs
    val cty2 = flowCmp (scope, tyenv', kindenv) e2

    fun transHandler ((xs, e), cty) =
    let
      val tyenv = tyenvPlusTypedVars tyenv xs
    in
      MILTy.unionCmpTypes(cty,flowCmp (scope,tyenv,kindenv) e)
    end

    val cty1 = foldr transHandler cty2 handlers
  in
    MILTy.unionCmpTypes (cty0, cty1)
  end

(*......................................................................*)
(* Conditional                                                          *)
(*......................................................................*)
| Cond(t, v1, v2, e1, e2) =>
  (flowVal env v1; flowVal env v2;
  let
    val cty1 = flowCmp (newScope (scope, MILPath.Cond true), tyenv,kindenv) e1
    val cty2 = flowCmp (newScope (scope, MILPath.Cond false),tyenv,kindenv) e2
  in
    MILTy.unionCmpTypes(cty1,cty2)
  end)
  
(*......................................................................*)
(* Allocation       							*)
(*......................................................................*)
| Alloc (g,v) =>
  let
    val (ty,_,_) = flowVal env v
  in
    MILTy.cmp(Effect.allocs, [MILTy.refty ty])
  end

(*......................................................................*)
(* Dereferencing							*)
(*......................................................................*)
| Deref v =>
  let
    val refty = 
      case v of 
        Var x => #1 (Var.lookup(tyenv,x))
      | _ => #1 (flowVal env v)

    val ty = case MILTy.fromRefty refty of
      SOME ty => ty
    | NONE => Debug.fail "Flow.flowCmp: expected reference type"
  in
    MILTy.cmp(Effect.reads, [ty])
  end

(*......................................................................*)
(* Assignment      							*)
(*......................................................................*)
| Assign(v1, v2) =>
  ((case v1 of Var x => #1 (Var.lookup(tyenv,x)) | _ => #1 (flowVal env v1));
  flowVal env v2;
  MILTy.cmp(Effect.writes, []))
    
(*......................................................................*)
(* Internal Java class definition					*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun transMethod (i,(name, mods, tys, tyopt, optabs)) =
          case optabs of 
            NONE => 
            ()

          | SOME (f, (xs, e)) =>
            let
              val argtys = 
                if List.exists (fn Method.STATIC => true | _ => false) mods
                then tys
                else classname::tys
              val tyenv = 
                ListPair.foldl (fn (x,ty,tyenv) => 
                  Var.Map.insert(tyenv,x,(ty,false))) tyenv (xs,argtys)
            in
              ignore (flowCmp (newScope(inTail scope, MILPath.LetClass f), 
                tyenv,kindenv) e)
            end
  in
    Gen.appi transMethod methods;
    MILTy.cmpTypePlus(flowCmp env e, Effect.io)
  end
  
(*......................................................................*)
(* Recursive functions.                                                 *)
(*......................................................................*)
| LetFun(tyvars, kind, RecFun recbinds, e) =>
  let
    fun makeFunTypes (f,g,(typedvars,e):MILTerm.TAbstr,cty) =
      ((f, MILTy.forall(tyvars, MILTy.arrow(map #2 typedvars, cty))),
       (g, MILTy.arrow(map #2 typedvars, cty)))

    val pairs = map makeFunTypes recbinds
    val (bodyfuns,defnfuns) = ListPair.unzip pairs
    val defntyenv = tyenvPlusTypedVars tyenv defnfuns
    val defnkindenv = Var.extend(kindenv, tyvars)

    val bodytyenv = tyenvPlusTypedVars tyenv bodyfuns

    fun transDef (f, g, (xs, e), cty) =
        flowCmp (newScope(
          if not(MILTermOps.isLocal kind) then inTail scope else scope,
          MILPath.LetFun f), tyenvPlusTypedVars defntyenv xs, defnkindenv) e
    val recbinds' = map transDef recbinds
    val cty = flowCmp (scope, bodytyenv, kindenv) e
  in
    addFunVars (scope,tyvars) (map #1 recbinds, map #2 recbinds);
    cty
  end

(*......................................................................*)
(* Non-recursive function.                                              *)
(*......................................................................*)
| LetFun(tyvars, kind, Fun (f, (xs, e1)), e2) =>
  let
    val defnkindenv = Var.extend(kindenv, tyvars)
    
    val cty = flowCmp (newScope(
      if MILTermOps.isLocal kind then scope else inTail scope,
      MILPath.LetFun f), tyenvPlusTypedVars tyenv xs, defnkindenv) e1
    val bodytyenv = Var.Map.insert(tyenv, f, 
        (MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty)), false))
    val cty = flowCmp (scope, bodytyenv, kindenv) e2
  in
    addFunVars (scope,tyvars) ([f], [Var.dummy]);
    cty
  end

end

  val _ = Counters.reset ()

  val cty = flowCmp (topTail, Var.Map.map (fn ty => (ty,false)) tyenv, 
    Var.Map.empty) e

  val apps = !apps
  val funkinds = !funkinds
  val hoists = !hoists
  

  fun isLocal v = case Var.Map.find(funkinds, v) of 
                      SOME (LocalFun,_) => true
                    | _ => false

  val normaliseScope = FlowOps.normaliseScope (not o isLocal)

(*----------------------------------------------------------------------*)
(* Add non-top-level ref variables to higher-order set.			*)
(*----------------------------------------------------------------------*)

  val higherrefs = 
    Var.Map.foldli (fn (x,scope,h) =>
    case normaliseScope scope of (_,[]) => h | _ => Var.Set.add(h,x))
    (!higherrefs) (!refscopes)

  val _ = 
    if Controls.isOn "showGlobalRefs"
    then Debug.print ("\n  higherrefs = {" ^ Pretty.simpleVec "," 
            Var.toString (Var.Set.listItems higherrefs) ^ "}\n")
    else ()

  val _ = 
    if doFloatBindings andalso Controls.isOn "showFloat"
    then Debug.print ("\nfloated bindings = " ^
      (MILPathOps.Map.toString (fn (x,v) => Var.toString x
        ^ " = " ^ MILPretty.valToString v) (!floatedvars)) ^ "\n")
    else ()

  val _ = 
    if doHoistBindings andalso Controls.isOn "showHoist"
    then Debug.print ("\nhoisted bindings = " ^ 
      Pretty.simpleVec "\n" 
      (fn (x, { defn, use, value }) => Var.toString x
         ^ " = " ^ MILPretty.valToString value) 
         (Var.Map.listItemsi (!hoistedvars)) ^ "\n")
    else ()

  val hoistedvartyvars = !floatedvartyvars

  val (hoistedvals, hoistedvartyvars) =
    Var.Map.foldli
    (fn (x, { defn, use, value }, (hoistedvals,hoistedvars)) =>
      case use of
        SOME use =>
        (case hoistScope (defn,use) of
          NONE => 
          (hoistedvals,hoistedvars)

        | SOME newscope =>
          if enabled hoistVal then 
            (MILPathOps.Map.insert(hoistedvals, newscope, (x,value)),
             Var.Map.insert(hoistedvars, x, (x, [])))
          else (hoistedvals,hoistedvars)
        )

      | NONE => 
        (hoistedvals,hoistedvars)

    ) (!floatedvars, hoistedvartyvars) (!hoistedvars)


(*
  val _ = 
      if Controls.isOn "showFlow"
      then
        Debug.print ("\nFlow: apps = {" ^ Pretty.simpleVec "; "
          (fn (v,scopes) => Var.toString v ^ ":" ^ 
             Pretty.simpleVec "," scopeToString scopes) 
          (Var.Map.listItemsi apps) ^ "}\n  defs = {" ^ Pretty.simpleVec "; "
          (fn (v,(funkind,scope)) => 
            MILPretty.kindToString funkind ^ 
             Var.toString v ^ ":" ^ scopeToString scope)
          (Var.Map.listItemsi funkinds) ^ 
          "}\n  higherrefs = {" ^ Pretty.simpleVec "," 
            Var.toString (Var.Set.listItems higherrefs) ^
          "}\n  hoists = {" ^ Pretty.simpleVec ","
             Var.toString (Var.Set.listItems hoists) ^ "}\n  bindvars = {" ^
          Pretty.simpleVec "; " (fn (v, {defn,use,value}) => 
            Var.toString v ^ ":" ^
            MILPathOps.toString defn ^
            (case use of NONE => "" | SOME scope => ":" ^ 
              MILPathOps.toString scope) ^
            (case value of NONE => "" | SOME v => "=" ^ 
               MILPretty.valToString v))
              (Var.Map.listItemsi bindvars) ^ "}\n  bhoist = {" ^
          Pretty.simpleVec "; " (fn (v, scope) => Var.toString v ^ ":" ^
            MILPathOps.toString scope) (Var.Map.listItemsi bhoist) ^ "}\n")
      else ()
*)
in
  { apps = apps, kinds = funkinds, higherrefs = higherrefs, 
    hoistedvals = hoistedvals, hoistedvars = hoistedvartyvars,
    hoistedfuns = MILPathOps.Map.empty, hoistedfunvars = Var.Set.empty }
end


end (* of local open *)
end (* of struct *)

