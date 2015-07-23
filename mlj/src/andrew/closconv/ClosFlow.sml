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
(* Perform a flow analysis for closure functions.			*)
(*======================================================================*)
structure ClosFlow :> CLOSFLOW =
struct

(*----------------------------------------------------------------------*)
(* A partition of closure function variables by the equivalence `might  *)
(* flow to the same application as'.                                    *)
(*----------------------------------------------------------------------*)
structure Partition = Partition(Var.Map)

local 
  open FVInfoEnv MILTerm 
in

(*----------------------------------------------------------------------*)
(* Function/method info: see signature for details.			*)
(*----------------------------------------------------------------------*)
type FunInfo = 
{
  kind : MILTerm.FunKind option,
  args : MILTerm.TypedVar list,
  cty  : MILTy.CmpType,
  tyvars : (Var.Var * MILTy.Kind) list
} Var.Map.map


(*----------------------------------------------------------------------*)
(* Gather information about a term ce.                                  *)
(*----------------------------------------------------------------------*)
fun gather ce =
let


(*----------------------------------------------------------------------*)
(* Free variable info for functions and methods   			*)
(* Methods have kind=NONE.                                              *)
(*----------------------------------------------------------------------*)
val funinfo = 
  ref (Var.Map.empty : FunInfo)

val iter = ref 1

(*----------------------------------------------------------------------*)
(* Store a function's info;				                *)
(*----------------------------------------------------------------------*)
fun addFun (env : Env, f, g, kind, tyvars, vs, cty) =
  let
    val temp = Var.Map.insert(!funinfo, f, 
    { kind = kind, cty = cty, args = vs, 
      tyvars = tyvars @ Var.Map.listItemsi (#kindenv env) })
  in
    funinfo := 
    (if Var.isDummy g then temp
     else
       Var.Map.insert(temp, g, { kind = kind, args = vs, cty = cty, 
         tyvars = tyvars @ Var.Map.listItemsi (#kindenv env) }))
  end

val partition = ref (Partition.new_partition [])

(*----------------------------------------------------------------------*)
(* Possible closure functions for applications.  			*)
(*----------------------------------------------------------------------*)
val apps = ref (Var.Map.empty : Var.Var Var.Map.map)

(*----------------------------------------------------------------------*)
(* What's the app method number for this type?				*)
(*----------------------------------------------------------------------*)
fun appMethod (env : FVInfoEnv.Env,f,ty) =
let
  val parts = Partition.list_parts (!partition)
  val ty = 
    case MILTy.fromForall ty of
      SOME (a as (kinds, _)) => 
      let  
        val ty = MILTy.app (MILTy.abs a, map (fn MILTy.Bound ty => ty) kinds)
      in
        MILTy.forceBounds (#kindenv env) ty
      end

    | NONE => 
      MILTy.forceBounds (#kindenv env) ty

  val ty = 
    case MILTy.fromMu ty of
      NONE => ty
    | SOME a => MILTy.unfold a

  val SOME (argtys,cty) = MILTy.fromArrow ty

  val possparts =
    List.filter (fn part => 
    let 
      val rep::_ = Partition.list_part (!partition, part)
      val SOME { tyvars, args, cty = cty', ... } = Var.Map.find(!funinfo, rep)
      val argtys' = map #2 args
      val ty' = MILTy.arrow(argtys', cty')
      val S = foldl (fn ((x,MILTy.Bound ty),S) => Var.Map.insert(S,x,ty))
        Var.Map.empty tyvars
      val ty' = MILTy.subst S ty'
      val SOME (argtys', cty') = MILTy.fromArrow ty'
    in
      isSome (MILTy.lubs (argtys,argtys'))
      andalso 
      isSome (MILTy.lubs (#2 (MILTy.fromCmp cty), #2 (MILTy.fromCmp cty')))
      (*
      MILTy.leq (#kindenv env) (ty,ty') *)
    end) parts   
in
  case possparts of
    [] =>
    Debug.print 
    ("\nWarning: no partition for application of " ^ Var.toString f ^
      ":" ^ MILTy.toString ty)

  | firstpart::_ =>
    apps := Var.Map.insert(!apps, f, hd (Partition.list_part (!partition, 
    firstpart)));

    (case possparts of
      [] => ()
    | _ =>
      partition := Partition.union_list(!partition, possparts))
end

fun plus env typedvars =
  envPlusTypedVars env (typedvars, Other)

(*----------------------------------------------------------------------*)
(* Collect initial free variable information about a value term.	*)
(* Determine its type at the same time.                                 *)
(*----------------------------------------------------------------------*)
fun flowVal isapp env v =
  (case v of

(*......................................................................*)
(* Constant introduction                    				*)
(*......................................................................*)
  SCon (ty, jcon) => 
  ty
  
(*......................................................................*)
(* Variables								*)
(*......................................................................*)
| Var x => 
  let
    val (ty,varinfo) = lookup(env, x)
  in
    (case varinfo of
      (FunInfo AnyFun | Other) =>
      if isapp andalso !iter=2 then appMethod (env, x, ty) else ()
    | _ => ());
    ty
  end
    
(*......................................................................*)
(* Mu introduction							*)
(*......................................................................*)
| Fold(v, ty) =>
  (flowVal false env v; ty)

(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold v =>
  let
    val subty = flowVal isapp env v
    val SOME a = MILTy.fromMu subty
  in
    MILTy.unfold a
  end

(*......................................................................*)
(* Sum introduction                                			*)
(*......................................................................*)
| Inj(ty, i, vs) => 
  (flowVals env vs; ty)

(*......................................................................*)
(* Exception introduction                                               *)
(*......................................................................*)
| ExCon(excon, vs) => 
  (flowVals env vs; MILTys.topExn)

(*......................................................................*)
(* Product introduction                                           	*)
(*......................................................................*)
| Tuple vs =>
  let
    val tys = flowVals env vs
  in
    MILTy.prod tys
  end

(*......................................................................*)
(* Product elimination                                    		*)
(*......................................................................*)
| Proj(i, v) => 
  let 
    val prodty = flowVal false env v
    val SOME tys = MILTy.fromProdCon prodty
  in
    List.nth(tys,i)
  end

(*......................................................................*)
(* Coercions.                                		                *)
(*......................................................................*)
| Coerce(v, ty) =>
  (flowVal false env v; ty)

(*......................................................................*)
(* Null values       							*)
(*......................................................................*)
| Null ty =>
  ty

(*......................................................................*)
(* Type abstraction       						*)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val ty = flowVal false (envPlusTyVars env tyvars) v
  in
    MILTy.forall (tyvars, ty)
  end

(*......................................................................*)
(* Type application       						*)
(*......................................................................*)
| TApp(v, tys) =>
  let
    val polyty = flowVal isapp env v
    val SOME a = MILTy.fromForall polyty
  in
    MILTy.app(MILTy.abs a, tys)
  end

| _ =>
  MILPretty.failVal v "ClosFlow.flowVal: illegal value term")

(*----------------------------------------------------------------------*)
(* Collect initial free variable information about value terms.   	*)
(* Determine their types at the same time.                              *)
(*----------------------------------------------------------------------*)
and flowVals env vs = map (flowVal false env) vs

(*----------------------------------------------------------------------*)
(* Collect initial free variable information about a computation term. 	*)
(* Determine its type at the same time.                                 *)
(*----------------------------------------------------------------------*)
and flowCmp env ce =
let
  fun flowCases tysFor (v, bindargs, cases, optdefault) =
  let
    val defresult =  Option.map (flowCmp env) optdefault
    fun gather [] = 
        []

      | gather ((i, (xs, e))::rest) =
        let
          val env = plus env (ListPair.zip(xs,tysFor i))
          val cty = flowCmp env e
          val ctys = gather rest
        in
          cty::ctys
        end
    val ctys = gather cases
    val cty::ctys = 
      case defresult of NONE => ctys
                      | SOME cty => cty::ctys
  in
    cty
  end

in
  case ce of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(v, vs) =>
  let
    val funty = flowVal true env v
    val SOME (_, cty) = MILTy.fromArrow funty
  in
    flowVals env vs; cty
  end

(*......................................................................*)
(* Java operation							*)
(*......................................................................*)
| Java(j, vs, cty) =>
  (flowVals env vs; cty)

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  let
    val cty = flowCmp env e
    val (eff, tys) = MILTy.fromCmp cty
  in
    MILTy.cmp(Effect.allocs, tys)
  end

| Let(Alloc(GlobalRef, v), (xs, e)) =>
  (flowVal false env v; 
   flowCmp (envPlusTypedVars env (xs, FVInfoEnv.GlobalRef)) e)

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, (xs, e2)) =>
  let
    val e1ty = flowCmp env e1
    val e2ty = flowCmp (plus env xs) e2
  in  
    MILTy.unionCmpTypes(e1ty,e2ty)
  end

(*......................................................................*)
(* Value bindings                                                       *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val ty = flowVal false env v
  in
    flowCmp (plus env [(x,ty)]) e
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  let
    val tys = flowVals env vs
  in
    MILTy.noeffect tys
  end

(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case (ve,bindargs,cases,default) =>
  let 
    val ty = flowVal false env ve
    val tyss = 
      case MILTy.fromSum ty of
        SOME tyss => tyss
      | NONE => MILPretty.failVal ve "ClosFlow.flowCmp: expected sum type"
    fun tysFor i =
    let
      val tys = List.nth(tyss, i)
    in
      if bindargs then tys else [MILTy.con tys]
    end
  in
    flowCases tysFor (ve, bindargs, cases, default)
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (ve, bindargs, cases, default) =>
  (flowVal false env ve; 
   flowCases (fn _ => []) (ve, bindargs, cases, default))
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| CaseExCon (ve, bindargs, cases, default) =>
  let 
    fun tysFor ty = if bindargs then MILTys.exnTys ty else [ty]
  in
    flowVal false env ve; 
    flowCases tysFor (ve, bindargs, cases, default)
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(ve, tys, loc) =>
  (flowVal false env ve; MILTy.cmp(Effect.throws, tys))

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(e, handlers, (xs, body)) =>
  (
    flowCmp env e;
    app (fn (xs,e) => ignore (flowCmp (plus env xs) e)) handlers;
    flowCmp (plus env xs) body
  )

(*......................................................................*)
(* Conditional                                                          *)
(*......................................................................*)
| Cond(t, v1, v2, e1, e2) =>
  let
    val _ = flowVal false env v1    
    val _ = flowVal false env v2
    val cty1 = flowCmp env e1
    val cty2 = flowCmp env e2
  in
    cty1
  end

(*......................................................................*)
(* Initialisation							*)
(*......................................................................*)
| Init(x, i, v, e) =>
  (flowVal false env v; flowCmp env e)
  
(*......................................................................*)
(* Allocation       							*)
(*......................................................................*)
| Alloc (f,v) =>
  let
    val ty = flowVal false env v
  in
    MILTy.cmp(Effect.allocs, [MILTy.refty ty])
  end

(*......................................................................*)
(* Dereferencing							*)
(*......................................................................*)
| Deref v =>
  let
    val refty = flowVal false env v
    val SOME ty = MILTy.fromRefty refty
  in
    MILTy.cmp(Effect.reads, [ty])
  end

(*......................................................................*)
(* Assignment      							*)
(*......................................................................*)
| Assign(v1, v2) =>
  (flowVal false env v1;
   flowVal false env v2;
   MILTy.cmp(Effect.writes, []))

(*......................................................................*)
(* Internal Java class definition					*)
(* If there are any free variables in the methods then we put them in   *)
(* globals first and extract them in individual methods.                *)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun flowMethods [] = 
        ()

      | flowMethods ((name, mods, tys, tyopt, optabs)::methods) =
        case optabs of 
          NONE => 
          flowMethods methods 

        | SOME (f, (xs, e)) =>
          let
            val argtys = 
              if List.exists (fn Method.STATIC => true | _ => false) mods
              then tys
              else classname::tys
            val env = envNotTop (envPlusVars env (xs,argtys))
            val cty = flowCmp env e
          in
            addFun (env, f, Var.dummy, NONE, [], ListPair.zip(xs,argtys), cty);
            flowMethods methods 
          end
  in
    flowMethods methods;
    flowCmp env e
  end

(*......................................................................*)
(* Recursion.                                                           *)
(*......................................................................*)
| LetFun(tyvars, kind, def, ce) =>
  let

  (*..................................................................*)
  (* The type/kind environment for inside the defns		      *)
  (*..................................................................*)
    val defnenv = envPlusTyVars env tyvars

    val (defsvars, bodyvars) = 
      case def of
        Fun (f, _) => ([], [f])
      | RecFun recbinds => (map #2 recbinds, map #1 recbinds)

    fun flowDef (Fun (f, (xs, e))) =
        let
          (* Gather information about the body of the function *)
          val cty = flowCmp (plus defnenv xs) e          

          (* The type of the function as seen externally *)
          val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))

          val bodyenv = 
            envPlusTypedVars env ([(f, ty)], FunInfo kind)
        in
          addFun (env, f, Var.dummy, SOME kind, tyvars, xs, cty);

          bodyenv
        end

      | flowDef (RecFun recbinds) =
        let
          val defnenv = 
            foldl
            (fn ((_, g, (xs, _), cty), defnenv) =>             
            envPlusTypedVars defnenv ([(g, MILTy.arrow(map #2 xs, cty))],
              FunInfo kind)) defnenv recbinds

          fun processDef (_, g, (xs, e), cty) = flowCmp (plus defnenv xs) e
          val _ = app (ignore o processDef) recbinds
      
          val bodyenv = 
            foldl
            (fn ((f, g, (xs, _), cty), env) =>
              let
                val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))
              in
                envPlusTypedVars env ([(f, ty)], FunInfo kind)
              end) env recbinds
        in
          app (fn (f, g, (xs, _), cty) =>
            addFun (env, f, g, SOME kind, tyvars, xs, cty)) recbinds;

          bodyenv
        end

    val bodyenv = flowDef def
    val bodycty = flowCmp bodyenv ce
  in
    bodycty
  end
end 

val _ = (iter := 1; flowCmp emptyEnv ce)

val _ = partition := Partition.new_partition 
      (map #1 (Var.Map.listItemsi 
        (Var.Map.filter (fn { kind = SOME AnyFun, ...} => true | _ => false)
          (!funinfo))))

val _ = (iter := 2; flowCmp emptyEnv ce)

val funs = !funinfo

val appmeths = Gen.foldri (fn (i,part,appmeths) =>
  foldl (fn (f, appmeths) => Var.Map.insert(appmeths, f, i)) appmeths
  (Partition.list_part (!partition, part))) 
  Var.Map.empty
  (Partition.list_parts (!partition))

val appmeths = Var.Map.foldli (fn (unknown, known, appmeths) =>
  let
    val SOME m = Var.Map.find(appmeths, known)
  in
    Var.Map.insert(appmeths, unknown, m)
  end) appmeths (!apps)

val methtys = map (fn part =>
  let
    val SOME { tyvars, args, cty, ... } =     
      Var.Map.find(funs, hd (Partition.list_part (!partition, part)))
    val S = foldl (fn ((x,MILTy.Bound ty),S) => Var.Map.insert(S,x,ty))
      Var.Map.empty tyvars
  in
    MILTy.subst S (MILTy.arrow(map #2 args, cty))
  end) (Partition.list_parts (!partition))

val _ = 
if Controls.isOn "showClosFlow"
then
Debug.print 
(
  "\nFunctions: " ^ Pretty.simpleVec "," (fn (x,_) => Var.toString x)
  (Var.Map.listItemsi funs) ^

  "\nPartitions:\n" ^ Pretty.simpleVec ";\n" (fn part =>
    Pretty.simpleVec ", " 
    (fn f => 
     let val SOME { tyvars, args, cty, ... } = Var.Map.find(!funinfo, f)
     in
       "(" ^ Pretty.simpleVec "," MILTy.boundTyVarToString tyvars ^ ") " ^
       Var.toString f ^ ":" ^ MILTy.toString(MILTy.arrow(map #2 args,cty))
     end) (Partition.list_part (!partition,part)))
  (Partition.list_parts (!partition))
)

else ()

in
  { appmeths = appmeths, methtys = methtys }
end
  
end (* of local open *)
end (* of struct *)
