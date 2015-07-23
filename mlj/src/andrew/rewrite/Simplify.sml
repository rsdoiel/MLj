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
(* Typed simplification of MIL.        					*)
(* Akin to:                                                             *)
(*   "Shrinking Lambda Expressions in Linear Time", Appel & Jim, JFP'97 *)
(*                                                                      *)
(* We do cc's on let,case,try by parameterising the simplify function   *)
(* on a stack of handlers/continuations. This stack has the form        *)
(*    H1(x1)e1, ..., Hn(xn)en                                           *)
(* where each (xi)ei is an abstraction and each Hi is a list of exn     *)
(* handlers.                                                            *)
(*                                                                      *)
(* The simplifyCmp function preserves the following invariants:         *)
(*   (a) all bound names in the term and its stack are distinct;        *)
(*   (b) the census in the state monad has the correct counts for       *)
(*       non-bound variable occurrences in the term and its stack.      *)
(*======================================================================*)
structure Simplify :> SIMPLIFY =
struct

local 
  open MILTerm Counters MILPretty
in

type Stack = (MILTerm.TAbstr list * MILTerm.TAbstr) list

(*----------------------------------------------------------------------*)
(* Iterate simplifyCmp until no more redexes remain.                    *)
(*----------------------------------------------------------------------*)
fun simplify { removeEncaps, doInlineEq, doComplex, doBranches } 
  (tyenv) term =
let

  val sv = SimplifyAtom.simplify 
  val svs = SimplifyAtom.simplifyVec

(*----------------------------------------------------------------------*)
(* Reduce a beta-redex if possible					*)
(*----------------------------------------------------------------------*)
fun simplifyBeta (env : SimplifyEnv.Env) (e : MILTerm.Cmp) =
case e of
(*......................................................................*)
(* Java operation							*)
(*                                                                      *)
(* const-op:                                                            *)
(*     jop(vs) : cty                                                    *)
(* --> val (meaning-of-jop(vs))                                         *)
(*......................................................................*)
  Java(jop as (optype, _, _), vs, cty) =>
  let
    val (vs, tys) = svs env vs
    fun default () = (Java(jop, vs, cty), cty)

    fun getConsts ([], result) = 
        SOME (rev result)

      | getConsts (SCon(_,jcon)::vs, result) =
        getConsts (vs, jcon::result)

      | getConsts _ = NONE
  in
    case getConsts (vs, []) of
      NONE => 
      default ()

    | SOME jcons =>
      case ConstOps.applyOp (optype, jcons, cty) of
        NONE => 
        default ()

      | SOME jcon => 
        if enabled constOp 
        then (Triv [SCon(hd (#2 (MILTy.fromCmp cty)), jcon)], cty)
        else default ()
  end

(*......................................................................*)
(* Allocate store							*)
(*......................................................................*)
| Alloc (f,v) =>
  let
    val (v,ty) = sv env v
  in
  (
    Alloc (f,v),
    MILTy.cmp(Effect.allocs, [MILTy.refty ty])
  )
  end

(*......................................................................*)
(* Dereference pointer							*)
(* Convert non-first-class Java field access (HACK!)			*)
(*......................................................................*)
| Deref v =>
  let
    val (v, refty) = sv env v
    val ty = case MILTy.fromRefty refty of
      SOME ty => ty
    | NONE => failCmp e "Simplify.simplifyBeta: expected reference type"
    val cty = MILTy.cmp(Effect.reads, [ty])
  in
    case SimplifyEnv.lookupCmpBind (env, v) of
      SOME (x, Java((Java.FieldRef, tyopt, nameopt), vs, _)) =>
      if enabled javaField
      then (app (fn v => Census.addVal (v, 1)) vs;
        Census.addVal(v, ~1);
        (Java((Java.GetField, tyopt, nameopt), vs, cty), cty))
      else (Deref v, cty)

    | NONE =>
      (Deref v, cty)
  end

(*......................................................................*)
(* Assignment							        *)
(* Convert non-first-class Java field access (HACK!)			*)
(*......................................................................*)
| Assign(v1, v2) =>
  let
    val (v1,ty1) = sv env v1
    val (v2,ty2) = sv env v2
    val cty = MILTy.cmp(Effect.writes, [])
  in
    case SimplifyEnv.lookupCmpBind (env, v1) of
      SOME (x, Java((Java.FieldRef, tyopt, nameopt), vs, _)) =>
      if enabled javaField
      then
        (app (fn v => Census.addVal (v, 1)) vs;
        Census.addVal(v1, ~1);
        (Java((Java.PutField, tyopt, nameopt), vs @ [v2], cty), cty))
      else (Assign(v1,v2), cty)

    | NONE =>
      (Assign(v1, v2), cty)
  end

(*......................................................................*)
(* Trivial computation							*)
(*......................................................................*)
| Triv vs => 
  let
    val (vs, tys) = SimplifyVal.simplifyVec env vs
  in
  (
    Triv vs, 
    MILTy.noeffect tys
  )
  end

| _ =>
  failCmp e "Simplify.simplifyBeta: continuation term"

(*----------------------------------------------------------------------*)
(* Apply rewrites throughout a computation expr  			*)
(*----------------------------------------------------------------------*)
fun simplifyCmp (env : SimplifyEnv.Env) (S : Stack) (e : MILTerm.Cmp)  = 
let 

(*----------------------------------------------------------------------*)
(* Compare arguments to constructor with bound variables         	*)
(*----------------------------------------------------------------------*)
  fun checkArgs ([], []) = true
    | checkArgs (x::xs, Var y::args) = 
      Var.eq(x,y) andalso checkArgs (xs, args)
    | checkArgs _ = false

(*----------------------------------------------------------------------*)
(* Compare type arguments to constructor with bound type variables    	*)
(*----------------------------------------------------------------------*)
  fun checkTyArgs ([], []) = true
    | checkTyArgs (x::xs, arg::args) = 
      (case MILTy.fromTyvar arg of
        NONE => false
      | SOME y => Var.eq(x,y) andalso checkTyArgs (xs, args))
    | checkTyArgs _ = false

  fun makeLet (e, abs : MILTerm.TAbstr, cty) =
  case abs of
    (xs, Triv vs) =>
    let
      val xs' = map #1 xs
    in
      if checkArgs (xs', vs) andalso enabled letEta
      then 
      (
        app Census.removeVar xs';
        (e, cty)
      )
      else (Let(e, abs), cty)
    end

  | (xs, e' as App(v, vs)) =>
    (case SimplifyEnv.lookupFunBind(env, v) of
      SOME (x, _, LocalFun, (xs', Triv vs')) =>
      if checkArgs (map #1 xs,vs) andalso checkArgs (map #1 xs', vs')
      andalso enabled letEta
      then (Census.removeCmp e'; (e, cty))
      else (Let(e, abs), cty)

    | _ => (Let(e, abs), cty)
    )
    
  | _ => (Let(e, abs), cty)

(*----------------------------------------------------------------------*)
(* Is this a local function application? (i.e. a goto)			*)
(*----------------------------------------------------------------------*)
fun isGoto env (App (v, vs)) = 
    let
      val (v,_) = sv env v
      val (vs,_) = svs env vs
    in
    (App(v, vs), 
      case v of
        Var x => 
        (case SimplifyEnv.isLocalFun (env, x) of
          NONE => NONE
        | SOME ty => let val SOME (_,cty) = MILTy.fromArrow ty
                     in SOME cty end)
      | TApp(Var x, tys) => 
        (case SimplifyEnv.isLocalFun (env, x) of
          NONE => NONE
        | SOME ty => 
          let val SOME (_,cty) = 
            MILTy.fromArrow (MILTy.app (MILTy.abs (valOf(MILTy.fromForall ty)), tys))
          in SOME cty end
        )

      | _ => NONE)
    end

  | isGoto env e = (e, NONE)

fun isGoto' env (App (v, vs)) = 
    (case v of
        Var x => SimplifyEnv.isLocalFun' (env, x)
      | TApp(Var x, _) => SimplifyEnv.isLocalFun' (env, x)
      | _ => false)

  | isGoto' env e = false

fun printVar p v =
 (Debug.print (" " ^ p ^ ":" ^ Var.toString v ^ " "); v)

(*----------------------------------------------------------------------*)
(* Block an abstraction (xs)e if necessary			        *)
(*----------------------------------------------------------------------*)
(*
fun blockAbs env S (xs,e) =
  bind (isGoto env e) (fn (e, goto) =>
  if goto then unit (env, Gen.id, unit e)
  else
    bind freshVar (fn fvar =>
    bind (simplifyCmp env S e) (fn (e,cty) =>
    unit
    (
      bind (Census.addVar (fvar, 1)) (fn _ =>
      bind mH (fn H =>
      bind (Ops.map (fn (x,ty) => 
        bind (Census.freshVar 1) (fn x =>
        unit (x,ty))) xs) (fn xs' =>
      unit [(H, (xs', App(Var fvar, map (Var o #1) xs')))]))),
      
      fn (e',cty) => (LetFun([], LocalFun, Fun(fvar, (xs, e)), C e'), cty),

      SimplifyEnv.envPlusLocalFun (env,fvar,MILTy.arrow (map #2 xs, cty))
    )))
  )
*)

fun killStack [] = ()
  | killStack ((H,tabs)::S) =
    (Census.removeTAbstr tabs; app Census.removeTAbstr H; killStack S)

fun allDead [] = true
  | allDead ((x,_)::xs) = 
    Census.getVar x = 0 andalso allDead xs

fun maybeKill (e, eff, [(x,_)]) =
    let
      val n = Census.getVar x 
    in
      if n=0 andalso Effect.sub(eff, Effect.union(Effect.allocs, Effect.reads))
      andalso enabled deadLet
      then (Census.removeCmp e; Census.removeVar x; (true,true))
      else if n < 0  (* it's been inlined *)
      then (Census.removeVar x; (true,true))
      else (false, n=0)
    end

  | maybeKill (e, eff, xs) =
    let
      val yes = allDead xs
    in
      if yes andalso Effect.sub(eff, Effect.union(Effect.allocs, Effect.reads))
      andalso enabled deadLet
      then (Census.removeTAbstr (xs, e); (true,true))
      else (false, yes)
    end

(*----------------------------------------------------------------------*)
(* Turn a branch into a local block if doBranches is true.		*)
(*----------------------------------------------------------------------*)
fun makeBranch env (xs,e) =
if not (isGoto' env e) andalso doBranches andalso enabled branchBlock
then
let
  val xs' = map (fn (x,ty) => (Census.freshVar 1, ty)) xs
  val fvar = Census.freshVar 1
in
  (fn e' => LetFun([], LocalFun, Fun(fvar, (xs, e)), e'),
  (xs', App(Var fvar, map (Var o #1) xs')))
end
else (fn e' => e', (xs,e))
     
fun simplifyRedexResult env S (e, cty) =
  case S of
    [] => 
    (e, cty)

  | (H, (xs, e'))::S =>
    let
      val (eff, tys) = MILTy.fromCmp cty
      val (waskilled, isdead) = maybeKill (e, eff, xs)
    in
      if waskilled
      then 
        (app Census.removeTAbstr H; simplifyCmp env S e')
      else

      let
        val env' = 
        if eff = Effect.none orelse eff = Effect.allocs
        then SimplifyEnv.envPlusValCmp (env, xs, e)
        else SimplifyEnv.envPlusTypedVars (env, xs)
      in
        if null H orelse (Effect.intersection(Effect.throws, eff)=Effect.none
          andalso enabled deadTry)
        then
          (app Census.removeTAbstr H;

          case e of
            Triv vs =>
            simplifyLetVal env S (map #1 xs, vs, tys, e')

          | _ =>
            let
              val (e', cty') = simplifyCmp env' S e'
              val (waskilled,isdead) = maybeKill (e, eff, xs)
            in
              if waskilled
              then 
                (e', cty')
              else
                makeLet (e, 
                  (if isdead then map (fn (x,ty) => (Var.dummy,ty)) xs
                   else xs, e'), MILTy.unionCmpTypes (cty, cty'))
            end
          )
        else
          let
            val (e', cty') = simplifyCmp env' S e'
            fun simplifyHandlers (acc, [], C, cty) = (rev acc, C, cty)
              | simplifyHandlers (acc, (abs as (xs as [(x,ty)], e))::H,C,cty) =
                if List.exists (fn ([(_,ty')], _) =>
                  MILTy.subClass (ty, ty') = SOME true) acc
                  andalso enabled deadHandler
                then 
                (
                  Census.removeTAbstr abs; 
                  simplifyHandlers (acc, H, C, cty)
                )
                else
                let
                  val (e,cty') = 
                    simplifyCmp (SimplifyEnv.envPlusTypedVars (env, xs)) [] e
                  val (C',abs) = makeBranch env (xs,e)
                  val cty = MILTy.unionCmpTypes(cty,cty')
                in
                  simplifyHandlers (abs::acc, H, C o C', cty)
                end

            val (H, C, cty) = simplifyHandlers ([], H, fn e' => e', cty)
            val (C',(xs,e')) = makeBranch env' (xs,e')
            val e = C'(C(if null H then Let(e, (xs, e')) else 
                                        TryLet(e, H, (xs, e'))))
            val cty = MILTy.unionCmpTypes (cty,cty')
          in
            if doBranches
            then
            let
              val f = Census.freshVar 1
            in
              (LetFun([], LocalFun, Fun (f, ([], e)), App(Var f, [])), cty)
            end
            else (e,cty)
          end
      end
    end


fun contToString [] = ""
  | contToString ((H,abs)::S) =
    MILPretty.tabstrToString abs ^ "\n ; \n" ^ contToString S

(*----------------------------------------------------------------------*)
(* Abstract a continuation into a local block if necessary		*)
(* Only do the cc if doComplex is true.                                 *)
(*----------------------------------------------------------------------*)
fun blockCont env r [] = 
    (fn () => [], fn (e,cty) => (e,cty), env, NONE)

  | blockCont env r (S as (H,(xs,e))::S') =
    let
      val env' = SimplifyEnv.envPlusTypedVars (env, xs)
    in
      if doComplex andalso enabled r 
      then
      let
        val (mH, C, env) = blockHandlers env H
        val (e, gototy) = isGoto env' e        
      in
        if isSome gototy andalso justEnabled elideCont
        then
        (
          killStack S'; Census.removeTAbstr (xs,e);
          (
            fn () =>
            let
              val H = mH ()
              val abs as (_,e) = Census.renameTAbstr (xs,e)
            in
              [(H, abs)]
            end,

            fn (e',cty) => (C e', cty),

            env,

            gototy
          )
        )

        else
        let
          val (e, cty) = simplifyCmp env' S' e
          fun default () =
          let
            val fvar = Census.freshVar 0            
          in
          (
            fn () =>
            let
              val H = mH ()
              val xs' = map (fn (x,ty) => (Census.freshVar 1, ty)) xs
            in
              Census.addVar (fvar, 1);
              [(H, (xs', App(Var fvar, map (Var o #1) xs')))]
            end,
      
            fn (e',cty) => 
              (LetFun([], LocalFun, Fun(fvar, (xs, e)), C e'), cty),

            SimplifyEnv.envPlusLocalFun (env,fvar,MILTy.arrow (map #2 xs,cty)),

            SOME cty
 
          )
          end

          val xs' = map #1 xs
        in
          case e of
            Triv vs =>
            if null H andalso checkArgs (xs',vs) andalso enabled letEta2
            then 
              (app Census.removeVar xs';
              (fn () => [], Gen.id, env, SOME cty))
            else default ()

          | _ => default ()
        end
      end
    else
      (fn () => [], simplifyRedexResult env S, env, NONE)
  end

(*----------------------------------------------------------------------*)
(* Abstract handlers into local blocks if necessary			*)
(*----------------------------------------------------------------------*)
and blockHandlers env [] = 
    (fn () => [], Gen.id, env)

  | blockHandlers env ((xs,e)::H) =
    let
      val env' = SimplifyEnv.envPlusTypedVars(env,xs)
      val (m,C,env) = blockHandlers env H
      val (e, goto) = isGoto env' e
    in
      if isSome goto andalso justEnabled elideCont
      then
      (
        Census.removeTAbstr (xs,e);
        (
          fn () =>
          let
            val H = m ()
            val abs as (_,e) = Census.renameTAbstr (xs,e)
          in
            (abs::H)
          end,

          C,
  
          env
        )
      )

      else
      let
        val (e,cty) = simplifyCmp env' [] e
        val fvar = Census.freshVar 0
      in
      (
        fn () =>
        let
          val H = m ()
          val xs' = map (fn (_,ty) => (Census.freshVar 1, ty)) xs
        in
          Census.addVar (fvar, 1);
          ((xs', App(Var fvar, map (Var o #1) xs'))::H)
        end,

        fn e' =>
        LetFun([], LocalFun, Fun(fvar, (xs, e)), C e'),

        SimplifyEnv.envPlusLocalFun (env, fvar, MILTy.arrow(map #2 xs, cty))
      )
      end
    end

(*----------------------------------------------------------------------*)
(* Simplify a case construct, doing appropriate cc's and eta if poss.   *)
(*----------------------------------------------------------------------*)
fun simplifyCase env 
  (ty, tysFor, fromCons, toCons, eq, makeCase) (v,bindargs,cases,defopt) =
let
  val v' = SimplifyEnv.lookupBinding (env, v)
  fun default () =
  let
    val (mS,C,env,_) = blockCont env caseCC S
    fun matches (i, (xs, Triv [v])) =
        (case fromCons v of
          NONE => false
        | SOME (i', vs) =>
          eq(i,i') andalso checkArgs (xs, vs)
        )

      | matches _ = false

    fun simpCase (i, (xs, e)) =
    let
      val S = mS ()
      val tys = tysFor i
      val env = SimplifyEnv.envPlusVars (env, xs, tys)
      val env = if Controls.isOn "caseBind" 
        then
        (case (v, toCons (i, map Var xs)) of
          (Var x, SOME v') => 
          SimplifyEnv.envPlusValVar (env, x, v', ty)

        | (Unfold (Var x), SOME v') =>
          let
            val (ty, _) = SimplifyEnv.lookupVarVal (env, x)
          in
            SimplifyEnv.envPlusValVar (env, x, Fold(v', ty), ty)
          end

        | _ => env)

        else env

      val (e, cty) = simplifyCmp env S e
      val (C,(xs,e)) = makeBranch env (ListPair.zip(xs,tys),e)
    in
      (C, (i, (map #1 xs,e)), cty)
    end

    fun simpCases [] =
        Debug.fail "Simplify.simpCases: no clauses"

      | simpCases [c] = 
        let val (C,c,cty) = simpCase c
        in
          (C,[c],cty)
        end

      | simpCases (c::cs) = 
        let
          val (C,c,cty) = simpCase c
          val (C',cs,cty') = simpCases cs
        in
          (C o C', c::cs, MILTy.unionCmpTypes (cty,cty'))
        end

    fun simpDefault cty NONE = 
        (fn e' => e', NONE, cty)

      | simpDefault cty (SOME e) =
        let
          val S = mS ()
          val (e,cty') = simplifyCmp env S e
          val (C,(_,e)) = makeBranch env ([],e)
        in
          (C, SOME e, MILTy.unionCmpTypes(cty,cty'))
        end

    val (C',cases,cty) = simpCases cases
    val (C'',defopt,cty) = simpDefault cty defopt
    val makeCase = C'' o C' o makeCase

    fun default () = C (makeCase (v, bindargs, cases, defopt), cty)
    fun getCond () = 
      case SimplifyEnv.lookupCmpBind (env, v) of
        SOME (x, Cond(t,v1,v2,Triv [thenv],elsebranch)) =>
        SOME (x, thenv, elsebranch, fn (e1,e2) => Cond(t,v1,v2,e1,e2))

      | SOME (x, Cond(t,v1,v2,thenbranch, Triv [elsev])) =>
        SOME (x, elsev, thenbranch, fn (e1,e2) => Cond(t,v1,v2,e2,e1))

      | _ =>
        NONE

    fun default2 () =
      case getCond () of
        SOME (x, thenv, elsebranch, makeCond) =>
        let
          fun find _ [] = 
              default ()

            | find (i,args) ((i', (xs,branch))::cases') =
              if eq(i,i') andalso enabled condCase
              then 
              let
                val tys = tysFor i
                val fvar = Census.freshVar 2
                val x' = Census.freshVar 1
                val xs' = map (fn _ => Census.freshVar 1) xs
                val newcases = map 
                  (fn (i', abs) =>
                    if eq(i,i') 
                    then (i, (xs', App(Var fvar, map Var xs')))
                    else (i', abs)) 
                  cases
              in         
                Census.inlineVar x;
                let
                  val e = 
                  makeCond(App(Var fvar, args), 
                    Let(elsebranch, ([(x',ty)], 
                      makeCase (Var x',bindargs,newcases, NONE))))
                in
                  C(LetFun([], LocalFun, Fun(fvar, 
                  (ListPair.zip(xs,tys), branch)), e), cty)
                end
              end
              else find (i,args) cases' 
        in
          case fromCons thenv of
            NONE => default ()
          | SOME (i, args) => 
            if Census.getVar x<>1 then default ()
            else find (i,args) cases
        end

      | _ => default ()

    fun checkCases [] = 
        if enabled caseEta 
        then
          (app (fn (i,(xs,_)) => app Census.removeVar xs) cases;
          (C (Triv [v], cty)))
        else default2 ()

      | checkCases (c::cs) =
        if matches c
        then checkCases cs
        else default2 ()
  in
    if isSome defopt then default ()
    else checkCases cases
  end
in
  case fromCons v' of
    SOME (i, args) =>
    let
      fun find [] = 
          (case defopt of 
            NONE => 
            default ()

          | SOME e => 
            if enabled caseBeta
            then 
              (app (Census.removeAbstr o #2) cases;
              Census.addVal(v, ~1);
              simplifyCmp env S e)
            else default ()
          )

        | find ((i', abs as (xs,e'))::cases') =
          if eq(i,i') andalso enabled caseBeta
          then 
            (app (fn (i', abs) => 
               if eq(i,i') then () else Census.removeAbstr abs) cases;
            ignore (Option.map (fn e => Census.removeCmp e) defopt);
            Census.addVal(v, ~1);
            app (fn v => Census.addVal(v,1)) args;
            simplifyLetVal env S (xs, args, tysFor i, e'))
          else find cases'
    in
      find cases
    end

  | NONE =>
    default ()
end

in
case e of

(*......................................................................*)
(* Moggi-let: pull into stack.						*)
(*......................................................................*)
  Let(e, abs) =>
  (case S of
    [] =>
    simplifyCmp env [([],abs)] e

  | ((H,abs')::S) =>    
    let
      val (mH, C, env) = blockHandlers env H
      val H1 = mH ()
      val H2 = mH ()
      val (e,cty) = simplifyCmp env ((H1,abs)::(H2,abs')::S) e
    in
      (C e, cty)
    end
  )

(*......................................................................*)
(* Try-handle: pull into stack. 					*)
(*......................................................................*)
| TryLet(innere, innerH, innerabs as (innerxs, innere')) =>
  (case S of
    [] => 
    simplifyCmp env ([(innerH,innerabs)]) innere

  | (S as ((outerH,(outerxs,outere))::S')) =>    
    let
      val fvar = Census.freshVar 0
      val (outere,outercty) = 
        simplifyCmp (SimplifyEnv.envPlusTypedVars (env, outerxs)) S' outere
      val (makeH, C, env) = blockHandlers env outerH
      val env = SimplifyEnv.envPlusLocalFun(env, fvar, 
          MILTy.arrow (map #2 outerxs, outercty))

      fun makeOuterTry () =
        let
          val outerH = makeH ()
          val outerxs' = map (fn (x,ty) => (Census.freshVar 1,ty)) outerxs
        in
          Census.addVar(fvar, 1);
          (outerH, (outerxs', App(Var fvar, map (Var o #1) outerxs')))
        end

      val innerH = map (fn (ys,e) =>
         let val (outerH, outerabs) = makeOuterTry ()
         in
           (ys, TryLet(e, outerH, outerabs))
         end) innerH

      val outerH = makeH ()
      val (outerH', outerabs') = makeOuterTry ()
      val (innere, innercty) = simplifyCmp env 
        [(innerH @ outerH, (innerxs, innere')), (outerH', outerabs')] innere
    in
      (LetFun([], LocalFun, Fun (fvar, (outerxs,outere)), C innere), 
      innercty)
    end
  )

(*......................................................................*)
(* Value binding: use auxiliary function.				*)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val (v,ty) = SimplifyVal.simplify env v
  in
    simplifyLetVal env S ([x],[v],[ty],e)
  end

(*......................................................................*)
(* Initialisation							*)
(*......................................................................*)
| Init(x, i, v, e) =>
  let
    val (v,ty) = SimplifyVal.simplify env v
    val (e, cty) = simplifyCmp env S e
  in
    (Init(x, i, v, e), MILTy.cmpTypePlus(cty, Effect.writes))
  end

(*......................................................................*)
(* Class definitions							*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun simpMethod (n, ms, tys, tyopt, SOME (f,(vars, e))) =
        let 
          val argtys = 
            if List.exists (fn Method.STATIC => true | _ => false) ms
            then tys
            else classname::tys
          val env = SimplifyEnv.envPlusVars (env, vars, argtys)
          val (e', _) = simplifyCmp env [] e
        in
          (n, ms, tys, tyopt, SOME (f,(vars, e')))
        end

      | simpMethod m = m

    val (e', cty) = simplifyCmp env S e
    val (eff, tys) = MILTy.fromCmp cty
  in
    (
      LetClass(classname, classinfo, fields, map simpMethod methods, e'),
      MILTy.cmp (Effect.union (Effect.io, eff), tys)
    )
  end

(*......................................................................*)
(* Conditional elimination						*)
(*                                                                      *)
(* cond-beta:								*)
(*     if c1 t c2 then e1 else e2                                       *)
(* --> e1          if c1 t c2 is valid                                  *)  
(*                                                                      *)
(*     if c1 t c2 then e1 else e2                                       *)
(* --> e2          if c1 t c2 is not valid                              *)  
(*......................................................................*)
| Cond(t, v1, v2, e1, e2) => 
  let
    val (v1, ty1) = sv env v1
    val (v2, ty2) = sv env v2
    fun default () =     
      if doComplex
      then
(*
      let
        val (e1,cty1) = simplifyCmp env S e1
        val (e2,cty2) = simplifyCmp env S e2
      in
        (Cond(t, v1, v2, e1, e2), MILTy.unionCmpTypes(cty1,cty2))
      end
*)
      let
        val (mS, C, env, _) = blockCont env condCC S
        val S1 = mS ()
        val S2 = mS ()
        val (e1,cty1) = simplifyCmp env S1 e1
        val (e2,cty2) = simplifyCmp env S2 e2
        val (C1,(_,e1)) = makeBranch env ([],e1)
        val (C2,(_,e2)) = makeBranch env ([],e2)
      in
        C (C1 (C2 (Cond(t, v1, v2, e1, e2))), MILTy.unionCmpTypes(cty1,cty2))
      end
      else
      let
        val (e1,cty1) = simplifyCmp env [] e1
        val (e2,cty2) = simplifyCmp env [] e2
      in
        simplifyRedexResult env S (Cond(t, v1, v2, e1, e2), 
          MILTy.unionCmpTypes(cty1,cty2))
      end

    fun applyTest (JavaTest t, SCon(_, jcon1), SCon(_, jcon2)) =
        ConstOps.applyTest (t, jcon1, jcon2)

      | applyTest (MLEq, v1, v2) =
        MILTermOps.valEq (v1,v2)

      | applyTest _ = NONE

    val v1' = SimplifyEnv.lookupBinding (env, v1)
    val v2' = SimplifyEnv.lookupBinding (env, v2)

    fun applyInline (v,i,vs) =
    let
      val (mS, C, env, _) = blockCont env condCC S
      val S1 = mS ()
      val S2 = mS ()
      val (e1,cty1) = simplifyCmp env S1 e1
      val (e2,cty2) = simplifyCmp env S2 e2
    in
      Census.addVal(v1, ~1); Census.addVal(v2, ~1);
      let
        val cty = MILTy.unionCmpTypes(cty1,cty2)
        val (C', e2') =           
          if null vs
          then (Gen.id, e2)
          else 
            let val f = Census.freshVar (length vs+1)
            in
              (fn e => LetFun([], LocalFun, Fun(f, ([], e2)), e), 
              App(Var f, []))
            end
        val abs = foldr (fn (v,(xs,e)) => 
          let val x = Census.freshVar 1
          in
            Census.addVal(v,1);
            (x::xs, Cond(MLEq, Var x, v, e, e2'))
          end) ([],e1) vs
      in
        Census.addVal(v, 1);
        C (C' (Case(v, true, [(i, abs)], SOME e2')), cty)
      end
    end

  in
    case applyTest (t, v1', v2') of 
      NONE => 
      (case (v1',v2') of
          (_, Inj(_, i2, vs)) =>    
          if doInlineEq andalso enabled inlineEq
          then applyInline (v1',i2,vs)
          else default () 

        | (_, Fold(Inj(_, i2, vs),_)) =>    
          if doInlineEq andalso enabled inlineEq
          then applyInline (Unfold v1',i2,vs)
          else default () 

        | (Inj(_, i1, vs),_) =>
          if doInlineEq andalso enabled inlineEq
          then applyInline (v2',i1,vs)
          else default () 
            
        | (Fold(Inj(_, i1, vs),_), _) =>
          if doInlineEq andalso enabled inlineEq
          then applyInline (Unfold v2',i1,vs)
          else default () 
            
        | _ => default ()
      )

    | SOME result => 
      if enabled condBeta
      then
        if result 
        then 
         (Census.removeCmp e2; Census.addVal(v1, ~1); Census.addVal(v2, ~1);
          simplifyCmp env S e1)
        else 
         (Census.removeCmp e1; Census.addVal(v1, ~1); Census.addVal(v2, ~1);
          simplifyCmp env S e2)
      else default ()
  end

(*......................................................................*)
(* Sum elimination							*)
(*                                                                      *)
(* case-beta:								*)
(*     case in_i <v_1,...,v_n> of ... in_i <x_1,...,x_n> => e ...       *)
(* --> let x_1 = v_1 in ... in let x_n = v_n in e                       *)
(*                                                                      *)
(*     case in_i <v_1,...,v_n> of ... | _ => e ...    (in_i not a case) *)
(* --> e                                                                *)
(*                                                                      *)
(* sum-eta:								*)
(*     case v of in_1 xs_1 => val (in_1 xs_1) | ...                     *)
(*             | in_n xs_n => val (in_n xs_n)                           *)
(* --> v                                                                *)
(*                                                                      *)
(* cond-case:                                                           *)
(*     let x <= if v then true else false                               *)
(*     in C[case x of true => e1 | false => e2]                         *)
(* --> C[if v then e1 else e2]    (x occurs once)                       *)
(*                                                                      *)
(*......................................................................*)
| Case (v, bindargs, cases, defopt) => 
  let
    val (v,ty) = sv env v
    val tyss = case MILTy.fromSum ty of
      SOME tyss => tyss
    | NONE => failVal v "Simplify.simplifyCmp: expected sum type"

    fun tysFor i = 
    let 
      val tys = (List.nth(tyss,i)) handle Subscript => 
        failCmp e "Simplify.simplifyCmp: out of range summand"
    in
      if bindargs then tys
      else [MILTy.con tys]
    end

    fun fromCons (Inj(ty', i', args)) = 
        if bindargs andalso MILTy.eq (ty, ty') 
        then SOME (i', args)
        else NONE

      | fromCons _ = NONE

    fun toCons (i, args) = if bindargs then SOME (Inj(ty, i, args)) else NONE
  in
    simplifyCase env (ty, tysFor, fromCons, toCons, op=, Case) 
      (v,bindargs,cases,defopt)
  end

(*......................................................................*)
(* Special constant elimination						*)
(*                                                                      *)
(* case-beta:								*)
(*     case c of ... c => e ...                                         *)
(* --> e                                                                *)  
(*                                                                      *)
(*     case c of ... | _ => e                                           *)
(* --> e     (c not a case)                                             *)
(*......................................................................*)
| CaseSCon (v, bindargs, cases, defopt) => 
  let
    val (v,ty) = sv env v
    fun fromCons (SCon(ty',i')) = 
        if MILTy.eq (ty,ty') then SOME (i', []) else NONE
      | fromCons _ = NONE

    fun toCons (i, args) = SOME (SCon(ty, i))
  in
    simplifyCase env (ty, fn _ => [], fromCons, toCons,
      fn (i,i') => Constants.equal(i,i',false), CaseSCon) 
      (v, bindargs, cases, defopt)
  end

(*......................................................................*)
(* Exception elimination						*)
(*                                                                      *)
(* case-beta:                                                           *)
(*     case ex_i <v_1,...,v_n> of ... ex_i <x_1,...,x_n> => e ...       *)
(* --> let x_1 = v_1 in ... in let x_n = v_n in e                       *)
(*                                                                      *)
(*     case ex_i <v_1,...,v_n> of ... | _ => e ...    (ex_i not a case) *)
(* --> e                                                                *)
(*                                                                      *)
(*......................................................................*)
| CaseExCon (v, bindargs, cases, defopt) => 
  let
    val (v,ty) = sv env v
    fun fromCons (ExCon(exname', args)) = 
        (* if bindargs then SOME (exname', args) else *) NONE
      | fromCons _ = NONE
    fun toCons (ty, args) = 
        if bindargs then SOME (ExCon(ty, args)) else NONE
    fun tysFor ty =
      if bindargs then MILTys.exnTys ty else [ty]

    val (result,cty) = 
      simplifyCase env (ty, tysFor, fromCons, toCons, MILTy.eq, CaseExCon)
      (v, bindargs, cases, defopt)
  in
    if doBranches
    then
    let
      val f = Census.freshVar 1
    in
      (LetFun([], LocalFun, Fun (f, ([], result)), App(Var f, [])), cty)
    end
    else (result,cty)
  end

(*......................................................................*)
(* dead-letrec:								*)
(*    letrec (tvs) f_1 xs_1 = e_1, ..., f_n xs_n = e_n in e  -->  e     *)
(*       if f_1, ..., f_n do not occur free in e                        *)
(*......................................................................*)
| LetFun(tyvars, kind, def, body) =>
  let
    val doLocalCC = 
      kind=LocalFun andalso not (null S) andalso justEnabled letFunCC     
      andalso doComplex andalso null tyvars 

    val (mS, C, env, ctyopt) = 
      (* Haven't worked out how to do this for polymorphic functions yet *)

      if kind=LocalFun
      then (blockCont env (if null tyvars then letFunCC else letPolyFunCC) S)
      else (fn () => [], Gen.id, env, NONE)

    (* The kind environment for type checking the definitions themselves *)
    val defnenv = SimplifyEnv.envPlusTyVars (env, tyvars)

    (* Purify types in bindings if possible; construct type environment *)
    val (def, defnenv) = 
      case def of
        Fun _ => 
        (def, defnenv)

      | RecFun recbinds =>
        let
          val (recbinds, defnenv) = 
            foldr (fn ((f,g,tabs as (vs,e),cty), (recbinds, env)) =>
            let
              val cty = getOpt(ctyopt, cty)
              val (eff,tys) = MILTy.fromCmp cty
              val cty = 
                if SimplifyOps.isPure e then MILTy.noeffect tys else cty
            in
              ((f, g, tabs, cty)::recbinds, 
              if kind=LocalFun 
              then
                SimplifyEnv.envPlusLocalFun(env,g,MILTy.arrow (map #2 vs, cty))
              else
                SimplifyEnv.envPlusVars 
                  (env, [g], [MILTy.arrow(map #2 vs, cty)]))
            end) ([], defnenv) recbinds
        in
          (RecFun recbinds, defnenv)
        end

    fun kill (Fun (f, abs)) =
        let
          val n = Census.getVar f
        in
          if n=0 andalso enabled deadLetFun
          then 
            (Census.removeTAbstr abs;
             Census.removeVar f; true)
          else if n < 0  (* it's been inlined *)
          then (Census.removeVar f; true)
          else false
        end
      
      | kill (RecFun defs) =
        let 
          fun test [] = 
              if enabled deadLetRec
              then 
                (app (Census.removeTAbstr o #3) defs;
                 app (fn (f,g,_,_) => (Census.removeVar f; Census.removeVar g))
                 defs;
                 true
                )
              else false

            | test ((f,_,_,_)::rest) =
              Census.getVar f = 0 andalso test rest
        in
          test defs
        end

    fun canEta ((typedvars, App(Var g, vs)) : MILTerm.TAbstr) = 
        if checkArgs (map #1 typedvars, vs) 
        then SOME g else NONE

      | canEta ((typedvars, App(TApp(Var g, tys), vs)) : MILTerm.TAbstr) =
        if checkArgs (map #1 typedvars, vs) 
        andalso checkTyArgs(map #1 tyvars, tys)
        then SOME g else NONE

      | canEta _ = NONE        

    fun simplifyDefs (def as Fun (f, abs as (xs, e))) =     
(*   
        if Census.getVar f = 1 andalso justEnabled arrowBeta
        then
          (def, 
          SimplifyEnv.envPlusFunBind(env, f, (tyvars,kind,abs), MILTy.prod []))
        else
*)
        let
          val S = mS ()
          val (e, cty) = simplifyCmp 
            (SimplifyEnv.envPlusTypedVars (defnenv, xs)) S e
          val abs = (xs, e)
          val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))
          fun default () = 
            (Fun (f, abs),
             SimplifyEnv.envPlusFunBind (env, f, (tyvars, kind, abs), ty))
        in
          case canEta abs of
            NONE => 
            default ()

          | SOME g => 
            if not doBranches andalso justEnabled arrowEta 
            then ((Fun (f,abs), 
              SimplifyEnv.envPlusValVar (env, f, Var g, ty)))
            else default ()
        end
          
            
      | simplifyDefs (RecFun recbinds) =
        let
          val (recbinds, bodyenv) = foldr 
            (fn ((f,g,(xs,e),cty), (result,env)) =>
            let 
              val S = mS ()
              val (e,cty') = 
                simplifyCmp (SimplifyEnv.envPlusTypedVars (defnenv,xs)) S e
              val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty'))
            in
              ((f,g,(xs,e),cty')::result,
              if kind=LocalFun
              then SimplifyEnv.envPlusLocalFun (env,f,ty)
              else SimplifyEnv.envPlusTypedVars (env,[(f, ty)]))
            end) ([], env) recbinds
        in
          (RecFun recbinds, bodyenv)
        end

    val S = if kind=LocalFun then mS () else S

  in
    (* If none of the variables are used in the body, do a deadLetrec *)
    if kill def then C(simplifyCmp env S body)
    else

    let 
      (* First apply simplifyCmp to the definitions *)
      val (def, bodyenv) = simplifyDefs def

      (* Now simplify the body, possibly with function bindings available *)
      val (body, cty) = simplifyCmp bodyenv S body

    in
      if kill def
      then C(body, cty)
      else 
      case def of
        Fun _ => 
        C(LetFun(tyvars, kind, def, body), cty)

      | RecFun recbinds =>
        if List.all (fn (_,g,_,_) => Census.getVar g = 0) recbinds
          andalso enabled removeRec
        then
          C(foldr (fn ((f,_,tabs,_),e) => LetFun(tyvars,kind,Fun (f,tabs),e))
            body recbinds, cty)
        else
          C(LetFun(tyvars, kind, def, body), cty)
    end
  end

(*......................................................................*)
(* try-throw:							        *)
(*      trylet ys <= throw v                                            *)
(*        handle (x_1:ty_1) => e_1 | ... | (x_n:ty_n) => e_n            *)
(*      in e                                                            *)
(*                                                                      *)
(* [1: if all ty_j for j<i definitely don't match v but ty_i does]      *)
(* -->  let x_i = v in e_i                                              *)
(*                                                                      *)
(* [2: if all ty_i definitely don't match v]                            *)
(* -->  raise v                                                         *)
(*                                                                      *)
(* [3: otherwise]                                                       *)
(* -->  typecase v of                                                   *)
(*        x_1:ty_1 => e_1                                               *)
(*      | ...                                                           *)
(*      | x_n:ty_n => e_n                                               *)
(*      | _ => raise v                                                  *)
(*......................................................................*)
| Throw (v, tys, loc) =>
  let
    val (v,ty) = sv env v

    (* What do we know for sure about the exception that's raised? *)
    val exnty = 
      case SimplifyEnv.lookupBinding (env,v) of
        ExCon(exnty, _) => SOME exnty
      | _ => NONE     

    (* Does a match against a class succeed? Return NONE for "don't know" *)
    fun matches exnty' =
      if MILTys.isTopExn exnty' 
      then SOME true
      else 
        case exnty of
          NONE => NONE
        | SOME exnty => MILTy.subClass (exnty, exnty')
  in

  case S of
    [] => 
    (Throw(v, tys, loc), MILTy.cmp(Effect.throws, tys))
          
  | ((H,(xs,e))::S) =>
    let
      fun find (resid, H) =  
        case H of

          (*..........................................................*)
          (* 2: none match       				      *)
          (*..........................................................*)       
          [] =>
          let
            (* Must do this just to find out its type! *)
            val (e, cty) = 
              simplifyCmp (SimplifyEnv.envPlusTypedVars (env,xs)) S e
            val (_,tys) = MILTy.fromCmp cty
          in
            app Census.removeTAbstr resid;
            Census.removeTAbstr (xs,e);
            (Throw(v, tys, loc), MILTy.cmp(Effect.throws, tys))
          end

        | (h as ([(exnvar, exnty)], e1))::H' =>
          case matches exnty of

          (*..........................................................*)
          (* 3: this handler might match			      *)
          (*..........................................................*)       
            NONE => 
            let
              val (e, cty) = 
                simplifyCmp (SimplifyEnv.envPlusTypedVars (env,xs)) S e
              val (_,tys) = MILTy.fromCmp cty
            in
              Census.removeTAbstr (xs,e);
              (CaseExCon(v, false, 
                map (fn ([(x,ty)],e) => (ty, ([x], e))) H,
                  SOME (Throw(v, tys, loc))), cty)
            end

          (*..........................................................*)
          (* this handler does not match			      *)
          (*..........................................................*)       
          | SOME false => 
            find (h::resid, H')

          (*..........................................................*)
          (* 1: this handler does match  			      *)
          (*..........................................................*)       
          | SOME true => 
            (killStack ((resid @ H',(xs,e))::S);
            simplifyLetVal env [] ([exnvar], [Coerce(v, exnty)],[exnty],e1))
    in
      find ([], H)
    end
  end

(*......................................................................*)
(* elide-encap:                                                         *)
(*     encap e                                                          *)
(* --> e                                                                *)
(*                                                                      *)
(* dead-encap:                                                          *)
(*     trylet xs <= encap e handle H in e'                              *)
(* --> e'   [if xs not free in e']                                      *)
(*......................................................................*)
| Encap e =>
  let
    fun reduceEffect cty = 
      MILTy.cmp(Effect.allocs, #2 (MILTy.fromCmp cty))
  in
    case S of
      [] =>
      let
        val (e, cty) = simplifyCmp env S e
      in
      (
        if removeEncaps andalso enabled elideEncap then e else Encap e,
        reduceEffect cty
      )
      end

    | (H, (xs,e'))::S' =>
      if allDead xs andalso enabled deadEncap
      then 
        (Census.removeTAbstr(xs, e); app Census.removeTAbstr H;
        simplifyCmp env S' e')
      else
 
      if removeEncaps andalso enabled elideEncap 
      then simplifyCmp env S e
      else 
      let
        val (e, cty) = simplifyCmp env [] e
        val (e', cty') = 
          simplifyCmp (SimplifyEnv.envPlusTypedVars (env,xs)) S' e'
      in
        app Census.removeTAbstr H;
        makeLet(Encap e, (xs, e'), MILTy.unionCmpTypes(reduceEffect cty, cty'))
      end
  end

(*......................................................................*)
(* Arrow elimination    						*)
(*                                                                      *)
(* arrow-beta:								*)
(*     (\ <x_1:ty_1, ..., x_n:ty_n> . e) <v_1, ..., v_n>                *)
(*       -->  let x_1,...,x_n = val <v_1,...,v_n> in e                  *)
(*......................................................................*)
| App(v, vs) =>  
  let
    val (vs, tys) = svs env vs

    fun default () = 
    let
      val (v, ty) = sv env v
      val cty = 
        case MILTy.fromArrow ty of
          SOME (_,cty) => cty
        | NONE => failCmp e "Simplify.simplifyBeta: expected arrow type"
    in

      if isGoto' env e
      andalso justEnabled elideCont
      then 
        (app (fn (H,abs) =>
          (app Census.removeTAbstr H; Census.removeTAbstr abs)) S;
        (App(v,vs), cty))
      else simplifyRedexResult env S (App(v, vs), cty)
    end
  in
    case SimplifyEnv.lookupFunBind (env, v) of
      SOME (x, tysubst, funkind, (typedvars, body)) =>
      if not doBranches andalso Census.getVar x = 1
      andalso enabled arrowBeta
      then
      ( 
        Census.inlineVar x;
        simplifyLetVal env S
          (map #1 typedvars, vs, 
          map (fn (v,ty) => MILTy.subst tysubst ty) typedvars,
          if Var.Map.numItems tysubst = 0 then body
          else MILTermOps.substCmp tysubst body)
      )
      else default ()

    | NONE => default ()
  end

(*......................................................................*)
(* All other terms are `non-continuation' terms.			*)
(*......................................................................*)
| _ =>
  simplifyRedexResult env S (simplifyBeta env e)


end (* of simplifyCmp *)

(*......................................................................*)
(* Simplify a value binding computation term of the form:		*)
(*    let x1 = v1 in ...                                                *)
(* ...let xn = vn in e                                                  *)
(*                                                                      *)
(* bind-beta:                                                           *)
(*     let x=v in e                                                     *)
(* --> e[v/x]       if v is atomic                                      *)
(*                                                                      *)
(* dead-bind:                                                           *)
(*     let x=v in e                                                     *)
(* --> e            if x does not occur in e                            *)
(*......................................................................*)
and simplifyLetVal env S ([],[],[],e) = 
    simplifyCmp env S e

  | simplifyLetVal env S (x::xs, v::vs, ty::tys, e) =
    if Census.getVar x  = 0 andalso enabled deadBind
    then 
      (Census.addVal(v, ~1); Census.removeVar x;
       simplifyLetVal env S (xs,vs,tys,e))
    else 
    let
      val (e, cty) = 
        simplifyLetVal (SimplifyEnv.envPlusValVar (env, x, v, ty)) 
        S (xs,vs,tys,e)
    in
      if Census.getVar x = 0 andalso enabled deadBind2
      then
        (Census.addVal(v, ~1); Census.removeVar x; (e, cty))
      else 
        (LetVal(x, v, e), cty)
    end

  | simplifyLetVal env S _ =
    Debug.fail "Simplify.simplifyLetVal: mismatched lists"

(*----------------------------------------------------------------------*)
(* Iterate simplifyCmp until no more redexes remain.                    *)
(*----------------------------------------------------------------------*)
  val env = SimplifyEnv.emptyEnv tyenv
  fun simp e = 
  let 
    val _ = Counters.reset ()
    val (e, cty) = simplifyCmp env [] e
    val total = (* Counters.getCount arrowBeta + 
                Counters.getCount deadLetFun +
                Counters.getCount deadLetRec *) !Counters.total
    val _ = if total<>0 then printCounts () else ()
    val _ = 
      if Controls.isOn "iterateDump" 
      then (PrintManager.print "d..."; 
        Debug.print ("\n" ^ MILPretty.cmpToString e ^ "\n"))
      else ()
    val _ = 
      if Controls.isOn "checkCensusI" 
      then (PrintManager.print "c..."; Census.checkCmp e) 
      else ()
    val _ = 
      if Controls.isOn "checkTypesI" 
      then ignore(PrintManager.print "tc..."; TypeCheck.check false  
        { tyenv = tyenv, kindenv = Var.Map.empty,
          funenv = Var.Map.empty, globenv = Var.Map.empty } e)
      else ()

  in
    if total = 0 then e
    else simp e
  end

  val e = simp term
in
  e
end

end (* of local open *)

end (* of struct *)

