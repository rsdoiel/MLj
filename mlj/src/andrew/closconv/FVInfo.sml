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
(* Gather free variable information prior to closure converting.        *)
(*======================================================================*)
structure FVInfo :> FVINFO =
struct

local 
  open FVInfoEnv MILTerm FVInfoOps 
in

(*----------------------------------------------------------------------*)
(* Function/method info: see signature for details.			*)
(*----------------------------------------------------------------------*)
type FunInfo = 
{
  kind : MILTerm.FunKind option,
  args : MILTerm.TypedVar list,
  fvs  : VarsInfo,
  cty  : MILTy.CmpType,
  tyvars : (Var.Var * MILTy.Kind) list
} Var.Map.map


fun kindToString (SOME LocalFun) = "Block"
  | kindToString (SOME AnyFun) = "Closure"
  | kindToString (SOME KnownFun) = "Global function"
  | kindToString NONE = "Method"

fun dumpFunInfo (funs : FunInfo) =
  (Debug.print ("\n" ^ Int.toString (Var.Map.numItems funs) 
     ^ " functions and methods:\n");
  app (fn (x,{ kind, args, fvs, cty, tyvars, ... }) => 
    Debug.print (kindToString kind ^ " " ^ Var.toString x ^ "\n  " ^
      "(" ^ Pretty.simpleVec "," MILTy.boundTyVarToString tyvars ^ ") <" ^
    Pretty.simpleVec "," (fn (x,ty) => Var.toString x ^ ":" ^ 
      MILTy.toString ty) args ^ "> : " ^ MILTy.cmpToString cty ^ "\n  " ^ 
       varsInfoToString fvs ^ "\n")) (Var.Map.listItemsi funs))

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

val resulttys =
  ref (Var.Map.empty : MILTy.CmpType Var.Map.map)

(*
val insts = Instances.instances ce
*)

val changed = ref true
val iter = ref 1

(*----------------------------------------------------------------------*)
(* Store a function's info;				                *)
(*----------------------------------------------------------------------*)
fun addFun (env : Env, f, g, kind, tyvars, vs, cty, info : VarsInfo) =
  let
    val temp = Var.Map.insert(!funinfo, f, 
    { kind = kind, cty = cty, args = vs, fvs =  info, 
      tyvars = tyvars @ Var.Map.listItemsi (#kindenv env) })
  in
    if !changed then ()
    else
      let 
         val SOME { fvs = oldinfo, ... } = Var.Map.find(!funinfo, f) 
      in 
        if infoEq (info, oldinfo) then () else changed := true
      end;
    
    funinfo := 
    (if Var.isDummy g then temp
     else
       (if !changed then ()
       else
       let 
         val SOME { fvs = oldinfo, ... } = Var.Map.find(!funinfo, g) 
       in 
         if infoEq (info, oldinfo) then () else changed := true
       end;
       Var.Map.insert(temp, g, { kind = kind, args = vs, cty = cty, 
         fvs = info, tyvars = tyvars @ Var.Map.listItemsi (#kindenv env) })))
  end

fun remove env (fvs, vs) =
  if isTop env 
  then removeGlobVars(fvs, vs)
  else removeFree(fvs, vs)

fun plus env typedvars =
  envPlusTypedVars env (typedvars, if isTop env then Global else Other)

(*----------------------------------------------------------------------*)
(* Collect initial free variable information about a value term.	*)
(* Determine its type at the same time.                                 *)
(*----------------------------------------------------------------------*)
fun fvVal isapp env v =
  (case v of

(*......................................................................*)
(* Constant introduction                    				*)
(*......................................................................*)
  SCon (ty, jcon) => 
  (ty, empty)
  
(*......................................................................*)
(* Variables								*)
(*......................................................................*)
| Var x => 
  let
    val (ty,varinfo) = lookup(env, x)
  in
    (* Classify variable as known fun, local fun, global or other. *)
    (* For local/global functions add in free vars of function. *)
    (* For local functions also add in globals and local funs of function. *)
    (* Closure _applications_ don't involve any extra variables *)
    case varinfo of
      FVInfoEnv.GlobalRef => 
      (ty, empty)

    | FunInfo funkind =>
      let
        val { fvs, locals, known, globvars } =
          case Var.Map.find(!funinfo, x) of
            NONE => empty
          | SOME { fvs, ... } => fvs
      in
      (ty, 
       case funkind of
        LocalFun =>
        union (singleLocal x, 
          { fvs = fvs, locals = locals, known = Var.Set.empty, 
            globvars = globvars })
        

      | KnownFun => 
        union (singleKnown x, 
          { fvs = fvs, locals = Var.Set.empty, known = Var.Set.empty,
            globvars = Var.Map.empty })

      | AnyFun => 
        (singleFree (x,ty))
      )
      end

    | Global =>
      (ty, singleGlobVar (x,ty))

    | Other =>
      (ty, singleFree (x,ty))
  end

(*......................................................................*)
(* Mu introduction							*)
(*......................................................................*)
| Fold(v, ty) =>
  let
    val (_, fvs) = fvVal false env v
  in
    (ty, fvs)
  end

(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold v =>
  let
    val (subty, fvs) = fvVal isapp env v

    (* The type of the subterm must be a mu type *)
    val SOME a = MILTy.fromMu subty

    (* Unfold this type one level *)
    val unfoldty = MILTy.unfold a
  in
    (unfoldty, fvs)
  end

(*......................................................................*)
(* Sum introduction                                			*)
(*......................................................................*)
| Inj(ty, i, vs) => 
  let
    val (tys, fvs) = fvVals env vs
  in
    (ty, fvs)
  end

(*......................................................................*)
(* Exception introduction                                               *)
(*......................................................................*)
| ExCon(excon, vs) => 
  let
    val (tys, fvs) = fvVals env vs
  in
    (MILTys.topExn, fvs)
  end

(*......................................................................*)
(* Product introduction                                           	*)
(*......................................................................*)
| Tuple vs =>
  let
    val (tys, fvs) = fvVals env vs
  in
    (MILTy.prod tys, fvs)
  end

(*......................................................................*)
(* Product elimination                                    		*)
(*......................................................................*)
| Proj(i, v) => 
  let 
    val (prodty, fvs) = fvVal false env v
    val SOME tys = MILTy.fromProdCon prodty
  in
    (List.nth(tys,i), fvs)
  end

(*......................................................................*)
(* Coercions.                                		                *)
(*......................................................................*)
| Coerce(v, outputty) =>
  let
    val (_, fvs) = fvVal false env v
  in
    (outputty, fvs)
  end

(*......................................................................*)
(* Null values       							*)
(*......................................................................*)
| Null ty =>
  (ty, empty)

(*......................................................................*)
(* Type abstraction       						*)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val (ty, fvs) = fvVal false (envPlusTyVars env tyvars) v
  in
    (MILTy.forall (tyvars, ty), fvs)
  end

(*......................................................................*)
(* Type application       						*)
(*......................................................................*)
| TApp(v, tys) =>
  let
    val (polyty, fvs) = fvVal isapp env v
    val SOME a = MILTy.fromForall polyty
  in
    (MILTy.app(MILTy.abs a, tys), fvs)
  end

| _ =>
  MILPretty.failVal v "ClosConv.fvVal: illegal value term")

(*----------------------------------------------------------------------*)
(* Collect initial free variable information about value terms.   	*)
(* Determine their types at the same time.                              *)
(*----------------------------------------------------------------------*)
and fvVals env [] = 
    ([], empty)

  | fvVals env (v::vs) =
    let
      val (ty, fvs) = fvVal false env v
      val (tys, fvs') = fvVals env vs
    in
      (ty::tys, union(fvs, fvs'))
    end

(*----------------------------------------------------------------------*)
(* Collect initial free variable information about a computation term. 	*)
(* Determine its type at the same time.                                 *)
(*----------------------------------------------------------------------*)
and fvCmp env ce =
let
  fun fvCases tysFor (v, bindargs, cases, optdefault) =
  let
    val defresult =  Option.map (fvCmp env) optdefault
    fun gather [] = 
        ([], empty)

      | gather ((i, (xs, e))::rest) =
        let
          val env = plus env (ListPair.zip(xs,tysFor i))
          val (cty, fvs) = fvCmp env e
          val (ctys, fvs') = gather rest
        in
          (cty::ctys, union(remove env (fvs, xs), fvs'))
        end
    val (ctys, fvs) = gather cases
    val (cty::ctys, fvs) = 
      case defresult of NONE => (ctys, fvs) 
                      | SOME (cty,fvs') => (cty::ctys, union(fvs,fvs'))
  in
    (cty, fvs)
  end

in
  case ce of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(ve, velist) =>
  let
    val (funty, fvs) = fvVal true env ve    
    val SOME (_, cty) = MILTy.fromArrow funty
    val (tys, fvs') = fvVals env velist    
  in
    (cty, union(fvs, fvs'))
  end

(*......................................................................*)
(* Java operation							*)
(*......................................................................*)
| Java(j, vs, cty) =>
  let
    val (tys, fvs) = fvVals env vs
  in
    (cty, fvs)
  end

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  let
    val (cty, fvs) = fvCmp env e
    val (eff, tys) = MILTy.fromCmp cty
  in
    (MILTy.cmp(Effect.allocs, tys), fvs)
  end

| Let(Alloc(GlobalRef, v), (xs, e)) =>
  let
    val (_,fvs1) = fvVal false env v
    val (cty, fvs2) = fvCmp (envPlusTypedVars env (xs, FVInfoEnv.GlobalRef)) e
  in
    (cty, union(fvs1, fvs2))
  end

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, (xs, e2)) =>
  let
    val (e1ty, fvs1) = fvCmp env e1
    val (e2ty, fvs2) = fvCmp (plus env xs) e2
  in  
    (MILTy.unionCmpTypes(e1ty,e2ty), union(fvs1, remove env (fvs2, map #1 xs)))
  end

(*......................................................................*)
(* Value bindings                                                       *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val (ty, fvs) = fvVal false env v
    val (cty, fvs') = fvCmp (plus env [(x,ty)]) e
  in
    (cty, union(fvs, remove env (fvs', [x])))
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  let
    val (tys, fvs) = fvVals env vs
  in
    (MILTy.noeffect tys, fvs)
  end

(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case (ve,bindargs,cases,default) =>
  let 
    val (ty, fvs) = fvVal false env ve
    val tyss = 
      case MILTy.fromSum ty of
        SOME tyss => tyss
      | NONE => MILPretty.failVal ve "ClosConv.fvCmp: expected sum type"
    fun tysFor i =
    let
      val tys = List.nth(tyss, i)
    in
      if bindargs then tys else [MILTy.con tys]
    end

    val (cty, fvs') = fvCases tysFor (ve, bindargs, cases, default)
  in
    (cty, union(fvs, fvs'))
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (ve, bindargs, cases, default) =>
  let 
    val (_, fvs) = fvVal false env ve
    val (cty, fvs') = fvCases (fn _ => []) (ve, bindargs, cases, default)
  in
    (cty, union(fvs, fvs'))
  end
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| CaseExCon (ve, bindargs, cases, default) =>
  let 
    val (_, fvs) = fvVal false env ve
    fun tysFor ty = if bindargs then MILTys.exnTys ty else [ty]
    val (cty, fvs') = fvCases tysFor (ve, bindargs, cases, default)
  in
    (cty, union(fvs, fvs'))
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(ve, tys, loc) =>
  let
    val (_, fvs) = fvVal false env ve
  in
    (MILTy.cmp(Effect.throws, tys), fvs)
  end

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(e, handlers, (xs, body)) =>
  let
    val (cty0, fvs0) = fvCmp env e

    val fvs1 = 
    foldr (fn ((xs, e), fvs') => 
    let
      val (cty, fvs) = fvCmp (plus env xs) e
    in
      union(fvs', remove env (fvs, map #1 xs))
    end) fvs0 handlers

    val (cty2, fvs2) = fvCmp (plus env xs) body
  in
    (cty2, union(fvs1, remove env (fvs2, map #1 xs)))
  end

(*......................................................................*)
(* Conditional                                                          *)
(*......................................................................*)
| Cond(t, v1, v2, e1, e2) =>
  let
    val (_, fvs1) = fvVal false env v1    
    val (_, fvs2) = fvVal false env v2
    val (cty1,fvs1') = fvCmp env e1
    val (cty2,fvs2') = fvCmp env e2
  in
    (cty1, foldl union fvs1 [fvs2,fvs1',fvs2'])
  end

(*......................................................................*)
(* Initialisation							*)
(*......................................................................*)
| Init(x, i, v, e) =>
  let
    val (xty,_) = lookup(env, x)
    val (ty, fvs1) = fvVal false env v
    val (cty, fvs2) = fvCmp env e
  in
    (cty, union(singleFree (x, xty), union(fvs1,fvs2)))
  end
  
(*......................................................................*)
(* Allocation       							*)
(*......................................................................*)
| Alloc (f,v) =>
  let
    val (ty, fvs) = fvVal false env v
  in
    (MILTy.cmp(Effect.allocs, [MILTy.refty ty]), fvs)
  end

(*......................................................................*)
(* Dereferencing							*)
(*......................................................................*)
| Deref v =>
  let
    val (refty, fvs) = fvVal false env v
    val SOME ty = MILTy.fromRefty refty
  in
    (MILTy.cmp(Effect.reads, [ty]), fvs)
  end

(*......................................................................*)
(* Assignment      							*)
(*......................................................................*)
| Assign(v1, v2) =>
  let
    val (_, fvs1) = fvVal false env v1
    val (_, fvs2) = fvVal false env v2
  in
    (MILTy.cmp(Effect.writes, []), union(fvs1,fvs2))
  end

(*......................................................................*)
(* Internal Java class definition					*)
(* If there are any free variables in the methods then we put them in   *)
(* globals first and extract them in individual methods.                *)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun fvMethods [] fvs = 
        fvs

      | fvMethods ((name, mods, tys, tyopt, optabs)::methods) allfvs =
        case optabs of 
          NONE => 
          fvMethods methods allfvs

        | SOME (f, (xs, e)) =>
          let
            val argtys = 
              if List.exists (fn Method.STATIC => true | _ => false) mods
              then tys
              else classname::tys
            val env = envNotTop (envPlusVars env (xs,argtys))
            val (cty, fvs) = fvCmp env e
            val methodfvs = removeFree(fvs, xs)
            val fvs = 
              removeFree (fvs, map #1 (Var.Map.listItemsi (#globvars fvs)))
          in
            addFun (env, f, Var.dummy, NONE, [],
              ListPair.zip(xs,argtys), cty, methodfvs);
            fvMethods methods (union(allfvs, methodfvs))
          end

    val fvs = fvMethods methods empty
    val (cty, fvs') = fvCmp env e
  in
    (cty, union(fvs,fvs'))
  end

(*......................................................................*)
(* Recursion.                                                           *)
(*......................................................................*)
| LetFun(tyvars, kind, def, ce) =>
  let

    val istop = isTop env

  (*..................................................................*)
  (* The type/kind environment for inside the defns		      *)
  (*..................................................................*)
    val defnenv = 
      envPlusTyVars (if MILTermOps.isLocal kind then env else envNotTop env) 
      tyvars

    val (defsvars, bodyvars) = 
      case def of
        Fun (f, _) => ([], [f])
      | RecFun recbinds => (map #2 recbinds, map #1 recbinds)

    fun fvDef (Fun (f, (xs, e))) =
        let
          (* Gather information about the body of the function *)
          val (cty, fvs) = fvCmp (plus defnenv xs) e          

          (* Remove argument variables *)
          val fvs = remove defnenv (fvs, map #1 xs)

          (* The type of the function as seen externally *)
          val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))

          val bodyenv = 
           if kind = AnyFun andalso istop
           then envPlusTypedVars env ([(f, ty)], Global)
           else envPlusTypedVars env ([(f, ty)], FunInfo kind)
        in
          resulttys := Var.Map.insert(!resulttys, f, cty);

          addFun (env, f, Var.dummy, SOME kind, tyvars, xs, cty, fvs);

          (if kind<>KnownFun then #fvs fvs else Var.Map.empty, 
           bodyenv)
        end

      | fvDef (RecFun recbinds) =
        let
          fun loop (iter, fvss) =
          let
            val defnenv = 
              ListPair.foldl
              (fn ((_, g, (xs, _), cty), fvs, defnenv) =>             
              envPlusTypedVars defnenv ([(g, MILTy.arrow(map #2 xs, cty))],
                FunInfo kind)) defnenv (recbinds, fvss)

            fun processDef (_, g, (xs, e), cty) =
            let
              val (cty, fvs) = fvCmp (plus defnenv xs) e
               
              (* Remove argument variables *)
              val fvs = remove defnenv (fvs, map #1 xs)

              (* Remove recursive occurrences where appropriate *)
              val fvs  = 
                case kind of
                  LocalFun => 
                  removeLocal(fvs, defsvars)

                | KnownFun => 
                  removeKnown(fvs, defsvars)

                | AnyFun   => 
                  removeFree(fvs, [g])
            in
              fvs
            end
      
            val newfvss = map processDef recbinds
          in
            newfvss (*
            if Eq.list infoEq (fvss, newfvss)
            then fvss
            else loop (iter+1, newfvss) *)
          end

          val fvss = loop (0, map (fn _ => empty) recbinds)
        
          val bodyenv = 
            ListPair.foldl
            (fn ((f, g, (xs, _), cty), fvs, env) =>
              let
                val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))
              in
                if kind = AnyFun andalso istop
                then envPlusTypedVars env ([(f, ty)], Global)
                else envPlusTypedVars env ([(f, ty)], FunInfo kind)
              end) env (recbinds, fvss)
        in
          ListPair.app (fn ((f, g, (xs, _), cty), fvs) =>
            addFun (env, f, g, SOME kind, tyvars, xs, cty, fvs)) 
            (recbinds, fvss);

          (#fvs (removeFree(foldl union empty fvss, defsvars)), bodyenv)
        end

    val (accfvs, bodyenv) = fvDef def
    val (bodycty, bodyfvs) = fvCmp bodyenv ce
  in
    (
      bodycty,

      (* Closures need free variables in their creation *)
      (* Local blocks are in their correct scope already *)
      if kind<>KnownFun then
        union({ fvs = accfvs, 
          globvars = Var.Map.empty, locals = Var.Set.empty, 
          known = Var.Set.empty},
          if kind=LocalFun then removeLocal (bodyfvs, bodyvars)
          else remove env (bodyfvs, bodyvars)) else bodyfvs
    )
  end
end (* of let fun fvCases *)

fun iterate (iter', ce) =
let
  val _ = iter := iter'
  val (cty, fvsinfo) = fvCmp emptyEnv ce
  val _ = 
  if Controls.isOn "showFVInfo"
  then (Debug.print ("\n**** Iteration " ^ Int.toString iter');
    dumpFunInfo (!funinfo))
  else ()
in
  if !changed 
  then 
  (
    changed := false; 
    iterate(iter'+1, ce)
  )
  else (if Controls.isOn "showFVInfo" then 
        Debug.print ("\nIterations: " ^ Int.toString iter') else ();
       fvsinfo)
end

val fvsinfo = iterate (1, ce)

val funs = !funinfo

in
  { fvs = fvsinfo, funs = funs, resulttys = !resulttys }
end
  
end (* of local open *)
end (* of struct *)
