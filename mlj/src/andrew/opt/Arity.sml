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
(* Arity-raise and de-curry known functions.                            *)
(*======================================================================*)
structure Arity :> TRANSFORMER =
struct

local 
  open MILTerm 
in

(*----------------------------------------------------------------------*)
(* The different varieties of iso.					*)
(*----------------------------------------------------------------------*)
type Isos = { argArity : bool, resArity : bool, uncurry : bool }

(*----------------------------------------------------------------------*)
(* Apply a transformation to the term e 				*)
(*----------------------------------------------------------------------*)
fun transform (tyenv) e =
let

(*----------------------------------------------------------------------*)
(* Given a value term return its type.                            	*)
(*----------------------------------------------------------------------*)
  fun transVal (env as (funs,tyenv,kindenv)) v =
  case v of

(*......................................................................*)
(* Variables								*)
(*......................................................................*)
  Var x => 
  Var.lookup(tyenv, x)

(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold v =>
  let
    val subty = transVal env v

    (* The type of the subterm must be a mu type *)
    val a = 
      case MILTy.fromMu subty of
        NONE => MILPretty.failVal v "Arity.transVal: expected mu type"
      | SOME a => a
  in
    MILTy.unfold a
  end

(*......................................................................*)
(* Coercions and sum/mu/constant introduction				*)
(*......................................................................*)
| (Coerce(_, ty) | Inj(ty, _, _) | Fold(_, ty) | SCon(ty,_)) =>
  ty

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
  in
    case MILTy.fromProdCon prodty of
      NONE => 
      MILPretty.failVal v
        "Arity.transVal: expected product/constructor type"

    | SOME tys =>
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
    val ty = transVal (funs, tyenv, Var.extend(kindenv,tyvars)) v
  in
    MILTy.forall(tyvars, ty)
  end

| Null ty =>
  ty

| _ =>
  MILPretty.failVal v "Arity.transVal: illegal value term"

and transCmp (env as (funs, tyenv, kindenv)) e =
let
  fun transCase tysFor ((i, (vs, e)), (result, cty)) =
      let 
        val tys = tysFor i
        val (e, cty') =  
          transCmp (funs, Var.extend(tyenv, ListPair.zip(vs, tys)), kindenv) e
      in
        ((i, (vs, e))::result,     
          case cty of NONE => SOME cty' 
                    | SOME cty => SOME (MILTy.unionCmpTypes(cty,cty')))
      end

  fun transCases tysFor (cases, defopt) =
    let
      val (cases, SOME cty) = foldr (transCase tysFor) ([],NONE) cases
    in
    case defopt of
      NONE => 
      (cases, NONE, cty)

    | SOME e =>
      let
        val (e, cty') = transCmp env e
      in
        (cases, SOME e, MILTy.unionCmpTypes(cty, cty'))
      end
    end

in
  case e of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(v, vs) =>
  let
    val funty = transVal env v
    val tys = map (transVal env) vs
  in
  case MILTy.fromArrow funty of
    NONE => 
    MILPretty.failCmp e "Arity.transCmp: expected arrow type"

  | SOME (_, cty) => 
    let
      val (eff,restys) = MILTy.fromCmp cty
      fun default () = (App(v, vs), cty)

      fun whichIsos (Var x) = Var.Map.find(funs, x)
        | whichIsos (TApp(Var x, _)) = Var.Map.find(funs, x)
        | whichIsos _ = NONE
    in
      case whichIsos v of
        NONE => default ()

      | SOME { argArity, resArity, uncurry } =>
        let
          fun doResult (C,args) = 
            case restys of
              [resty] =>
              (case MILTy.fromProd resty of
                SOME [] => 
                if resArity 
                then (C (Let(App(v, args), ([], Triv [Tuple []]))), cty)
                else (C (App(v, args)), cty)

              | _ => 
                (case (eff = Effect.none andalso uncurry, 
                  MILTy.fromArrow resty) of
                    (true, SOME (arg2tys, cty2)) =>
                    let
                      val vars = map (fn _ => Census.freshVar 1) arg2tys
                      val funvar = Census.freshVar 1
                    in
                      (C (LetFun([], AnyFun, Fun (funvar, 
                        (ListPair.zip(vars, arg2tys), 
                          App(v, args @ map Var vars))), 
                        Triv [Var funvar])), cty)
                    end

                  | _ => (C (App(v, args)), cty)
                )
              )

            | _ => (C (App(v, args)), cty)
        in
          case (vs, tys, argArity) of
            ([arg], [argty], true) => 
            (case MILTy.fromProd argty of
              SOME prodtys =>
              let
                val vars = map (fn _ => Census.freshVar 1) prodtys
              in
                Census.addVal (arg, length vars - 1);
                doResult 
                  (fn e' => Gen.foldli 
                  (fn (i, var, e) => LetVal(var, Proj(i, arg), e)) e' vars,
                  map Var vars)
              end

            | NONE => 
              doResult (Gen.id, [arg])
            )

          | _ =>
            doResult (Gen.id, vs)
        end
    end  
  end

(*......................................................................*)
(* Java operation							*)
(*......................................................................*)
| Java(_,_,cty) =>
  (e, cty)

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, (xs, e2)) =>
  let
    val (e1, cty1) = transCmp env e1
    val (effect, tys) = MILTy.fromCmp cty1
    val (e2, cty2) = transCmp (funs, Var.extend(tyenv, xs), kindenv) e2
  in
    (Let(e1, (xs, e2)), MILTy.unionCmpTypes(cty1,cty2))
  end
    
(*......................................................................*)
(* Value bindings.                                                      *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val ty = transVal env v
    val tyenv = Var.Map.insert(tyenv, x, ty)
    val (e, cty) = transCmp (funs, tyenv, kindenv) e
  in
    (LetVal(x, v, e), cty)
  end

(*......................................................................*)
(* Initialisation.                                                      *)
(*......................................................................*)
| Init(x, i, v, e) =>
  let
    val ty = transVal env v
    val (e, cty) = transCmp env e
  in
    (Init(x, i, v, e), cty)
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  let
    val tys = map (transVal env) vs
  in
    (Triv vs, MILTy.noeffect tys)
  end

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap e =>
  let
    val (e, cty) = transCmp env e
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap e, MILTy.cmp (Effect.allocs, tys))
  end

(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case(v, bindargs, cases, optdefault) =>
  let 
    val ty = transVal env v
    val tyss = 
      case MILTy.fromSum ty of
        SOME tyss => tyss
      | NONE => MILPretty.failVal v "Arity.transCmp: expected sum type"
    fun tysFor i =
    let val tys = List.nth(tyss, i)
    in
      if bindargs then tys else [MILTy.con tys]
    end

    val (cases, optdefault, cty) = transCases tysFor (cases, optdefault)
  in
    (Case(v, bindargs, cases, optdefault), cty)   
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (v, bindargs, cases, optdefault) =>
  let
    val (cases, optdefault, cty) = transCases (fn _ =>[]) (cases,optdefault)
  in
    (CaseSCon(v, bindargs, cases, optdefault), cty)
  end
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| CaseExCon (v, bindargs, cases, optdefault) =>
  let
    fun tysFor ty =
    if bindargs then MILTys.exnTys ty else [ty]

    val (cases, optdefault, cty) = transCases tysFor (cases,optdefault)
  in
    (CaseExCon(v, bindargs, cases, optdefault), cty)
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(v, tys, loc) =>
  (Throw(v, tys, loc), MILTy.cmp(Effect.throws, tys))

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(ce0, tabss, (vs, ce2)) =>
  let
    val (ce0, cty0) = transCmp env ce0
    val (ce2, cty2) = transCmp (funs, Var.extend(tyenv, vs), kindenv) ce2
    fun transHandler ((typedvars, ce), (result, cty)) =
        let
          val (ce, cty') = 
            transCmp (funs, Var.extend(tyenv, typedvars), kindenv) ce
        in
          ((typedvars, ce)::result, MILTy.unionCmpTypes(cty,cty'))
        end

    val (tabss, cty1) = foldr transHandler ([], cty2) tabss
  in
    (TryLet(ce0, tabss, (vs, ce2)), MILTy.unionCmpTypes(cty0,cty1))
  end

(*......................................................................*)
(* Conditional                                                          *)
(*......................................................................*)
| Cond(t, v1, v2, e1, e2) =>
  let
    val (e1, cty1) = transCmp env e1
    val (e2, cty2) = transCmp env e2
    val cty = MILTy.unionCmpTypes(cty1,cty2)  
  in
    (Cond(t, v1, v2, e1, e2), cty)
  end
  
(*......................................................................*)
(* Allocation       							*)
(*......................................................................*)
| Alloc (f,v) =>
  let
    val ty = transVal env v
  in
    (Alloc(f,v), MILTy.cmp(Effect.allocs, [MILTy.refty ty]))
  end

(*......................................................................*)
(* Dereferencing							*)
(*......................................................................*)
| Deref v =>
  let
    val refty = transVal env v
  in
    case MILTy.fromRefty refty of
      SOME ty => 
      (Deref v, MILTy.cmp(Effect.reads, [ty]))

    | NONE => 
      MILPretty.failCmp e "Arity.transCmp: expected ref type"
  end

(*......................................................................*)
(* Assignment      							*)
(*......................................................................*)
| Assign _ =>
  (e, MILTy.cmp(Effect.writes, []))

(*......................................................................*)
(* Internal Java class definition					*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let
    fun transMethod (name, mods, tys, tyopt, optabs) =
        (name, mods, tys, tyopt,
          case optabs of 
            NONE => NONE
          | SOME (f,(vs, e)) =>
            let
              val argtys = 
                if List.exists (fn Method.STATIC => true | _ => false) mods
                then tys
                else classname::tys
              val tyenv = Var.extend(tyenv, ListPair.zip(vs,argtys))
              val (e, cty) = transCmp (funs, tyenv, kindenv) e
            in
              SOME (f,(vs, e))
            end)

    val (e, cty) = transCmp env e
    val methods = map transMethod methods
    val (eff, tys) = MILTy.fromCmp cty
  in
    (LetClass(classname, classinfo, fields, methods, e), 
      MILTy.cmp(Effect.union(eff, Effect.io), tys))
  end
  
(*......................................................................*)
(* Recursive functions.                                                 *)
(*......................................................................*)
| LetFun(tyvars, funkind, RecFun recbinds, ce) =>
  let
    fun makeFunType (_, _, (vs : MILTerm.TypedVar list,ce),cty) = 
      MILTy.arrow(map #2 vs, cty)
    val funtys = map makeFunType recbinds
    val defntyenv = Var.extend(tyenv, ListPair.zip(map #2 recbinds,funtys))
    val defnkindenv = Var.extend(kindenv, tyvars)

    val bodytyenv = Var.extend(tyenv,
      map (fn (funvar,ty) => (funvar,MILTy.forall(tyvars, ty)))
          (ListPair.zip(map #1 recbinds, funtys)))

    fun doIso ((f, g, (typedvars, ce), cty), funs)  =
      if funkind <> AnyFun 
      then
        let
          val argArity = 
            (case typedvars of
              [(_,ty)] => isSome (MILTy.fromProd ty)
            | _ => false) 
            andalso Counters.enabled Counters.argArity

          val (resArity, uncurry) = 
            case MILTy.fromCmp cty of
              (eff,[ty]) => 
              (case MILTy.fromProd ty of
                 SOME [] => 
                 (Counters.enabled Counters.resultArity, false) 

               | _ => 
                if eff = Effect.none andalso isSome (MILTy.fromArrow ty)
                then (false, Counters.enabled Counters.uncurry) 
                else (false, false)
              )
            | _ => (false, false)
          val entry = { resArity = resArity, argArity = argArity, uncurry = uncurry }
        in
          Var.Map.insert(Var.Map.insert(funs, f, entry), g, entry)
        end
      else funs

    val funs = foldl doIso funs recbinds

    fun transDef (f, g, (typedvars, ce), cty) =
    let
      val (ce, _) = 
        transCmp (funs, Var.extend(defntyenv,typedvars), defnkindenv) ce

      fun default () = (f, g, (typedvars, ce), cty)

      val { argArity, resArity, uncurry } = 
        getOpt(Var.Map.find(funs,f), 
        {argArity = false, resArity = false, uncurry = false})

      fun doResult (tabs as (typedvars, e)) =
        case MILTy.fromCmp cty of
          (eff,[ty]) =>
          (case MILTy.fromProd ty of
            SOME [] => 
            if resArity
            then (f, g, (typedvars, Let(e, 
              ([(Var.dummy, ty)], Triv []))), MILTy.cmp(eff, []))
            else (f, g, tabs, cty)

          | _ => 
            (case (eff = Effect.none andalso uncurry, 
               MILTy.fromArrow ty) of
               (true, SOME (arg2tys, cty2)) =>
               let
                 val vars = map (fn _ => Census.freshVar 1) arg2tys
                 val f' = Census.freshVar 1
               in
                 (f, g, (typedvars @ ListPair.zip(vars,arg2tys),
                    Let(e, ([(f',ty)], App(Var f', map Var vars)))), cty2)
               end

             | _ => (f, g, tabs, cty)
             )
          )

        | _ => 
          (f, g, tabs, cty)

    in
      if funkind <> AnyFun then
        case typedvars of
          [(var,ty)] => 
          (case (argArity, MILTy.fromProd ty) of
            (true, SOME prodtys) =>
            let
              val vars = map (fn _ => Census.freshVar 1) prodtys
            in
              doResult (ListPair.zip(vars, prodtys),
                LetVal(var, Tuple (map Var vars), ce))
            end

          | _ =>
            doResult (typedvars, ce)
          )

        | _ => 
          doResult (typedvars, ce)
      else default ()
    end

    val recbinds = map transDef recbinds
    val (ce, cty) = transCmp (funs, bodytyenv, kindenv) ce
  in
    (LetFun(tyvars, funkind, RecFun recbinds, ce), cty)
  end

(*......................................................................*)
(* Non-recursive functions.                                             *)
(*......................................................................*)
| LetFun(tyvars, funkind, Fun (f, (typedvars,e1)), e2) =>
  let
    (* First translate the body of the function *)
    val (e1, cty) = transCmp (funs, Var.extend(tyenv, typedvars),
      Var.extend(kindenv, tyvars)) e1

    (* Decide which iso's to apply *)
    val (funs, { argArity, resArity, uncurry }) = 
      if funkind <> AnyFun 
      then
        let
          val argArity = 
            (case typedvars of
              [(_,ty)] => isSome (MILTy.fromProd ty)
            | _ => false) 
            andalso Counters.enabled Counters.argArity

          val (resArity, uncurry) = 
            case MILTy.fromCmp cty of
              (eff,[ty]) => 
              (case MILTy.fromProd ty of
                 SOME [] => 
                 (Counters.enabled Counters.resultArity,false) 

               | _ => 
                if eff = Effect.none andalso isSome (MILTy.fromArrow ty)
                then (false, Counters.enabled Counters.uncurry) 
                else (false, false)
              )
            | _ => (false, false)

          val entry = 
            { resArity = resArity, argArity = argArity, uncurry = uncurry }
        in
          (Var.Map.insert(funs, f, entry), entry)
        end
      else (funs, { resArity = false, argArity = false, uncurry = false })

      val tabs' = (typedvars, e1)

      val bodytyenv = 
        Var.Map.insert(tyenv, f, MILTy.forall(tyvars, 
          MILTy.arrow(map #2 typedvars,cty)))

      fun makeNewDef () =
      let
        fun doResult (tabs as (typedvars, e)) =
          case MILTy.fromCmp cty of
            (eff,[ty]) =>
            (case MILTy.fromProd ty of
              SOME [] => 
              (
                if resArity
                then (typedvars, Let(e, ([(Var.dummy,ty)], Triv [])))
                else tabs
              )
              

            | _ => 
              (case (eff = Effect.none andalso uncurry, 
                 MILTy.fromArrow ty) of
                  (true, SOME (arg2tys, cty2)) =>
                  let
                    val vars = map (fn _ => Census.freshVar 1) arg2tys
                    val f' = Census.freshVar 1
                  in
                    ((typedvars @ ListPair.zip(vars,arg2tys),
                    Let(e, ([(f', ty)], App(Var f', map Var vars)))))
                  end

                | _ => 
                  tabs
              ))

          | _ => 
            tabs

      in
        if funkind <> AnyFun then
          case typedvars of
            [(var,ty)] => 
            (case (argArity, MILTy.fromProd ty) of
              (true, SOME prodtys) =>
              let
                val vars = map (fn _ => Census.freshVar 1) prodtys
              in
                doResult (ListPair.zip(vars, prodtys),
                  LetVal(var, Tuple (map Var vars), e1))
              end

            | _ =>
              doResult tabs')

          | _ => 
            doResult tabs'
        else tabs'
      end

    val (e2, cty) = transCmp (funs, bodytyenv, kindenv) e2
    val tabs = makeNewDef ()
  in
    (LetFun(tyvars, funkind, Fun (f, tabs), e2), cty)
  end

end

val _ = Counters.reset ()
val (e,cty) = 
  PrintManager.process
  ("Arity raising", false)
  (fn () => transCmp (Var.Map.empty, tyenv, Var.Map.empty) e)

in
  Counters.printCounts (); e
end

end (* of local open *)
end (* of struct *)

