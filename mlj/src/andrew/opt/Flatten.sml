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
(* Uniformly apply the following type iso's:				*)
(*     T1 * ... * T(i-1) * unit * T(i+1) * ... * Tn                     *)
(* --> T1 * ... * T(i-1) * T(i+1) * ... * Tn             [unitProd]     *)
(*                                                                      *)
(*     <T1,...,T(i-1),unit,T(i+1),...,Tn> -> R                          *)
(* --> <T1,...,T(i-1),T(i+1),...,Tn> -> R                [unitArg]      *)
(*                                                                      *)
(*     Ts -> <T1,...,T(i-1),unit,T(i+1),...,Tn>                         *)
(* --> Ts -> <T1,...,T(i-1),T(i+1),...,Tn>               [unitResult]   *)
(*                                                                      *)
(*     con <T1,...,T(i-1),unit,T(i+1),...,Tn>                           *)
(* --> con <T1,...,T(i-1),T(i+1),...,Tn>                 [unitCon]      *) 
(*                                                                      *)
(*     exn_i <T1,...,T(i-1),unit,T(i+1),...,Tn>                         *)
(* --> exn_i <T1,...,T(i-1),T(i+1),...,Tn>               [unitExCon]    *) 
(*                                                                      *)
(*     con <T1 * ... * Tn>                                              *)
(* --> con <T1,...,Tn>                                   [flattenCon]   *)
(*======================================================================*)
structure Flatten :> TRANSFORMER =
struct

local 
  open MILTerm FlattenTypes Counters 
in

(*----------------------------------------------------------------------*)
(* Is the type integer-compatible i.e. usable in "if" bytecodes		*)
(*----------------------------------------------------------------------*)
fun isInt ty =
  case MILTy.fromJava ty of
    SOME jty =>
    (case jty of
      Types.INT => true
    | Types.CHAR => true
    | Types.SHORT => true
    | Types.BOOLEAN => true
    | Types.BYTE => true
    | _ => false)

  | NONE => false

fun isClass ty =
  case MILTy.fromJava ty of
    SOME (Types.CLASS _) => true
  | _ => false

(*----------------------------------------------------------------------*)
(* Apply a transformation to the term e			        	*)
(*----------------------------------------------------------------------*)
fun transform (tyenv) e =
let

  fun transCon (c as (Constants.BYTE b)) = 
    (case Constants.convert (Constants.INT b, Types.BYTE) of
      SOME (Constants.BYTE b) => Constants.INT b
    | _ => Constants.INT b)
    | transCon c = c

(*----------------------------------------------------------------------*)
(* Given a value term, translate and return the type of the original	*)
(* term.                                                                *)
(* Also return a list of value bindings.                                *)
(*----------------------------------------------------------------------*)
  fun transVal (env as (tyenv,kindenv)) v =
  let
    val transType = FlattenTypes.transType kindenv
    val transKind = FlattenTypes.transKind kindenv
  in
  case v of

(*......................................................................*)
(* Constant introduction                    				*)
(*......................................................................*)
  SCon(ty, c) => 
  (SCon (transType ty, transCon c), ty, [])

(*......................................................................*)
(* Variables								*)
(*......................................................................*)
| Var x => 
  (v, Var.lookup(tyenv, x), [])

(*......................................................................*)
(* Mu introduction							*)
(*......................................................................*)
| Fold(v, ty) =>
  let 
    val (v,subty,b) = transVal env v
  in
    (Fold(v, transType ty), ty, b)
  end

(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold v =>
  let
    val (v, subty, b) = transVal env v

    (* The type of the subterm must be a mu type *)
    val a = 
      case MILTy.fromMu subty of
        NONE => Debug.fail "Flatten.transVal: expected mu type"
      | SOME a => a

    (* Unfold this type one level *)
    val unfoldty = MILTy.unfold a
  in
    (Unfold v, unfoldty, b)
  end

(*......................................................................*)
(* Sum introduction                                			*)
(*......................................................................*)
| Inj(ty, i, vs) => 
  let 
    val (vs, tys, b) = transVals (fn () => enabled unitCon) env vs
    val ty' = transType ty
  in
    case MILTy.fromSum ty of
      SOME ([[], [_]] | [[_], []]) =>
      (Inj(ty', i, vs), ty, b)

    | _ => 
      let
        val tys = List.mapPartial (fn ty => 
        let val ty = transType ty
        in if isUnit kindenv ty then NONE else SOME ty end) tys
      in
        case (vs, map MILTy.fromProd tys) of
          ([v], [SOME tys']) =>
          if enabled flattenCon
          then 
          let
            val xs = map (fn _ => Census.freshVar 1) tys'
          in
            Census.addVal (v, length xs - 1);
            (Inj(ty', i, map Var xs), ty, 
             b @ Gen.mapi (fn (i,x) => (x,Proj(i,v))) xs)
          end
          else (Inj(ty', i, vs), ty, b)

        | _ => (Inj(ty', i, vs), ty, b)
      end
  end

(*......................................................................*)
(* Free coercion						        *)
(*......................................................................*)
| Coerce(v, ty) =>
  let
    val (v,_,b) = transVal env v
  in
    (Coerce(v, transType ty), ty, b)
  end

(*......................................................................*)
(* Exception introduction                                               *)
(*......................................................................*)
| ExCon(exname, vs) => 
  let
    val (vs,tys,b) = transVals (fn () => enabled unitExCon) env vs
  in
    (ExCon(exname, vs), MILTys.topExn, b)
  end

(*......................................................................*)
(* Product introduction                                           	*)
(*......................................................................*)
| Tuple vs => 
  let
    val (vs,tys,b) = transVals (fn () => enabled unitProd) env vs
  in
    (Tuple vs, MILTy.prod tys, b)
  end

(*......................................................................*)
(* Product elimination                                    		*)
(*......................................................................*)
| Proj(i, v) => 
  let
    val (v,prodty,b) = transVal env v
  in
    case MILTy.fromProdCon prodty of
      NONE => 
      MILPretty.failVal v "Flatten.transVal: expected product/constructor type"

    | SOME tys =>
      let 
        val resultty = List.nth(tys, i)
        val elideUnit = 
          if isSome (MILTy.fromExn prodty) then justEnabled unitExCon
          else if isSome (MILTy.fromCon prodty) then justEnabled unitCon
          else justEnabled unitProd

        fun default () = 
        (if isUnit kindenv resultty 
         then Tuple [] 
         else Proj(if elideUnit then whichProj kindenv (i, tys) else i, v), 
           resultty, b)
      in
        case MILTy.fromCon prodty of
          SOME [ty] =>
          (case MILTy.fromProd (transType ty) of
            SOME tys => 
            if enabled flattenCon 
            then 
            let
              val xs = map (fn _ => Census.freshVar 1) tys              
            in
              Census.addVal(v, length xs - 1);
              (Tuple (map Var xs), resultty,
                b @ Gen.mapi (fn (i, x) => (x, Proj(i, v))) xs)
            end
            else default ()          
          | _ => default ())

        | _ => default ()
      end
  end

(*......................................................................*)
(* Quantifier elimination						*)
(*......................................................................*)
| TApp(v, tys) => 
  let
    val (v, polyty, b) = transVal env v
    val SOME a = MILTy.fromForall polyty
  in
    (TApp(v, map transType tys), MILTy.app (MILTy.abs a, tys), b)
  end

(*......................................................................*)
(* Quantifier introduction						*)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val (v, ty, b) = transVal (tyenv, Var.extend(kindenv,tyvars)) v
  in
    (TAbs(map (fn (x,k) => (x,transKind k)) tyvars, v), 
     MILTy.forall(tyvars, ty), b)
  end

| _ =>
  MILPretty.failVal v "Flatten.transVal: illegal value term"
end

(*----------------------------------------------------------------------*)
(* Translate a vector of values, removing unit values if elideUnit ()	*)
(* returns true.                                                        *)
(*----------------------------------------------------------------------*)
and transVals elideUnit env [] = 
    ([],[],[])

  | transVals elideUnit (env as (tyenv,kindenv)) (v::vs) =
    let
      val (v,ty,b) = transVal env v
      val (vs,tys,b') = transVals elideUnit env vs
    in
      (if isUnit kindenv ty andalso elideUnit () 
       then (Census.addVal(v,~1); vs) 
       else v::vs, ty::tys, b@b')
    end

(*----------------------------------------------------------------------*)
(* Translate a computation term						*)
(*----------------------------------------------------------------------*)
and transCmp (env as (tyenv, kindenv)) ce =
let

  val transType = FlattenTypes.transType kindenv
  val transKind = FlattenTypes.transKind kindenv
  val transCmpType = FlattenTypes.transCmpType kindenv

  fun transTypedVars typedvars = 
    map (fn (v,ty) => (v, transType ty)) typedvars

  fun transCase tysFor ((i, (vs, ce)), (result, cty)) =
      let 
        val tys = tysFor i
        val (ce, cty') =  
          transCmp (Var.extend(tyenv, ListPair.zip(vs, tys)), kindenv) ce
      in
        ((i, (vs, ce))::result,     
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

    | SOME ce =>
      let
        val (ce, cty') = transCmp env ce
      in
        (cases, SOME ce, MILTy.unionCmpTypes(cty, cty'))
      end
    end

  fun makeBinds ([],e) = e
    | makeBinds ((x,v)::b,e) = LetVal(x,v,makeBinds (b,e))
in
  case ce of

(*......................................................................*)
(* Arrow elimination							*)
(*......................................................................*)
  App(v, vs) =>
  let
    val (v, funty, b) = transVal env v
    val (vs, tys, b') = transVals (fn () => enabled unitArg) env vs
  in
  case MILTy.fromArrow funty of
    NONE => 
    MILPretty.failCmp ce "Flatten.transCmp: expected arrow type"

  | SOME (_, cty) => 
    let
      val (eff,restys) = MILTy.fromCmp cty
      fun default () = (makeBinds (b@b', App(v, vs)), cty)
    in
      if List.exists (isUnit kindenv) restys andalso enabled unitResult
      then 
      let
        val (resxs, resvs) = foldr (fn (ty, (xs,resvs)) =>
          if isUnit kindenv ty then (xs, Tuple [] :: resvs)
          else let val x = Census.freshVar 1 
               in ((x,ty)::xs, Var x :: resvs) end) ([],[]) restys
      in
        (makeBinds (b@b', Let(App(v, vs), (resxs, Triv resvs))), cty)
      end
      else default ()
    end  
  end

(*......................................................................*)
(* Java operation							*)
(*......................................................................*)
| Java(j as (jop, tyopt, nameopt), vs, cty) =>
  let
    val (vs, tys, b) = transVals (fn () => false) env vs
  in
    (makeBinds (b, Java((jop, Option.map transType tyopt, nameopt), 
    vs, transCmpType cty)), cty)
  end

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(e1, (xs, e2)) =>
  let
    val (e1, cty1) = transCmp env e1
    val (effect, tys) = MILTy.fromCmp cty1
    val (e2, cty2) = transCmp (Var.extend(tyenv, xs), kindenv) e2
  in
    (Let(e1, (transTypedVars xs, e2)), MILTy.unionCmpTypes(cty1,cty2))
  end
    
(*......................................................................*)
(* Value bindings.                                                      *)
(*......................................................................*)
| LetVal(x, v, e) =>
  let
    val (v, ty, b) = transVal env v
    val tyenv = Var.Map.insert(tyenv, x, ty)
    val (e, cty) = transCmp (tyenv, kindenv) e
  in
    (makeBinds (b, LetVal(x, v, e)), cty)
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv vs =>
  let
    val (vs, tys, b) = transVals (fn () => false) env vs
  in
    (makeBinds (b, Triv vs), MILTy.noeffect tys)
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
    val (v, ty, b) = transVal env v
    val ty' = transType ty
    val tyss = 
      case MILTy.fromSum ty of
        SOME tyss => tyss
      | NONE => MILPretty.failVal v "Flatten.transCmp: expected sum type"

    fun tysFor i =
    let
      val tys = List.nth(tyss, i)
    in
      if bindargs then tys else [MILTy.con tys]
    end
    
    val (cases, optdefault, cty) = transCases tysFor (cases, optdefault)
  in
    case (bindargs,tyss) of
      (true, [[], [ty]]) =>
      let
        fun findNoneBranch [] = valOf optdefault
          | findNoneBranch ((0,([],ce))::rest) = ce
          | findNoneBranch (_::rest) = findNoneBranch rest

        fun findSomeBranch [] = ([Census.freshVar 0], valOf optdefault)
          | findSomeBranch ((1,([v],ce))::rest) = ([v], ce)
          | findSomeBranch (_::rest) = findSomeBranch rest
      in
        (makeBinds (b, 
          Case(v, true, [
            (0, ([], findNoneBranch cases)),
            (1, findSomeBranch cases)], NONE)), cty)
      end
      
    | _ =>
      (makeBinds(b, Case(v, bindargs, cases, optdefault)), cty)
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (v, bindargs, cases, optdefault) =>
  let
    val (v, ty, b) = transVal env v
    val (cases, optdefault, cty) = transCases (fn _ =>[]) (cases,optdefault)
  in
    if not (isInt ty)
    then
    let
      fun makeCond [] =
          (case optdefault of
            NONE => MILPretty.failCmp ce "Flatten.transCmp: no default in case"
          | SOME e => e)
        
        | makeCond ((jcon, (_, e))::cases) =
          Cond (MLEq, v, SCon(ty,jcon), e, makeCond cases)
        
    in
      Census.addVal(v, length cases - 1);
      (makeBinds (b,makeCond cases), cty)
    end

    else 
      (makeBinds (b, CaseSCon(v, bindargs, cases, optdefault)), cty)
  end
      
(*......................................................................*)
(* Exception elimination						*)
(*......................................................................*)
| CaseExCon (v, bindargs,cases, optdefault) =>
  let
    val (v, _, b) = transVal env v
    fun tysFor ty =
    if bindargs then MILTys.exnTys ty else [ty]

    val (cases, optdefault, cty) = transCases tysFor (cases,optdefault)
  in
    (makeBinds (b,CaseExCon(v, bindargs,cases, optdefault)), cty)
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(v, tys, loc) =>
  let
    val (v, _, b) = transVal env v
  in
    (makeBinds (b, Throw(v, map transType tys, loc)), 
      MILTy.cmp(Effect.throws, tys))
  end

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(ce0, tabss, (vs, ce2)) =>
  let
    val (ce0, cty0) = transCmp env ce0
    val (ce2, cty2) = transCmp (Var.extend(tyenv, vs), kindenv) ce2
    fun transHandler ((xs, ce), (result, cty)) =
        let
          val (ce, cty') = transCmp (Var.extend(tyenv, xs), kindenv) ce
        in
          ((transTypedVars xs, ce)::result, MILTy.unionCmpTypes(cty,cty'))
        end
    val (tabss, cty1) = foldr transHandler ([], cty2) tabss
 in
   (TryLet(ce0, tabss, (transTypedVars vs, ce2)), 
   MILTy.unionCmpTypes(cty0,cty1))
  end

(*......................................................................*)
(* Conditional                                                          *)
(*......................................................................*)
| Cond(t, v1, v2, e1, e2) =>
  let
    val (v1, ty1, b1) = transVal env v1
    val (v2, ty2, b2) = transVal env v2
    val (e1, cty1) = transCmp env e1
    val (e2, cty2) = transCmp env e2
    val cty = MILTy.unionCmpTypes(cty1,cty2)  
  in
    (makeBinds (b1 @ b2, Cond(t, v1, v2, e1, e2)), cty)
  end
  
(*......................................................................*)
(* Allocation       							*)
(*......................................................................*)
| Alloc (f,v) =>
  let
    val (v, ty, b) = transVal env v
  in
    (makeBinds (b, Alloc(f,v)), 
    MILTy.cmp(Effect.allocs, [MILTy.refty ty]))
  end

(*......................................................................*)
(* Dereferencing							*)
(*......................................................................*)
| Deref v =>
  let
    val (v, refty, b) = transVal env v
  in
    case MILTy.fromRefty refty of
      SOME ty => 
      (makeBinds (b,Deref v), MILTy.cmp(Effect.reads, [ty]))

    | NONE => 
      MILPretty.failCmp ce "Flatten.transCmp: expected ref type"
  end

(*......................................................................*)
(* Assignment      							*)
(*......................................................................*)
| Assign(v1, v2) =>
  let
    val (v1,_,b1) = transVal env v1
    val (v2,_,b2) = transVal env v2
  in 
    (makeBinds (b1 @ b2, Assign(v1,v2)), MILTy.cmp(Effect.writes, []))
  end

(*......................................................................*)
(* Internal Java class definition					*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, ce) =>
  let
    fun transMethod (name, mods, tys, tyopt, optabs) =
        (name, mods, map transType tys, Option.map transType tyopt,
          case optabs of 
            NONE => NONE
          | SOME (f,(vs, ce)) =>
            let
              val argtys = 
                if List.exists (fn Method.STATIC => true | _ => false) mods
                then tys
                else classname::tys
              val tyenv = Var.extend(tyenv, ListPair.zip(vs,argtys))
              val (ce, cty) = transCmp (tyenv, kindenv) ce
            in
              SOME (f,(vs, ce))
            end)

    fun transField (name, mods, ty, c) = 
        (name, mods, transType ty, c)

    val (ce, cty) = transCmp env ce
    val methods = map transMethod methods
    val fields = map transField fields
    val (eff, tys) = MILTy.fromCmp cty
  in
    (LetClass(classname, classinfo, fields, methods, ce), 
      MILTy.cmp(Effect.union(eff, Effect.io), tys))
  end

(*......................................................................*)
(* Recursive functions.                                                 *)
(*......................................................................*)
| LetFun(tyvars, funkind, RecFun recbinds, e) =>
  let
    fun makeFunType (_, _, (vs : MILTerm.TypedVar list,ce),cty) = 
      MILTy.arrow(map #2 vs, cty)
    val funtys = map makeFunType recbinds
    val defntyenv = Var.extend(tyenv, ListPair.zip(map #2 recbinds,funtys))
    val defnkindenv = Var.extend(kindenv, tyvars)

    val bodytyenv = Var.extend(tyenv,
      map (fn (funvar,ty) => (funvar,MILTy.forall(tyvars, ty)))
          (ListPair.zip(map #1 recbinds, funtys)))

    fun transDef (f, g, (xs, e), cty) =
    let
      val (e, cty) = transCmp (Var.extend(defntyenv,xs), defnkindenv) e
      val (eff, restys) = MILTy.fromCmp cty

      val (xs', e) = foldr (fn ((x,ty), (xs,e)) =>
        if isUnit defnkindenv ty andalso enabled unitArg
        then (xs, LetVal(x, Tuple [], e))
        else ((x,FlattenTypes.transType defnkindenv ty)::xs, e)) ([], e) xs

      val doUnitResult =
        List.exists (isUnit defnkindenv) restys andalso enabled unitResult
      val e = 
      if doUnitResult
      then 
      let
        val (resxs, resvs) = foldr (fn (ty, (xs,resvs)) =>
          if isUnit defnkindenv ty then ((Var.dummy,MILTy.prod [])::xs, 
            resvs)
          else let val x = Census.freshVar 1 
               in ((x,FlattenTypes.transType defnkindenv ty)::xs, 
                   Var x :: resvs) end) ([],[]) restys
      in
        Let(e, (resxs, Triv resvs))
      end
      else e

    in
      ((f, g, (xs', e), FlattenTypes.transCmpType defnkindenv cty), 
      doUnitResult)
    end

    val (recbinds,doUnitResults) = ListPair.unzip (map transDef recbinds)
    val (e, cty) = transCmp (bodytyenv, kindenv) e
    val funkind = if funkind=LocalFun andalso List.exists Gen.id doUnitResults
                  then KnownFun else funkind
  in
    (LetFun(map (fn (x,k) => (x,transKind k)) tyvars, 
      funkind, RecFun recbinds, e), cty)
  end

(*......................................................................*)
(* Non-recursive functions.                                             *)
(*......................................................................*)
| LetFun(tyvars, funkind, Fun (f, (xs,e1)), e2) =>
  let
    (* First translate the body of the function *)
    val defnkindenv = Var.extend (kindenv, tyvars)
    val (e1, cty) = 
      transCmp (Var.extend(tyenv, xs), defnkindenv) e1

    val (eff, restys) = MILTy.fromCmp cty

    val (xs', e1) = foldr (fn ((x,ty), (xs,e)) =>
      if isUnit defnkindenv ty andalso enabled unitArg
      then (xs, LetVal(x, Tuple [], e))
      else ((x,FlattenTypes.transType defnkindenv ty)::xs, e)) ([], e1) xs

    val doUnitResult =
      List.exists (isUnit defnkindenv) restys andalso enabled unitResult
    val e1 = 
      if doUnitResult
      then 
      let
        val (resxs, resvs) = foldr (fn (ty, (xs,resvs)) =>
          if isUnit defnkindenv ty then ((Var.dummy,MILTy.prod [])::xs, 
            resvs)
          else let val x = Census.freshVar 1 
               in ((x,FlattenTypes.transType defnkindenv ty)::xs, 
                   Var x :: resvs) end) ([],[]) restys
      in
        Let(e1, (resxs, Triv resvs))
      end
      else e1

    val bodytyenv = Var.Map.insert(tyenv, f, MILTy.forall(tyvars, 
      MILTy.arrow(map #2 xs, cty)))

    val (e2, cty) = transCmp (bodytyenv, kindenv) e2
  in
    (LetFun(map (fn (x,k) => (x,transKind k)) tyvars, 
       if doUnitResult andalso funkind = LocalFun then KnownFun
                    else funkind, Fun (f, (xs',e1)), e2), cty)
  end        


end

val _ = Counters.reset ()
val (e,cty) = transCmp (tyenv, Var.Map.empty) e

in
  Counters.printCounts (); e
end

end (* of local open *)
end (* of struct *)


