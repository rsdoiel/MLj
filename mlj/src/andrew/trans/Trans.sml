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
(* Translation to MIL.							*)
(* The target of the translation is the `flat value' subset of MIL in   *)
(* which values appear only in val; elsewhere, they must be atoms.      *)
(*======================================================================*)
structure Trans :> TRANS =
struct

local 
  open MILTerm TransType TransOps
in

structure Map = Symbol.OrdMap
structure Set = Symbol.OrdSet

(*----------------------------------------------------------------------*)
(* Merge two environments, the second overriding the first.		*)
(*----------------------------------------------------------------------*)
fun merge (E1,E2)  = Map.unionWith #2 (E1,E2)
fun mergeEE (EE1, EE2) = SMLTy.ExMap.unionWith #2 (EE1, EE2)

(*----------------------------------------------------------------------*)
(* Evaluation of several expressions: optimise the Triv case.		*)
(*----------------------------------------------------------------------*)
fun transEval tr es f =
  let
    fun t typedvs [] = f (rev typedvs)
      | t typedvs (e::es) = 
        let
          val (ce, cty) = tr e
          val (eff, [ty]) = MILTy.fromCmp cty
          fun default () =
          let
            val v = freshVar ()
            val (ce2, cty2) = t ((Var v, ty) :: typedvs) es
          in
            case ce of 
              Triv [ve] => (LetVal(v, ve, ce2), cty2)
            | _ => (Let(ce, ([(v,ty)], ce2)), MILTy.unionCmpTypes(cty,cty2))
          end
        in
          case ce of
            Triv [ve as Var v] =>
            t ((ve, ty)::typedvs) es

          | Triv [ve as SCon c] => 
            t ((ve, ty)::typedvs) es
        
          | _ => default ()
        end
  in
    t [] es
  end

(*======================================================================*)
(* Main entry point!							*)
(*======================================================================*)
fun trans { SE, EE, entity, strexp, tynameTys = TNE, supply } = 
let

fun freshPolyVars (VE, [], tyvars) = 
    (VE, [])

  | freshPolyVars (VE, (v,ty)::vs, tyvars) = 
    let
      val v' = freshVar ()
      val (VE, vs') = freshPolyVars (VE, vs, tyvars)
    in
      (Map.insert(VE, v, (v', MILTy.forall(tyvars, ty), 
        map (MILTy.tyvar o #1) tyvars)), v'::vs')
    end

(*----------------------------------------------------------------------*)
(* Translate an SML longstrid into a series of projections.	        *)
(* Return a value expression and a list of value bindings.              *)
(*----------------------------------------------------------------------*)
fun transLongStrid SE (strid, rest) =
  case Map.find(SE, strid) of

    (* It must be a Java package/class as structure, so ignore *)
    NONE => 
    (Tuple [], MILTy.prod [], [])
    
  | SOME (milvar, milty) =>
    let
      fun trans (v,milty) [] bindings = (v, milty, rev bindings)
        | trans (v,milty) ((id,i)::rest) bindings =
          case MILTy.fromProd milty of
            NONE =>
            Debug.fail "Trans.transLongStrid: expected product type"

          | SOME vtys =>          
            let
              val x = freshVar ()
            in
              trans (Var x, List.nth(vtys, i)) rest ((x, Proj(i, v))::bindings)
            end
    in
      trans (Var milvar, milty) rest []
    end

(*----------------------------------------------------------------------*)
(* Translate an SML longid into a series of projections.	        *)
(* Return a value expression and a list of value bindings.              *)
(*----------------------------------------------------------------------*)
fun transLongid extra SE VE (id, []) =
    (case Map.find(VE, id) of
      SOME (milvar, milty, tys') =>
      if null tys' orelse not extra
      then
      (
        Var milvar,
        milty,
        []
      )
      else
      (
        MILTermOps.tapp(Var milvar, tys'),
        MILTy.app(MILTy.abs (valOf (MILTy.fromForall milty)), tys'),
        []
      )    

    | NONE => 
      Debug.fail ("Trans.transLongid: missing identifier " ^ 
        Pretty.idToString id)
    )
  
  | transLongid extra SE VE (strid, rest) = transLongStrid SE (strid, rest)



(*----------------------------------------------------------------------*)
(* Translate an ML `valuable' expression into a MIL value term.         *)
(* Inputs:                                                              *)
(*    SE: map from SML structure identifiers to MIL variables with      *)
(*        tuple types;                                                  *)
(*    TVE:map from SML type variables to MIL types;                     *)
(*    EE: map from SML generative exceptions to MIL variables holding   *)
(*        the dynamically-generated exception tag;                      *)
(*    VE: map from SML value identifiers to MIL variables and types;    *)
(*    funtyvars:                                                        *)
(*    e:  the SML valuable expression that is to be translated.         *)
(* Outputs:                                                             *)
(*    (ve, ty, funbinds, binds)                                         *)
(* where                                                                *)
(*    ve: MIL value term                                                *)
(*    ty: MIL value type                                                *)
(*    funbinds: function bindings                                       *)
(*    binds: value bindings                                             *)
(*----------------------------------------------------------------------*)
fun transVal
  (SE : (Var.Var * MILTy.Type) Map.map)       
  (TVE : MILTy.Type TyVar.Map.map)               
  (EE : TransOps.ExEnv)
  (VE : TransOps.ValEnv)
  (funtyvars)
  (e : SMLTerm.Exp) =
let


(*......................................................................*)
(* Given a typed abstraction and result type, construct the appropriate *)
(* transVal result as a function binding.                               *)
(*......................................................................*)
fun makeFun (tabs : MILTerm.TAbstr as (typedvars,_), cty) = 
  let
    val f = freshVar ()
  in
    (
      if null funtyvars 
      then Var f
      else TApp(Var f, map (MILTy.tyvar o #1) funtyvars), 
      MILTy.arrow (map #2 typedvars, cty),
      [(f, tabs)], 
      []
    )
  end

in
case e of

(*----------------------------------------------------------------------*)
(* Convert a resolved constant, possibly reporting an overflow error.   *)
(*----------------------------------------------------------------------*)
  SMLTerm.SCon(scon, ty, loc) => 
  let 
    val ty' = transType TVE TNE ty
    val v = 
    case TransSCon.trans (scon, ty') of
      NONE => 
      (addError(Error.error(loc, "constant too large")); Tuple [])

    | SOME jcon => 
      SCon (ty', jcon)
  in
    (v, ty', [], [])
  end

(*----------------------------------------------------------------------*)
(* Java inlined constant.						*)
(*----------------------------------------------------------------------*)
| SMLTerm.JCon jcon =>
  let 
    val Types.F(0, bty) = Constants.typeOf jcon
    val ty = MILTy.java bty
  in
    (SCon(ty, jcon), ty, [], [])
  end

(*----------------------------------------------------------------------*)
(* Non-overloaded longid                                                *)
(*----------------------------------------------------------------------*)
| SMLTerm.Var (longid, tys) =>
  let
    val (v, ty, bindings) = transLongid (null tys) SE VE longid
  in
    case MILTy.fromForall ty of

  (*..................................................................*)
  (* Polymorphic value						      *)
  (*..................................................................*)
      SOME a => 
      let
        val miltys = map (transType TVE TNE) tys
      in
        (TApp(v, miltys), MILTy.app (MILTy.abs a, miltys), [], bindings)
      end

  (*..................................................................*)
  (* Monomorphic value				                      *)
  (*..................................................................*)
    | NONE => 
      (v, ty, [], bindings)
  end

(*----------------------------------------------------------------------*)
(* Overloaded longid                                                    *)
(*----------------------------------------------------------------------*)
| SMLTerm.OverloadedVar (longid, tynameset, tys) =>
  let
    val (v, ty, bindings) = transLongid (null tys) SE VE longid
(*
    val _ = print (MILPretty.valToString v ^ " : " ^ MILTy.toString ty ^ " ")
    val _ = print ("{" ^ Pretty.simpleVec "," SMLTy.toString tys ^ "}\n")
*)
  in
    case tys of
(*
      [] => 
      (v, ty, [], bindings)

    | *) firstty::_ =>

      let
        (* We're expecting the MIL type to be a product *)
        val miltys = 
          case MILTy.fromProd ty of
            SOME tys => 
            tys

          | NONE =>
            Debug.fail ("Trans.transVal: expected product type for \
              \overloaded identifier: " ^ Pretty.longidToString 
              (map #1 (#2 longid)) ^ ":" ^ MILTy.toString ty)

        (* The type might be an internal tyvar. If so, default it. *)
        val tynameopt = 
          case SMLTy.fromConsType firstty of
            SOME ([], tyname) => 
            SOME tyname

          | _ => 
            case SMLTy.fromTyVar firstty of
              SOME tyvar =>
              (case TyVar.sort tyvar of
                 TyVar.Overloaded tynames => NONE (* TyName.default tynames *)
               | _ => Debug.fail "Trans.transVal: bad overloading type")
            | _ => Debug.fail "Trans.transVal: bad overloading type"

        fun find (i, tyname, []) = 
            Debug.fail("Trans.transVal: overloading not implemented at type "
              ^ TyName.toString tyname ^ " for identifier " ^ 
              Pretty.longidToString (map #1 (#2 longid)))

          | find (i, tyname, tyname'::tynames) =
            if TyName.eq(tyname,tyname') 
            then 
            (
              Proj(i, v), 
              List.nth(miltys, i),
              [],
              bindings
            )
            else find (i+1, tyname, tynames)
      in
        case tynameopt of
          SOME tyname => 
          find (0, tyname, TyName.Set.listItems tynameset)

        | NONE => 
          (
            v,
            ty,
            [],
            bindings
          )
      end
  end
  
(*----------------------------------------------------------------------*)
(* Datatype constructor							*)
(*----------------------------------------------------------------------*)
| SMLTerm.Con (con, (isrec, (tyvars, tyname, CE)), tys) => 
  let 
    val ty = SMLTy.consType(tys, tyname)
    val recty = transType TVE TNE ty
    val S = SMLTy.appSubst (ListPair.zip(tyvars, tys))
    val CE = Map.map (Option.map S) CE
    val (k, tyopt) = SMLTy.findCon(tyname, CE, con)
    val sumty = transCE TVE TNE CE 
    val singlenullary = case Map.listItems CE of [NONE] => true | _ => false
    val unaryrep = case Map.listItems CE of [SOME _] => true | _ => false
    fun makeTerm args =
      let 
        val ve = 
          if unaryrep
          then hd args 
          else Inj(sumty, k, args)
      in
        if isrec then Fold(ve, recty)
        else ve
      end
  in
    if singlenullary 
    then 
      (Tuple [], recty, [], [])
    else
    case tyopt of
      NONE => 
      (makeTerm [], recty, [], [])

    | SOME ty' => 
      let
        val v = freshVar ()
        val milty = transType TVE TNE ty'
      in
        makeFun (([(v, milty)], Triv [makeTerm [Var v]]), 
          MILTy.cmp(Effect.none, [recty]))
      end
  end

(*----------------------------------------------------------------------*)
(* Fully applied datatype constructor					*)
(*----------------------------------------------------------------------*)
| SMLTerm.App(SMLTerm.Con (con, (isrec, (tyvars, tyname, CE)), tys), e) => 
  let 
    val ty = SMLTy.consType(tys, tyname)
    val recty = transType TVE TNE ty
    val S = SMLTy.appSubst (ListPair.zip(tyvars, tys))
    val CE = Map.map (Option.map S) CE
    val (k, tyopt) = SMLTy.findCon(tyname, CE, con)
    val sumty = transCE TVE TNE CE 
    val unaryrep = case Map.listItems CE of [SOME _] => true | _ => false
    fun makeTerm arg =
      let 
        val ve = 
          if unaryrep
          then arg
          else Inj(sumty, k, [arg])
      in
        if isrec then Fold(ve, recty)
        else ve
      end
  in
    case tyopt of
      NONE => 
      Debug.fail "Trans.transVal: tried to apply nullary con"

    | SOME ty' => 
      let
        val (ve, _, funbinds, binds) = transVal SE TVE EE VE funtyvars e
      in
        (makeTerm ve, recty, funbinds, binds)
      end
  end

(*----------------------------------------------------------------------*)
(* Exception constructor						*)
(*----------------------------------------------------------------------*)
| SMLTerm.ExCon(exname, ty) =>
  let
    val SOME (varopt, milexty) = SMLTy.ExMap.find(EE, exname)
    val extra = map Var (Gen.optToList varopt)
  in
  case ty of 
    SOME ty => 
    let
      val v = freshVar ()
      val milty = transType TVE TNE ty
    in
      makeFun (([(v,milty)], Triv [ExCon(milexty, extra @ [Var v])]),
        MILTy.cmp(Effect.none, [MILTys.topExn]))
    end

  | NONE =>
    (ExCon(milexty, extra), MILTys.topExn, [], [])
  end

(*----------------------------------------------------------------------*)
(* Fully-applied exception constructor					*)
(*----------------------------------------------------------------------*)
| SMLTerm.App(SMLTerm.ExCon(exname, ty), e) =>
  let
    val SOME (varopt, milexty) = SMLTy.ExMap.find(EE, exname)
    val extra = map Var (Gen.optToList varopt)
  in
  case ty of 
    SOME ty => 
    let
      val (ve, _, funbinds, binds) = transVal SE TVE EE VE funtyvars e
      val v = freshVar ()
      val milty = transType TVE TNE ty
    in
      case MILTy.fromProd milty of
        SOME tys =>
        let
          val vars = map (fn _ => freshVar ()) tys
        in
          (ExCon(milexty, extra @ map Var vars), MILTys.topExn, funbinds,
          binds @ ((v, ve) :: 
            Gen.mapi (fn (i, v') => (v', Proj(i, Var v))) vars))
        end

      | NONE =>
        (ExCon(milexty, extra @ [ve]), MILTys.topExn, funbinds, binds)
    end

  | NONE =>
    Debug.fail "Trans.transVal: tried to apply nullary excon"
  end

(*----------------------------------------------------------------------*)
(* Pattern matching lambda abstraction: use pattern compiler.		*)
(*----------------------------------------------------------------------*)
| SMLTerm.Fn (ty, match) =>
  let
    val (tabs, cty) = 
      Pat.transFn 
      { entity = entity, transExp = transExp SE TVE EE, 
        transType = transType TVE TNE, VE = VE, EE = EE, smlty = ty, 
        match = match
      }
  in
    makeFun (tabs, cty)
  end

(*----------------------------------------------------------------------*)
(* Record construction (values only)					*)
(*----------------------------------------------------------------------*)
| SMLTerm.Record fields =>
  let 
    fun evalfld ([], vmap, funbinds, binds) =         
        let
          val vlist = Map.listItems vmap
          val (vs, tys) = ListPair.unzip vlist
        in
          (Tuple vs, MILTy.prod tys, funbinds, binds)
        end

      | evalfld ((label, e) :: rest, vmap, funbinds, binds) =
        let
          val (ve, ty, funbinds', binds') = transVal SE TVE EE VE funtyvars e
        in
          evalfld (rest, Map.insert(vmap, label, (ve,ty)), 
          funbinds @ funbinds', binds @ binds')
        end
  in
    evalfld (fields, Map.empty, [], [])
  end

| _ =>
  Debug.fail "Trans.transVal: non-valuable expression"
end

(*----------------------------------------------------------------------*)
(* Translate an ML typed expression e into a MIL computation term.      *)
(*----------------------------------------------------------------------*)
and transExp
  (SE : (Var.Var*MILTy.Type) Map.map)         
  (TVE : MILTy.Type TyVar.Map.map)               
  (EE : TransOps.ExEnv)
  (VE : TransOps.ValEnv)
  (e : SMLTerm.Exp) =
let
  val transEval = transEval (transExp SE TVE EE VE)
in
case e of

(*----------------------------------------------------------------------*)
(* Application: evaluate function expression, evaluate argument, apply	*)
(*----------------------------------------------------------------------*)
  SMLTerm.App (e1, e2) =>
  transEval [e1,e2] (fn [(ve1,funty),(ve2,_)] => 
  case MILTy.fromArrow funty of 
    NONE =>
    Debug.fail ("Trans.transExp: expected arrow type: " ^ MILTy.toString funty
    ^ " in " ^ SMLTermOps.expToString e)
  | SOME (_,tys) => 
    (App(ve1, [ve2]), tys))

(*----------------------------------------------------------------------*)
(* Special form for comparisons. This is a hack.        		*)
(*----------------------------------------------------------------------*)
| SMLTerm.Java((Java.Test t, _, _), [e1,e2], tyopt, effect) =>
  transEval [e1,e2] (fn [(ve1,_),(ve2,_)] =>
  (Cond(JavaTest t, ve1, ve2, 
    Triv [MILTermOps.trueVal], Triv [MILTermOps.falseVal]),
    MILTy.cmp(effect, [MILTys.bool])))

(*----------------------------------------------------------------------*)
(* Special form for no-op casts.                                        *)
(*----------------------------------------------------------------------*)
| SMLTerm.Java(j as (Java.NopCast, NONE, NONE), [e], SOME ty, effect) =>
  let
    val ty = transType TVE TNE ty
  in
    transEval [e] (fn [(ve,_)] =>  
    (Triv [Coerce(ve, ty)], MILTy.cmp(effect, [ty])))
  end

(*----------------------------------------------------------------------*)
(* Special form for purity assertions.                                  *)
(*----------------------------------------------------------------------*)
| SMLTerm.Java(j as (Java.Pure, NONE, NONE), [e], SOME ty, effect) =>
  let
    val (ce, cty) = transExp SE TVE EE VE e
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap ce, MILTy.cmp(Effect.allocs, tys))
  end

(*----------------------------------------------------------------------*)
(* Special form for synchronize constructs.                             *)
(*----------------------------------------------------------------------*)
| SMLTerm.Java(j as (Java.Synchronize, NONE, NONE), [e1,e2],SOME ty,effect) =>
  transEval [e1] (fn [(ve,_)] =>
  let
    val (ce, cty) = transExp SE TVE EE VE e2
    val (eff,tys) = MILTy.fromCmp cty
    val v = freshVar ()
    val exnv = freshVar ()
    val handler =
      ([(exnv, MILTys.topExn)], 
        Let(Java((Java.MonitorExit,NONE,NONE), [ve], MILTy.cmp(effect,[])),
        ([], Throw(Var exnv, tys, (entity, {left=0,right=0})))))
  in
    (Let(Java((Java.MonitorEnter,NONE,NONE), [ve], 
    MILTy.cmp(effect,[])), ([], 
    TryLet(ce, [handler],
      ([(v,hd tys)],
      Let(Java((Java.MonitorExit,NONE,NONE), [ve], 
      MILTy.cmp(effect,[])), ([],
      Triv [Var v])))))), MILTy.cmp(effect, tys))
  end)

(*----------------------------------------------------------------------*)
(* Java application: evaluate arguments, apply				*)
(*----------------------------------------------------------------------*)
| SMLTerm.Java(j as (jop,tyopt,nameopt), es, tyopt', effect) =>
  let 
    val j = (jop, Option.map (transType TVE TNE) tyopt, nameopt)
  in
    transEval es (fn typedvs =>
    case tyopt' of
      NONE =>
      let
        val cty = MILTy.cmp(effect, [])
      in
        (Let(Java(j, map #1 typedvs, cty), ([],Triv [Tuple []])), 
          MILTy.cmp(effect, [MILTy.prod []]))
      end

    | SOME ty =>
      let
        val cty = MILTy.cmp(effect, [transType TVE TNE ty])
      in
        (Java(j, map #1 typedvs, cty), cty)
      end
    )
  end

(*----------------------------------------------------------------------*)
(* Let binding								*)
(*----------------------------------------------------------------------*)
| SMLTerm.Let(dec, exp) =>
  let
    val (VE', EE', SE', Cx) = transDec SE TVE EE VE dec
    val (ce, cty) = 
      transExp (merge(SE,SE')) TVE (mergeEE(EE,EE')) (merge(VE,VE')) exp
  in
    Cx (ce, cty)
  end

(*----------------------------------------------------------------------*)
(* Exception handler							*)
(*----------------------------------------------------------------------*)
| SMLTerm.Handle (exp,match) =>
  Pat.transHandle 
  { entity = entity, transExp = transExp SE TVE EE, 
    transType = transType TVE TNE, VE = VE, EE = EE, exp = exp, 
    match = match 
  }

(*----------------------------------------------------------------------*)
(* Raising an exception							*)
(*----------------------------------------------------------------------*)
| SMLTerm.Raise (e, ty, loc) =>
  let
    val ty = transType TVE TNE ty
  in
    transEval [e] (fn [(ve,_)] => 
    (Throw(ve, [ty], (entity, loc)), MILTy.cmp(Effect.throws, [ty])))
  end

(*----------------------------------------------------------------------*)
(* Record construction							*)
(* Field expressions are evaluated from left to right			*)
(*----------------------------------------------------------------------*)
| SMLTerm.Record fields =>
  let 
    fun evalfld [] vemap = 
        let
          val vlist = Map.listItems vemap
          val (vs, tys) = ListPair.unzip vlist
        in
          (Triv [Tuple vs], MILTy.cmp(Effect.none, [MILTy.prod tys]))
        end

      | evalfld ((label, e) :: rest) vmap =
        transEval [e] (fn [(ve,ty)] =>
        evalfld rest (Map.insert(vmap, label, (ve,ty))))
  in
    evalfld fields Map.empty
  end

| _ =>
  let
    val (ve, ty, funbinds, binds) = transVal SE TVE EE VE [] e
  in
    (foldr (fn (fundef, e) => LetFun([], AnyFun, Fun fundef, e))
    (foldr (fn ((v, ve), e) => LetVal(v, ve, e))
       (Triv [ve]) binds) funbinds, MILTy.cmp(Effect.none, [ty]))
  end
end

(*----------------------------------------------------------------------*)
(* Translate an ML declaration item into a MIL computation context.     *)
(*----------------------------------------------------------------------*)
and transDecItem SE TVE EE VE decitem =
case decitem of

(*......................................................................*)
(* Special form for rebinding overloaded identifiers			*)
(*......................................................................*)
  SMLTerm.Val(loc, [], _, 
    SMLTerm.PatVar(x,_), SMLTerm.OverloadedVar(longid, _, [])) =>
  let
    val (Var milvar, ty, bindings) = transLongid false SE VE longid
    val VE = Map.insert(VE, x, (milvar, ty, []))
  in
    (VE, EE, Map.empty, 
      fn (rhs,rhscty) => 
        (foldr (fn ((x, v), e) => LetVal(x, v, e)) rhs bindings, rhscty))
  end        
 
(*......................................................................*)
(* Pattern val binding of the form					*)
(*    val pat:ty1 = e1                                                  *)
(* If e1 is not valuable (according to the Defn) or pat is refutable    *)
(* (this is our own restriction), then no generalisation is performed.  *)
(*......................................................................*)
| SMLTerm.Val(loc, tyvars, ty, pat, e) =>
  let 
    (* List the free variables of the pattern *)
    val fvtys = SMLTermOps.fvPat pat

    (* Generate MIL variables for each of them; they must be in alphabetical
       order, hence foldri not foldli *)
    val (VE', typedfvs) = 
      Map.foldri (fn (v,ty,(VE, typedfvs)) =>
        let
          val milty = transType TVE TNE ty
          val milvar = freshVar ()
        in
          (Map.insert(VE, v, (milvar, milty, [])), (milvar,milty)::typedfvs)
        end) (Map.empty, []) fvtys

    val miltys = map #2 typedfvs

    val v1 = freshVar ()
  in
    (* No type variables, so rhs and lhs can be non-valuable *)
    if null tyvars 
    then 
    let
      (* If none of the patterns match, throw a Bind exception *)
      val v = freshVar ()
      val failterm = 
        (MILTerm.LetVal(v, MILTerm.ExCon(MILTy.exn(TransExn.bindExn,[]),[]),
          MILTerm.Throw (MILTerm.Var v, miltys, (entity, loc))),
        MILTy.cmp(Effect.throws, miltys))

      val (patterm, patcty) = 
        Pat.transLetPat { transExp = transExp SE TVE EE, 
          transType = transType TVE TNE, VE = VE, EE = EE,
          var = v1, smlty = ty, fail = failterm, pat = pat, loc = loc }
      val (ce, cty) = transExp SE TVE EE VE e
      val (_,[ty]) = MILTy.fromCmp cty
    in
      (
        VE',
        EE,
        Map.empty, 
        fn (rhs,rhscty) => 
        (Let(ce, ([(v1,ty)], 
           Let(patterm, (typedfvs, rhs)))), 
             MILTy.unionCmpTypes(MILTy.unionCmpTypes(cty, patcty),rhscty))
      )
    end

    (* Otherwise we can generalise the types *)
    else 
    let
      val { bindings = binds2, VE = VE', TVE = TVE', tyvars = tyvars' } = 
        ValPat.trans { TVE = TVE, TNE = TNE, tyvars = tyvars, pat = pat,
                       var = v1, smlty = ty }

      val (ve, ty, funbinds, binds1) = transVal SE TVE' EE VE tyvars' e
    in
      (
        merge (VE,VE'),
        EE,
        Map.empty,
        fn (rhs, cty) => 
          (foldr (fn (fundef, e) => LetFun(tyvars', AnyFun, Fun fundef, e))
            (foldr (fn ((x,v),e) => LetVal(x,v,e)) rhs 
              (binds1 @ (v1, 
                MILTermOps.tabs(tyvars', ve)) :: binds2)) funbinds, cty)
      )
    end
  end

(*......................................................................*)
(* SML (potentially-) mutually-recursive letrec construct.              *)
(* We do a dependency analysis here, using fixpoints only where they    *)
(* are required.                                                        *)
(*......................................................................*)
| SMLTerm.ValRec (tyvars, recbinds) =>
  let 
    val funs = NList.foldl (fn ((f,_,_),s) => Set.add(s,f)) Set.empty recbinds
    fun findDeps (f,e,ty) =
      let val uses = Set.intersection(SMLTermOps.fv e, funs)
      in ((f,e,ty), Set.listItems uses) end

    val defs = NList.map findDeps recbinds

    val sccs = rev (Dep.scc (NList.toList defs, fn ((f,_,_),f') => 
      Symbol.equal(f,f')))

    val (localTVE, miltyvars) = freshTyVars (TVE, tyvars)
    val (VE', Cx) = transValRec (SE, TVE, EE, localTVE, VE, miltyvars, sccs)
  in   
    (VE', EE, Map.empty, Cx)
  end

(*
(*......................................................................*)
(* Exception definition.						*)
(*......................................................................*)
| SMLTerm.Exception exname =>
  let
    val v = freshVar ()
    val SOME (NONE, milexty) = SMLTy.ExMap.find(EE, exname)
  in
  (
    Map.empty,
    SMLTy.ExMap.insert(EE, exname, (SOME v, milexty)),
    Map.empty,    
    fn (ce,cty) => 
      (Let(New exname, ([(v, MILTy.java Types.INT)], ce)), 
      MILTy.cmpTypePlus (cty, Effect.allocs))
  )
  end
*)

(*......................................................................*)
(* Local declaration							*)
(*......................................................................*)
| SMLTerm.Local(d1, d2) =>
  let
    val (VE1, EE1, SE1, Cx1) = transDec SE TVE EE VE d1
    val (VE2, EE2, SE2, Cx2) = 
      transDec (merge(SE,SE1)) TVE (mergeEE(EE,EE1)) (merge(VE,VE1)) d2
  in
    (VE2, EE2, SE2, Cx1 o Cx2)
  end

(*......................................................................*)
(* Structure binding							*)
(*......................................................................*)
| SMLTerm.Structure(strid, strexp) =>
  let
    val (ce1, cty1) = transStrExp SE VE strexp
    val v = freshVar ()
    val (_, [ty]) = MILTy.fromCmp cty1
  in
    (VE, SMLTy.ExMap.empty, Map.insert(SE, strid, (v,ty)), 
    fn (ce2, cty2) => 
      (Let(ce1, ([(v,ty)], ce2)), MILTy.unionCmpTypes(cty1,cty2))
    )
  end

(*......................................................................*)
(* Java class type definition						*)
(*......................................................................*)
| SMLTerm.ClassType (classname, classinfo, fields, methods) =>
  let
    fun transMethod ((name, mods, argtys, resultty), argvars, body) =
      let 
        val argtys = map (transType TVE TNE) argtys
        val resultty = Option.map (transType TVE TNE) resultty
        val argvars = map (fn (NONE,_) => NONE
                            | (SOME x,ty) => SOME (x,ty)) 
                      (ListPair.zip (argvars, argtys))

        val (VE', vs) =
        if List.exists (fn Method.STATIC => true | _ => false) mods
        then freshOptVars (VE, argvars)
        else 
        let
          val argvars = SOME (Ids.symbol "_this", MILTy.tyname classname) 
            :: argvars
          val (VE', vs) = freshOptVars (VE, argvars)
        in
          (Map.insert(VE', Ids.symbol "this", valOf (Map.find(VE', 
            Ids.symbol "_this"))), vs)
        end

        val ceopt = Option.map (transExp SE TVE EE VE') body
        val v = freshVar ()
      in
        (name, mods, argtys, resultty, 
          case (ceopt, resultty) of
            (SOME (ce,_), NONE) => 
            SOME (freshVar (), (vs, Let(ce, ([(v,MILTy.prod [])], Triv []))))

          | (SOME (ce,_), SOME _) => 
            SOME (freshVar (), (vs, ce))

          | _ => NONE)
      end

    fun transFields ([], result) = result
      | transFields (((name, mods, ty, _), expopt)::fields, 
         (resultfields, e, eff)) = 
        let
          val ty = transType TVE TNE ty
          val resultfields' = (name, mods, ty, NONE) :: resultfields
        in
          case expopt of
            NONE =>
            transFields (fields, (resultfields', e, eff))

          | SOME exp =>
            let
              val (e', cty') = transExp SE TVE EE VE exp
              val v = freshVar ()
              val (eff', _) = MILTy.fromCmp cty'
            in
              transFields (fields, (resultfields', 
                Let(e', ([(v,ty)], 
                  Let(Java((Java.PutField, SOME (MILTy.tyname classname),
                    SOME name), [Var v], MILTy.cmp(Effect.writes, [])),
                  ([], e)))),
                  Effect.union(eff, eff')))
            end
        end

    val (flags, super, ints) = classinfo
    val classinfo' = (flags, Option.map (transType TVE TNE) super, 
      map (transType TVE TNE) ints)
    val methods' = map transMethod methods
    val (fields', e, eff) = transFields (fields, ([], Triv [], Effect.none))
  in        
    (VE, SMLTy.ExMap.empty, SE, 
      fn (ce2, cty) => 
      (LetClass(MILTy.tyname classname, classinfo', fields', 
        case e of 
          Triv [] => methods'
        | _ => 
          (JavaString.fromString "<clinit>", 
           [Method.PUBLIC,Method.STATIC],[],NONE,
           SOME(freshVar (), ([], e)))
         :: methods', 
        ce2), MILTy.cmpTypePlus(cty, Effect.io)))
  end

(*----------------------------------------------------------------------*)
(* Translate an ML declaration into a MIL computation context.          *)
(*----------------------------------------------------------------------*)
and transDec SE TVE EE VE dec =
case dec of
  []    => 
  (Map.empty, SMLTy.ExMap.empty, Map.empty, Gen.id)

| d::ds =>
  let
    val (VE1, EE1, SE1, Cx1) = transDecItem SE TVE EE VE d
    val (VE2, EE2, SE2, Cx2) = 
      transDec (merge(SE,SE1)) TVE (mergeEE(EE, EE1)) (merge(VE,VE1)) ds
  in
    (merge(VE1, VE2), mergeEE(EE1, EE2), merge(SE1, SE2), Cx1 o Cx2)
  end

(*----------------------------------------------------------------------*)
(* Translate a structure expression into a computation term.            *)
(*----------------------------------------------------------------------*)
and transStrExp SE VE exp = 
case exp of

  SMLTerm.Struct(vals, strs) =>  
  transEval (transStrExp SE VE) (Map.listItems strs) (fn strs =>
  let
    val (strvs, strtys) = ListPair.unzip strs
    val (valvs, valtys) = 
      ListPair.unzip (map (transStrBind VE) (Map.listItems vals))
  in
  (
    Triv [Tuple(strvs @ valvs)], 
    MILTy.cmp(Effect.none, [MILTy.prod(strtys @ valtys)])
  )
  end)


| SMLTerm.Strid (strid,rest) =>
  let
    val (v, ty, bindings) = transLongStrid SE (strid, rest)
  in
    (foldr (fn ((x,v),e) => LetVal(x,v,e)) (Triv [v]) bindings,
     MILTy.cmp(Effect.none, [ty]))
  end

| SMLTerm.StrLet(strdec, strexp) =>
  let 
    val EE = SMLTy.ExMap.map (fn exname => (NONE,exname)) EE
    val (VE', _, SE', Cx) = 
      transDec SE TyVar.Map.empty EE VE strdec
    val (ce,cty) = transStrExp (merge(SE,SE')) (merge(VE,VE')) strexp
  in
    Cx (ce,cty)
  end

and transStrBind VE id =
  case Map.find(VE, id) of
    SOME (milvar, milty, _) => (Var milvar, milty)
  | _ => Debug.fail "Trans.transStrBind: error"

(*----------------------------------------------------------------------*)
(* Translate a val rec binding.						*)
(* The type variables have already been translated.                     *)
(* This is complicated because we want to generate let's not letrec's   *)
(* where possible.                                                      *)
(*----------------------------------------------------------------------*)
and transValRec (SE, TVE, EE, defnTVE, VE, miltyvars, sccs) =
  case sccs of      
    [] =>
    (Map.empty, Gen.id)

  (*..................................................................*)
  (* A recursive set of bindings.				      *)
  (*..................................................................*)
  | (Dep.Rec scc :: sccs) =>
    let 
      (* The function variables from the SML term *)
      val funvars = map (fn (f,_,ty) => (f, transType defnTVE TNE ty)) 
        (NList.toList scc)

      (* Function variables for the body of the MIL letrec *)
      val (bodyVE, bodyfunvars) = freshPolyVars (Map.empty, funvars, miltyvars)

      (* Function variables for the definitions in the MIL letrec *)
      val (defnVE, defnfunvars) = freshVars (VE, funvars)

      (* Translate the other scc's *)
      val (resultVE, Cx) = transValRec (SE, TVE, EE, 
        defnTVE, merge(VE,bodyVE), miltyvars, sccs)

      val defs = map
        (fn ((bodyfunvar, defnfunvar), (_, SMLTerm.Fn (smlty,match), funty)) =>
        let 
          val ty = transType defnTVE TNE funty
          val (tabs, cty) = 
            Pat.transFn { entity = entity, transExp = transExp SE defnTVE EE, 
              transType = transType defnTVE TNE, VE = defnVE, EE = EE,
              smlty = smlty, match = match }
        in 
          (bodyfunvar, defnfunvar, tabs, cty)
        end) (ListPair.zip(ListPair.zip(bodyfunvars, defnfunvars), 
          NList.toList scc))
    in
      (
        merge(bodyVE, resultVE),
        fn (ce,cty) => 
        let val (ce,cty) = Cx (ce,cty)
        in (LetFun(miltyvars, AnyFun, RecFun defs, ce), cty) end
      )
    end

  (*..................................................................*)
  (* A non-recursive binding.					      *)
  (*..................................................................*)
  | (Dep.NonRec (funvar, SMLTerm.Fn (smlty,match), funty) :: sccs) =>
    let
      (* Variable for the body of the MIL let *)
      val (bodyVE, [bodyfunvar]) = freshPolyVars (Map.empty, [(funvar,
        transType defnTVE TNE funty)], miltyvars)

      (* Translate the other scc's *)
      val (resultVE, Cx) = transValRec (SE, TVE, EE, 
      defnTVE, merge(VE,bodyVE), miltyvars, sccs)

      (* Translate the definition *)
      val (tabs, _) = 
        Pat.transFn { entity = entity,
          transExp = transExp SE defnTVE EE, transType = transType defnTVE TNE,
          VE = VE, EE = EE, smlty = smlty, match = match }
    in
    (
      merge(bodyVE, resultVE),
      fn (ce,cty) => 
      let
        val (ce,cty) = Cx (ce,cty)
      in (LetFun(miltyvars, AnyFun, Fun (bodyfunvar, tabs), ce), cty) end
    )  
    end

val _ = 
  (TransOps.errors := []; TransOps.vs := supply; TransOps.tvs := Var.initial)

val (e,cty) = transStrExp SE Map.empty strexp

in
  { 
    term = e, 
    cty = cty, 
    errors = !TransOps.errors, 
    varsupply = !TransOps.vs, 
    tyvarsupply = !TransOps.tvs
  }
end
    

end (* of local open *)
end (* of struct *)
