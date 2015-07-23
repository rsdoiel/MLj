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
(* Elaboration of the core language					*)
(*======================================================================*)
structure ElabCore :> ELABCORE =
struct

structure T = SMLTerm

local open 
  Syntax ElabOps Env EnvOps SMLTy SMLPrimTy SMLSch SMLSchOps ElabTy ElabPat 
  ValBind
in

structure Map = Symbol.OrdMap

(*----------------------------------------------------------------------*)
(* Fun dec derived form							*)
(*----------------------------------------------------------------------*)
fun translateFValBindItem (fvalbinditem as (loc,f,pat,_,_)::_) =
  let
    val vs = map (fn _ => SMLTy.freshVar ()) pat
  in
    ((loc,PatVar (Short f)), abs vs (loc, 
        Case(expTuple loc (map (fn v => (loc, LongVid(Short v))) vs), 
             map (fn (loc, f, pats, exp as (loc',_), tyopt) => 
               (patTuple loc pats, 
               case tyopt of NONE => exp
                           | SOME ty => (loc',Constraint(exp,ty)))) 
               fvalbinditem)))
  end

fun translateFValBind (loc,(tyvars,fvalbind)) =
  (loc, ValRec (tyvars, map translateFValBindItem fvalbind))

val bogusTerm = T.Record []

fun bogusResult () =
  (bogusTerm, SMLTy.freshType ())

val falseTerm = T.Con(Ids.falseSym, TopEnv.boolCE, [])
val trueTerm  = T.Con(Ids.trueSym, TopEnv.boolCE, [])

fun nilTerm ty = T.Con(Ids.nilSym, TopEnv.listCE, [ty])
fun consTerm ty = T.Con(Ids.consSym, TopEnv.listCE, [ty])

fun clos vs1 vs2 =
  let 
    val tyvars = TyVar.Set.listItems (TyVar.Set.difference (vs2, vs1))
  in
    tyvars
  end

(*----------------------------------------------------------------------*)
(* Detect a valbind that's an overloaded binding.			*)
(*----------------------------------------------------------------------*)
fun fromOverloaded [] = NONE
  | fromOverloaded [_] = NONE
  | fromOverloaded (((_, PatVar (Short id | OpShort id)), exp)::valbind) =
    let
      fun checkrest ([], acc) = SOME (id, exp::rev acc)
        | checkrest (((_, PatVar (Short id' | OpShort id')), exp')::valbind, 
            acc) = 
          if Symbol.equal(id,id') then checkrest (valbind, exp'::acc)
          else NONE
    in
      checkrest (valbind, [])
    end

  | fromOverloaded _ = NONE
    
(*----------------------------------------------------------------------*)
(* Elaborate a longvid							*)
(*----------------------------------------------------------------------*)
fun infVid C (loc,longvid) =
  case EnvLookup.lookupVid' (EofC C, loc, longvid) of
    NONE => 
    (SMLTy.error (Error.error (loc, "unbound variable or constructor: " ^ 
      Pretty.longidToString longvid), []); bogusResult ())
 
  | SOME (vbind, longid) =>
    case vbind of
      VarSch(sch as SMLSch.TypeScheme(tyvars,_)) => 
      let
        val (tys, ty) = instantiate sch
      in
        case map TyVar.sort tyvars of
          [TyVar.Overloaded tynameset] => 
          (T.OverloadedVar (longid, tynameset, tys), ty)

        | _ =>
          (T.Var (longid, tys), ty)
      end

    | ConSch(sch,CE) =>
      let 
        val con = List.last longvid
        val (tys, ty) = instantiate sch
      in
        (T.Con (con, CE, tys), ty)
      end

    | ExTy(ty, exname) => 
      (T.ExCon(exname, Option.map #1 (fromFunType ty)), ty)

    | JavaTys(class, [ty]) =>
      let
        val classty = TransJava.classToML class
        val js = Symbol.toJavaString (List.last longvid)
      in
        (* We allow ref types through but might choke on them later *)
        if isSome (SMLTy.fromRefType ty)
        then (T.Java ((Java.FieldRef, SOME classty, SOME js),
          [], SOME ty, Effect.none), ty)
        else
          if isSome (SMLTy.fromFunType ty)
          then (SMLTy.error (Error.error (loc, 
            "Java methods cannot be used in a first-class way"), []);
            bogusResult ())
            
          else (T.Java ((Java.GetField, SOME classty, SOME js),
              [], SOME ty, Effect.reads), ty)
      end

    | JavaTys(class, _) =>
      (SMLTy.error (Error.error (loc, 
        "overloaded Java members cannot be used in a first-class way"), []);
      bogusResult())

(*----------------------------------------------------------------------*)
(* Elaborate an expression that may be a static Java identifier		*)
(*----------------------------------------------------------------------*)
datatype ExpOrJava = 
  Result of SMLTerm.Exp * SMLTy.Type
| JavaResult of ClassHandle.Handle * JavaString.t

fun infExpOrJava C (exp as (loc,preexp)) =
case preexp of
  LongVid vid =>
  let
    val vid = ElabOps.vidToLongid vid
  in
    case EnvLookup.lookupVid' (EofC C, loc, vid) of
      NONE => 
      (SMLTy.error (Error.error (loc, "unbound variable or constructor: " ^ 
        Pretty.longidToString vid), []);
      Result (bogusResult ()))

    | SOME (JavaTys (class, tys), longid) =>
      JavaResult (class, Symbol.toJavaString (List.last vid))

    | SOME _ =>
      Result (infExp C exp)
  end

| _ =>
  Result (infExp C exp)

(*----------------------------------------------------------------------*)
(* Elaborate expressions (p21 Defn)					*)
(*----------------------------------------------------------------------*)
and infExp (C : Context) (loc,preexp) = 
case preexp of

(* Rule 1 *)
  SCon scon => 
  let
    val ty = ElabSCon.typeSCon scon
  in
    (T.SCon(scon,ty,loc), ty)
  end

(* Rule 2 *)
| LongVid vid =>
  infVid C (loc, ElabOps.vidToLongid vid)

(* Rule 3 *)
| Record exps => 
  let
    val exptyrow = infExpRow C exps
  in
    (T.Record (map (fn (lab,(e,ty)) => (lab,e)) exptyrow), 
       SMLTy.recType (map (fn (lab,(e,ty)) => (lab,ty)) exptyrow))
  end

(* Rule 4 with new side condition *)
| Let(dec, exp) =>
  let
    val stamp = getStamp ()
    val (d, E') = infDec C false dec
    val (e, ty) = infExp (CplusE C E') exp
  in
    if List.all (fn tn => TyName.earlier(tn,stamp))
       (TyName.Set.listItems (SMLTy.tynames ty))
    then ()
    else error (Error.error (loc, 
      "local datatype declaration escapes scope"), []);
    (T.Let(d,e), ty)
  end

(* Rule 5 elided: parentheses *)
(* Rule 6 below *)
(* Rule 7 elided: atexp to exp *)

(*......................................................................*)
(* Hacks for the new syntax						*)
(*......................................................................*)

(* Non-static method invocation with explicit tuple argument *)
| App((_, App(exp, (_, DotHash method))), (_,Tuple exps)) =>
  infExp C (loc, Java(Java.Invoke, NONE, SOME(Symbol.toJavaString method), 
    exp :: exps))

(* Non-static superclass method invocation with explicit tuple argument *)
| App((_, App(exp, (_, DotHashHash method))), (_,Tuple exps)) =>
  infExp C (loc, Java(Java.InvokeSpecial,NONE,SOME(Symbol.toJavaString method),
    exp :: exps))

(* Non-static method invocation with non-tuple argument *)
| App((_, App(exp1, (_, DotHash method))), exp2) =>
  infExp C (loc, Java(Java.Invoke, NONE, SOME(Symbol.toJavaString method), 
    [exp1, exp2]))

(* Non-static superclass method invocation with non-tuple argument *)
| App((_, App(exp1, (_, DotHashHash method))), exp2) =>
  infExp C (loc, Java(Java.InvokeSpecial,NONE,SOME(Symbol.toJavaString method),
    [exp1, exp2]))

(* Field access *)
| App(exp, (_, DotHash field)) =>
  infExp C (loc, Java(Java.FieldRef, NONE, SOME(Symbol.toJavaString field),
    [exp]))

(* All other uses are illegal *)
| DotHash _ =>
  (error (Error.error (loc, "illegal use of .#"), []);
    (bogusTerm, SMLTy.freshType ()))

(* All other uses are illegal *)
| DotHashHash _ =>
  (error (Error.error (loc, "illegal use of .##"), []);
    (bogusTerm, SMLTy.freshType ()))

(* Rule 8a: Java operation *)
| Java(jop, typopt, idopt, exps) =>
  let
    val pairs = map (infExp C) exps
    val tyopt = Option.map (infTy C) typopt
    val CE = getClassEnv ()
  in
    ElabJava.elab (C,CE)
      (loc, (jop, tyopt, idopt), map (fn ((x,_),(y,z)) => (x,y,z)) 
      (ListPair.zip(exps, pairs)), 
       case exps of ((loc,Java(Java.Super,_,_,_))::_) => true | _ => false)
      handle SMLClassDefOps.ClassNotFound name =>
      (error (Error.error (loc, "class not found: " ^ name), []);
       (bogusTerm, SMLTy.freshType ()))
  end

(* Rule 8 + explicit static method invocation *)
| App(exp1 as (loc1,_), exp2 as (loc2, _)) =>
  (case infExpOrJava C exp1 of
    Result (e1, ty1) =>
    let
      val (e2, ty2) = infExp C exp2
    in
      case fromFunType ty1 of
        SOME (ty1, ty3) =>
        (unify ((SOME loc2, "argument type", ty2), 
                  (SOME loc1, "expected", ty1));
        (T.App(e1, e2), ty3))

      | NONE =>
        let
          val tyvar = SMLTy.freshType ()
        in
          unify ((SOME loc1, "function expression", ty1), 
                  (SOME loc2, "expected", SMLTy.funType (ty2, tyvar)));
          (T.App(e1, e2), tyvar)
        end
    end

  (*..................................................................*)
  (* Horrid hack to get new static method invocation to go through    *)
  (*..................................................................*)

  | JavaResult (class, name) =>
    case exp2 of
      (_,Tuple exps) =>
      infExp C (loc, Java(Java.Invoke, 
        SOME (loc, TyClass (ClassHandle.name class)),
        SOME name, exps))

    | _ =>
      infExp C (loc, Java(Java.Invoke, 
        SOME (loc, TyClass (ClassHandle.name class)),
        SOME name, [exp2]))
  )
        
(* Rule 9 *)
| Constraint(exp as (loc1,_), typ as (loc2,_)) =>
  let
    val (e, ty1) = infExp C exp
    val ty2 = infTy C typ
    val ty3 = unify ((SOME loc1, "expression", ty1), 
                 (SOME loc2, "type", ty2))
  in
    (e, ty3)
  end
 
(* Rule 9a: Java casting *)
| ConstraintGt(exp, typ) =>
  infExp C (loc, Java(Java.Cast, SOME typ, NONE, [exp]))
 
(* Rule 10 *)
| Handle(exp as (loc1,_), match) =>
  let
    val (e, ty1) = infExp C exp
    val (mrules, ty2, ty3) =
    infMatch C match 
      (fn ty => unify ((SOME loc1, "handle expression", ty),
                       (NONE, "expected", exnType)))
  in
    unify ((SOME loc1, "handle expression", ty1),
                (NONE, "handlers", ty3));
    (T.Handle(e,(ty3,mrules)), ty3)
  end

(* Rule 11 *)
| Raise (exp as (loc',_)) =>
  let
    val (e, ty) = infExp C exp
    val ty' = SMLTy.freshType ()
  in
    unify ((SOME loc', "expression", ty),
                (NONE, "expected", exnType));
    (T.Raise(e, ty', loc), ty')
  end
 
(* Rule 12 *)
| Fn match => 
  let
    val (mrules, ty1, ty2) = infMatch (lamC C) match Gen.id
  in
    (T.Fn (ty1, (ty2,mrules)), SMLTy.funType(ty1,ty2))
  end

(*......................................................................*)
(* Tuple expression derived form:					*)
(* (exp_1, ..., exp_n) ~~> {1=exp_1, ..., n=exp_n}                      *)
(*......................................................................*)
| Tuple [exp] => 
  Debug.fail "Elab.infExp: singleton tuple"

| Tuple exps =>
  let
    val etys = map (infExp C) exps
    val (es, tys) = ListPair.unzip etys 
  in
    (tupleTerm es, SMLTy.tupleType tys) 
  end

(*......................................................................*)
(* Field selection derived form:					*)
(* #lab ~~> fn {lab=var, . . .} => var                                  *)
(*......................................................................*)
| Hash lab =>
  let
    val v = SMLTy.freshVar ()
    val ty = SMLTy.freshType ()
    val rty = SMLTy.openRecType [(lab,ty)]
  in
    (T.Fn (rty, (ty, 
      [(loc, T.PatRecord(true, [(lab, T.PatVar(v,ty))]), monovar v)])),
      SMLTy.funType (rty, ty))
  end

(*......................................................................*)
(* Case expression derived form:					*)
(* case exp of match ~~> (fn match)(exp)                                *)
(*......................................................................*)
| Case (exp, match) =>
  let
    val (e, ty1) = infExp C exp
    val (mrules, ty2, ty3) = infMatch C match 
      (fn ty => unify ((SOME loc, "expression", ty1), 
                       (NONE, "pattern", ty)))
  in
    (caseTerm(e, ty1, (ty3, mrules)), ty3)
  end

(*......................................................................*)
(* Conditional derived form:						*)
(* if exp_1 then exp_2 else exp_3 ~~>                                   *)
(* case exp_1 of true => exp_2 | false => exp_3                         *)
(*......................................................................*)
| If (exp1 as (loc1,_), exp2 as (loc2,_), exp3 as (loc3,_)) =>
  let
    val (e1, ty1) = infExp C exp1
    val (e2, ty2) = infExp C exp2
    val (e3, ty3) = infExp C exp3
  in
    unify ((SOME loc1, "expression", ty1),
                (NONE, "expected", boolType));
    unify ((SOME loc2, "then branch", ty2),
                (SOME loc3, "else branch", ty3));
    (condTerm (e1, (e2,loc2), (e3,loc3), ty3), ty3)
  end

(*......................................................................*)
(* orelse derived form:							*)
(* exp_1 orelse exp_2 ~~> if exp_1 then true else exp_2                 *)
(*......................................................................*)
| Orelse (exp1 as (loc1,_), exp2 as (loc2,_)) =>
  let
    val (e1, ty1) = infExp C exp1
    val (e2, ty2) = infExp C exp2
  in
    unify ((SOME loc1,"expression",ty1),
                (NONE, "expected", boolType));
    unify ((SOME loc2,"expression",ty2),
                (NONE, "expected", boolType));
    (condTerm (e1, (trueTerm, loc1), (e2, loc2), boolType), boolType)
  end

(*......................................................................*)
(* andalso derived form:						*)
(*......................................................................*)
| Andalso (exp1 as (loc1,_), exp2 as (loc2,_)) =>
  let
    val (e1, ty1) = infExp C exp1
    val (e2, ty2) = infExp C exp2
  in
    unify ((SOME loc1,"expression",ty1),
                (NONE, "expected", boolType));
    unify ((SOME loc2,"expression",ty2),
                (NONE, "expected", boolType));
    (condTerm (e1, (e2,loc1), (falseTerm, loc2), boolType), boolType)
  end

(*......................................................................*)
(* Sequence derived form: (slightly simpler translation than p67 Defn)	*)
(* (exp_1; ... ; exp_n ; exp) ~~>                                       *)
(* let val _ = exp_1 in ...                                             *)
(* let val _ = exp_n in exp end ... end                                 *)   
(*......................................................................*)
| Sequence [] =>
  Debug.fail "Elab.infExp: empty sequence expression"

| Sequence [exp] =>
  infExp C exp

| Sequence (exp::(exps as (loc,_)::_)) =>
  let
    val (e1, ty1) = infExp C exp
    val (e2, ty2) = infExp C (loc, Sequence exps)
    val v = SMLTy.freshVar ()
  in
    (T.Let([T.Val(loc, [], ty1, T.PatWild, e1)], e2), ty2)
  end

(*......................................................................*)
(* while derived form:							*)
(*......................................................................*)
| While (exp1 as (loc1,_), exp2) =>
  let
    val (e1, ty1) = infExp C exp1
    val _ = unify ((SOME loc1, "expression", ty1),(NONE, "expected", boolType))
    val (e2, ty2) = infExp C exp2
  in
    (makeWhileTerm (loc1, e1, e2), ty2)
  end

(*......................................................................*)
(* List expression derived form:					*)
(* [exp_1, ..., exp_n] ~~>                                              *)
(* exp_1 :: ... :: exp_n :: nil                                         *)
(*......................................................................*)
| List es =>
  let
    val (e, ty) = infList C es
  in
    (e, listType ty)
  end

and infList C [] = 
    let
      val ty = SMLTy.freshType ()
    in
      (nilTerm ty, ty)
    end

  | infList C [exp] =
    let
      val (e, ty) = infExp C exp
    in
      (T.App(consTerm ty, tupleTerm [e, nilTerm ty]), ty)
    end

  | infList C ((exp as (loc1,_))::(exps as ((loc2,_)::_))) =
    let
      val (e1, ty1) = infExp C exp
      val (e2, ty2) = infList C exps
      val ty3 = unify ((SOME loc1, "list element", ty1),
                (SOME loc2, "rest of list", ty2))
    in
      (T.App(consTerm ty1, tupleTerm [e1, e2]), ty3)
    end
    
(*----------------------------------------------------------------------*)
(* Expression Rows (p22 Defn)						*)
(*----------------------------------------------------------------------*)
(* Rule 6 *)
and infExpRow C exprow = map (fn (lab,exp) => (lab, infExp C exp)) exprow

(*----------------------------------------------------------------------*)
(* Matches (p22 Defn)   						*)
(*----------------------------------------------------------------------*)
(* Rule 13 *)
and infMatch C [] unif = 
    Debug.fail "ElabCore.infMatch: empty match"

  | infMatch C [(mrule as ((loc,_),_))] unif =
    let
      val (pair, ty1, ty1') = infMRule C mrule unif
    in
      ([pair], ty1, ty1')
    end

  | infMatch C ((mrule as ((loc,_),_))::match) unif =
    let
      val (pair, ty1, ty1') = infMRule C mrule unif
      val (pairs, ty2, ty2') = infMatch C match unif
      val ty3 = unify((SOME loc, "pattern", ty1), (NONE, "pattern",ty2))
      val ty3' = unify((SOME loc, "expression", ty1'),(NONE,"expression",ty2'))
    in
      (pair::pairs, ty3, ty3')
    end

(*----------------------------------------------------------------------*)
(* Match Rules (p23 Defn)                                		*)
(*----------------------------------------------------------------------*)
(* Rule 14 with new side condition *)
and infMRule C (pat as (loc,_),exp) unif = 
  let
    val stamp = getStamp ()
    val (p, VE, ty, valuable) = infPat C pat
    val _ = unif ty
    val (e, ty') = infExp (CplusVE C VE) exp
  in
    if List.all (fn tn => TyName.earlier(tn,stamp)) 
    (TyName.Set.listItems (tynamesVE VE))
    then ()
    else error (Error.error (loc, "local datatype declaration escapes scope"),
      []);
    ((loc, p, e), ty, ty')
  end

(*----------------------------------------------------------------------*)
(* Declarations (p23 Defn)						*)
(* Given a context and a declaration return				*)
(* 1. a typed value declaration                                         *)
(* 2. an environment E which binds variables and type constructors	*)
(* The boolean flag indicates whether this is a `top-level' declaration;*)
(* that is, one in which ungeneralizable type variables should be       *)
(* flagged as errors.                                                   *)
(*----------------------------------------------------------------------*)
and infDecItem C toplevel ((loc,dec) : DecItem) = 
case dec of

(* Rule 15 *)  
  Val({explicit, implicit}, valbind) =>
  (case fromOverloaded valbind of
    NONE =>
    let
      val U = TyVar.Set.union(
        TyVar.Set.addList(TyVar.Set.empty, map TyVar.explicit explicit),
        TyVar.Set.difference (
          TyVar.Set.addList(TyVar.Set.empty, map TyVar.explicit implicit), 
          UofC C))
      val (d, VE) = infValBind U (CplusU C U) toplevel valbind
    in
      (d, VEinE VE)
    end

  | SOME (id, exps) =>
    let
      val pairs = map (infExp C) exps
      val SOME (S, ty') = SMLTy.antiunifylist (map #2 pairs)
    in
      case TyVar.Map.listItemsi S of
        [(tyvar, basetys)] =>
        let
          val (tynameset, exptymap) =
            ListPair.foldl (fn ((exp, ty), basety, (tynameset, exptymap)) =>
            let
              val SOME ([], tyname) = SMLTy.fromConsType basety
            in
              (TyName.Set.add(tynameset, tyname), 
              TyName.Map.insert(exptymap, tyname, (exp,ty)))
            end) (TyName.Set.empty, TyName.Map.empty) (pairs, basetys)

          val (exps,tys) = ListPair.unzip (TyName.Map.listItems exptymap)
          val prodty = SMLTy.tupleType tys
          val prodexp = tupleTerm exps 
          val tyvar' = freshTyVar (TyVar.Overloaded tynameset)
          val ty = SMLTy.appSubst [(tyvar, tyVarType tyvar')] ty'
        in
          ([T.Val(loc, [], prodty, T.PatVar(id, prodty), prodexp)],
          VEinE (Map.insert(Map.empty, id, VarSch(polyType ty))))
        end

      | _ =>
        (error (Error.error (loc, 
          "expected just one type variable in overloaded type"), 
           [("type", ty')]);
        ([], emptyE))
    end)    

| ValRec({explicit, implicit}, valrecbind) =>
  let
    val triples = map (fn ((_,PatVar (Short f)), exp) =>    
       (f, exp, SMLTy.freshType ())) valrecbind
    val U = TyVar.Set.union(
      TyVar.Set.addList(TyVar.Set.empty, map TyVar.explicit explicit),
      TyVar.Set.difference (
        TyVar.Set.addList(TyVar.Set.empty, map TyVar.explicit implicit), 
        UofC C))
    val VE = List.foldr 
        (fn ((f,exp,ty), VE) => Map.insert 
          (VE, f, VarSch(monoType ty))) Map.empty triples
    val (VE', bindings) = infValRecBind (CplusVE (CplusU C U) VE) loc triples
    val Cfvs = freeVE (VEofE (EofC C))
    val tyvarsVE' = map (SMLTy.resolve loc Cfvs) (Map.listItems VE')
    val tyvarsVE' = (foldr TyVar.Set.union TyVar.Set.empty tyvarsVE')
    val tyvars= clos Cfvs tyvarsVE'
    val tyvars = List.filter 
          (fn tyvar => 
          if TyVar.isExplicit tyvar then TyVar.Set.member(U,tyvar)
          else true) tyvars
    val VE'' = Map.map (fn ty => VarSch(TypeScheme(tyvars, ty))) VE'
  in
    ([T.ValRec (tyvars, (hd bindings, tl bindings))], VEinE VE'')
  end

| Fun(tyvars, fvalbind) =>
  infDecItem C toplevel (translateFValBind (loc, (tyvars,fvalbind)))

(* Rule 16 *)
| Type typbind =>
  let
    val TE = infTypBind C typbind
  in
    ([], TEinE TE)
  end

(* Rule 17 -- modified *)
| Datatype (datbind,typbindopt) =>
  let
    val (VE, TE, r) = infDatBind false C (datbind,typbindopt)
  in
    ([], VETEinE (VE,TE))
  end

(* Rule 18 *)
| DatatypeCopy(tycon, longtycon) =>
  let
    val E = infDatCopy C loc (tycon, longtycon)
  in
    ([], E)
  end

(* Rule 19 *)
| Abstype (datbind, typbindopt, dec) =>
  let
    val (VE, TE, tynames) = infDatBind false C (datbind, typbindopt)
    val (d, E') = infDec (CplusE C (VETEinE (VE,TE))) false dec
      val r = foldl (fn (tyname, r) =>
              TyName.Map.insert(r, tyname, 
                TyName.newSort tyname TySort.anySort))
                TyName.Map.empty tynames
  in
    (d, renameE r (EplusTE E' TE))
  end
        
(* Rule 20 *)
| Exception exbind =>
  let
    val (d, EE) = infExBind C loc exbind
  in
    (d, VEinE (Map.map ExTy EE))
  end

(* Rule 21 *)
| Local(dec1, dec2) =>
  let
    val (d1, E1) = infDec C false dec1
    val (d2, E2) = infDec (CplusE C E1) toplevel dec2
  in
    ([T.Local(d1,d2)], E2)
  end

(* Rule 22 *)
| Open longids =>
  infOpen C loc longids

| (Infix _ | Infixr _ | Nonfix _) =>
  ([], emptyE)
  
| _ =>
  Debug.fail "ElabCore.infDecItem: non-core declaration"


(*----------------------------------------------------------------------*)
(* Open translation							*)
(*----------------------------------------------------------------------*)
and infOpen C loc [] = 
    ([], emptyE)

  | infOpen C loc (longid::longids) =
    let
      val (E', longid') = EnvLookup.lookupStr (EofC C, loc, longid)
      val (d, E'') = infOpen (CplusE C E') loc longids
    in
      (makeOpen (loc, E', longid') @ d, EplusE E' E'')
    end

and infDec C toplevel (dec : Dec) = 
case dec of
(* Rule 23 *)
  [] => 
  ([], emptyE)
    
(* Rule 24 *)
| (onedec as (loc,_))::dec =>
  let
    val (d1, E1) = infDecItem C toplevel onedec
    val (d2, E2) = infDec (CplusE C E1) toplevel dec
  in
    (d1 @ d2, EplusE E1 E2)
  end

(*----------------------------------------------------------------------*)
(* Value Bindings (p26 Defn)						*)
(* In contrast to the definition, we do closure of a type wrt E at this *)
(* point, in order to check the value restriction by looking at the     *)
(* translated term.							*)
(*----------------------------------------------------------------------*)
(* Rule 25 *)
and infValBind U C toplevel [] = 
    ([], Map.empty)

  (* For (pat = exp) do pattern compilation and binding *)
  | infValBind U C toplevel ((pat as (loc1,_), exp as (loc2,_))::valbind) =
    let
      val (p, VE2, ty1, patValuable) = infPat C pat
      val (e1, ty1') = infExp C exp
      val ty1' = unify ((SOME loc1, "val pattern", ty1),
                 (SOME loc2, "val expression", ty1'))
      val Cfvs = freeVE (VEofE (EofC C))
      val tyvarsVE2 = map (SMLTy.resolve loc1 Cfvs) 
        (map (fn VarSch(TypeScheme(_, ty)) => ty) (Map.listItems VE2))

      val tyvars = foldr TyVar.Set.union TyVar.Set.empty tyvarsVE2
      val tyvars = clos Cfvs tyvars
      val tyvars = List.filter 
        (fn tyvar => if TyVar.isExplicit tyvar then TyVar.Set.member(U,tyvar)
          else true) tyvars
      val expValuable = SMLTermOps.isValuable e1
      val restrictedTyvars = 
        if expValuable andalso patValuable
        then tyvars else []
      val VE2' = 
        Map.map (fn VarSch(TypeScheme([], ty)) => 
                    VarSch(TypeScheme(restrictedTyvars, ty)) 
                  | c => c) VE2
    in
      if null tyvars orelse (expValuable andalso patValuable)
         orelse (not (Controls.isOn "valueWarning") 
         andalso not toplevel)
      then () 
      else 
        let val message = (if expValuable then "pattern value restriction" 
         else "value restriction") ^ " prevents type variable generalization"
        in
          SMLTy.error 
          (if toplevel then Error.error (loc1, message)
                       else Error.warning (loc1, message), [])
        end;

      let
        val (d, VE1) = infValBind U C toplevel valbind
        val VE = patmerge loc1 (VE1, VE2')
      in
        (T.Val(loc1, restrictedTyvars, ty1', p, e1)::d, VE)
      end
    end

(* Rule 26 *)
and infValRecBind C loc [] = 
    (Map.empty, [])

  | infValRecBind C loc ((f, exp, ty1) :: valbind) =
    let
      val (e, ty2) = infExp C exp
      val (VE, bindings) = infValRecBind C loc valbind
      val ty3 = unify ((SOME loc, "fun pattern", ty1),
                 (NONE, "fun expression", ty2))
    in
      (Map.insert (VE, f, ty3), (f, e, ty1)::bindings)
    end
  
(*----------------------------------------------------------------------*)
(* Exception Bindings (p25 Defn)					*)
(*----------------------------------------------------------------------*)
(* Rules 30-31 *)
and infExBind C loc [] = 
    ([], Map.empty)

  | infExBind C loc (((_,excon),e)::exbind) =
    let
      val (d, EE) = infExBind C loc exbind
    in
      case e of
        ExDesc NONE => 
        let
          val exname = freshExName (pathofC C @ [excon],NONE,lamofC C)
        in
          (if lamofC C then T.Exception exname::d else d, 
          Map.insert(EE, excon, (exnType, exname)))
        end

      | ExDesc (SOME typ) => 
        let
          val ty = infTy C typ
          val exname = freshExName(pathofC C@[excon],SOME ty,lamofC C)
        in
          (if lamofC C then T.Exception exname::d else d, 
          Map.insert(EE, excon, (SMLTy.funType (ty, exnType), exname)))
        end

      | ExBind oplongid =>
        let
          val vid = ElabOps.vidToLongid oplongid
        in
          case EnvLookup.lookupVid (EofC C, loc, vid) of
            NONE => 
            (case EnvLookup.lookupTyCon (EofC C, loc, vid) of
              SOME tystr =>
              let 
                val ty = TyStr.apply(tystr, [])
                val CE = getClassEnv ()
              in
                if SMLClassDefOps.subClass CE (ty, TopEnv.exceptionclass)
                then
                let
                  val exname = freshJavaExName (pathofC C @ [excon], ty)
                in
                 ([], Map.insert(EE, excon, (exnType, exname)))
        	end
                else
                (SMLTy.error(Error.error(loc, 
                   "class does not subclass java.lang.Exception: " ^ 
                   SMLTy.toString ty), []);
                ([], EE))
              end

            | NONE =>
              (SMLTy.error (Error.error (loc, "unbound exception: " ^
                Pretty.longidToString vid), []); ([], EE))
           )

         | SOME vbind =>
            case vbind of
              VarSch _ => 
              (error (Error.error (loc,
                "found variable instead of exception: " ^ Pretty.longidToString
                vid), []); ([], EE))

            | ConSch _ => 
              (error (Error.error (loc, 
                "found data constructor instead of exception: " ^ 
                Pretty.longidToString vid), []); ([], EE))

            | ExTy(exbind as (_,exname)) => 
              ([], Map.insert (EE, excon, exbind))
        end
      end

end (* of local open *)

end (* of struct *)
