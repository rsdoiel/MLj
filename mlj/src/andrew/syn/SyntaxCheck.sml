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
(* Check the syntactic restrictions for a declaration expression, and	*)
(* resolve                                                              *)
(*   (a) infix expressions, patterns and fun decs;                      *)
(*   (b) implicitly scoped type variables.                              *)
(*                                                                      *)
(* In more detail, the syntactic restrictions are:                      *)
(* (1) those described in Sections 2.9 and 3.5 of the Defn;             *)
(* (2) our own restrictions on the Java extensions to ML;               *)
(* (3) datatype definitions must be uniform, that is, in a definition   *)
(*     datatype ('a_1,...,'a_n) T all occurrences of T must be of the   *)
(*     form ('a_1,...,'a_n) T.                                          *)
(*                                                                      *)
(* Also make sure that the entity type matches the declaration type and *)
(* that the entity name matches the identitier name.			*)  
(*======================================================================*)
structure SyntaxCheck :> SYNTAXCHECK =
struct

open Syntax SyntaxCheckOps

(*----------------------------------------------------------------------*)
(* Resolution of infix expressions					*)
(*----------------------------------------------------------------------*)
structure ResolveExp = 
ResolveInfix 
(
  type Object = Exp 
  fun pair (exp1 as (loc,_),exp2) = (loc, Tuple [exp1, exp2])
  
  fun asId (loc,LongVid (Short id)) = SOME id
    | asId _ = NONE

  fun applyId (id, exp as (loc,_)) = (loc,App((loc,LongVid(Short id)),exp))
  fun applyObj (exp1 as (loc,_),exp2) = (loc,App(exp1,exp2))
);

(*----------------------------------------------------------------------*)
(* Resolution of infix patterns						*)
(*----------------------------------------------------------------------*)
structure ResolvePat =
ResolveInfix
(
  type Object = Pat
  fun pair (pat1 as (loc,_),pat2) = (loc, PatTuple [pat1, pat2])

  fun asId (loc,PatVar (Short id)) = SOME id
    | asId _ = NONE

  fun applyId (id, pat as (loc,_)) = (loc, PatCon([id], pat))
  fun applyObj ((loc,PatVar(Short id)),pat) = (loc, PatCon([id], pat))
    | applyObj ((loc,PatVar(Long longid)),pat) = (loc, PatCon(longid, pat))

);

(*----------------------------------------------------------------------*)
(* Java constructs that remain in the new syntax			*)
(*----------------------------------------------------------------------*)
val newJava = 
  [Java.New, Java.InstanceOf, Java.Pure, Java.Synchronize]

(*----------------------------------------------------------------------*)
(* Check and translate an AST.						*)
(*----------------------------------------------------------------------*)
fun check { entity : Entity.Ref, AST, sourcemap } =

let

(*----------------------------------------------------------------------*)
(* List the value and type identifiers bound in a specification, paired *)
(* with their source locations.                                         *)
(* Accumulate the results in ids and tycons.                            *)
(*----------------------------------------------------------------------*)
local
  fun bs ([], ids, tycons) = (ids, tycons)
    | bs (((loc,prespecitem) : Syntax.SpecItem) :: rest, ids, tycons) =
  case prespecitem of

  (* Just value identifiers *)
  ValDesc valdesc => 
  bs (rest, map (fn (a,_) => (a,loc)) valdesc @ ids, tycons)

  (* Just type identifiers *)
| (TypeDesc typdesc) => 
  bs (rest, ids, map (fn (_,a,_) => (a,loc)) typdesc @ tycons)

| (EqTypeDesc typdesc) => 
  bs (rest, ids, map (fn (_,a) => (a,loc)) typdesc @ tycons)

  (* Both type (datatype+withtype) and value (constructor) identifiers *)
| DatatypeDesc (datdesc, typbindopt) => 
  bs (rest,
  map (fn ((_,a),_) => (a,loc)) (List.concat (map #3 datdesc)) @ ids, 

   map (fn (_,a,_) => (a,loc)) datdesc @
   (case typbindopt of NONE => []
                    | SOME typbind => map (fn (_,a,_) => (a,loc)) typbind)
   @ tycons)

  (* Just a type identifier; the imported constructors must be checked later *)
| DatatypeDescCopy(id, longid) =>
  bs (rest, ids, (id,loc)::tycons)

  (* Just value identifiers *)
| ExceptionDesc exdesc => 
  bs (rest, map (fn ((_,a),_) => (a,loc)) exdesc @ ids, tycons)

  (* Just a type identifier *)
| JavaDesc (ClassType { modifiers, tycon, super, implements, body })=>
  bs (rest, ids, (tycon,loc) :: tycons)

  (* Just a value identifier *)
| JavaDesc (ClassException((_,a),_)) => 
  bs (rest, (a,loc) :: ids, tycons)

| _ => 
  bs (rest, ids, tycons)

in
  fun specIds spec = bs (spec, [], [])
end 

(*----------------------------------------------------------------------*)
(* Check type expressions and collect free type variables, accumulating *)
(* them in the last argument.                                           *)
(*----------------------------------------------------------------------*)
fun checkTy level (ty as (loc, prety) : Syntax.Ty) (acc as (errors, tyvars)) =
case prety of
  TyVar tyvar =>
  (errors, Symbol.OrdMap.insert(tyvars, tyvar, level))

| TyCon(tys, longid) =>
  checkTys level tys acc

| TyFun(ty1, ty2) => 
  checkTy level ty1 (checkTy level ty2 acc)

| TyClass classid =>
  if JavaString.is_classname classid
  then 
    if Controls.isOn "deprecatedMLj"
    then (Error.warning(loc, 
      "construct now deprecated: remove quotes from Java class")::errors, 
      tyvars)
    else acc
  else (Error.error(loc, "illegal Java class identifier")::errors, tyvars)

(*......................................................................*)
(* Section 2.9: no type row may bind the same label twice	        *)
(*......................................................................*)
| TyRecord tyrow => 
  let 
    val (labels, tys) = ListPair.unzip tyrow
    val errors = 
      checkDupAtoms (loc, labels, "duplicate labels in record type") errors
  in
    checkTys level tys (errors, tyvars)
  end

| TyTuple tys => 
  checkTys level tys acc

(*----------------------------------------------------------------------*)
(* Check a list of types						*)
(*----------------------------------------------------------------------*)
and checkTys level tys acc =
  foldl (fn (ty, acc) => checkTy level ty acc) acc tys

(*----------------------------------------------------------------------*)
(* Check an optional type						*)
(*----------------------------------------------------------------------*)
fun checkTyOpt level NONE p = p
  | checkTyOpt level (SOME ty) p = checkTy level ty p

(*----------------------------------------------------------------------*)
(* Section 2.9: no tyvarseq may contain the same tyvar twice.      	*)
(*----------------------------------------------------------------------*)
fun checkTyVars (loc, tyvarseq) errors = 
  checkDupAtoms (loc, tyvarseq, "duplicate type variable names") errors

fun getTyCons ids (loc,prety) = 
case prety of
  TyVar _ => []
| TyCon (args, [id]) =>
  List.concat (map (getTyCons ids) args) @
  (if List.exists (fn id' => Symbol.equal (id, id')) ids
  then [(loc,args)]
  else [])
| TyFun(ty1,ty2) => getTyCons ids ty1 @ getTyCons ids ty2
| TyRecord row => List.concat (map (getTyCons ids o #2) row)
| (TyTuple tys | TyCon (tys, _)) => List.concat (map (getTyCons ids) tys)
| TyClass _ => []

fun substTyCon S (ty as (loc,prety)) =
case prety of
  TyVar _ => ty
| TyCon (args, [id]) =>
  let
    val args = map (substTyCon S) args
    fun find [] = (loc,TyCon(args, [id]))
      | find ((tyvarseq, id', ty)::S) =
        if Symbol.equal(id,id')
        then substTyVar (ListPair.zip(tyvarseq,args)) ty
        else find S
  in
    find S
  end
| TyFun(ty1,ty2) => (loc,TyFun(substTyCon S ty1, substTyCon S ty2))
| TyRecord row => (loc,TyRecord(map (fn (lab,ty) =>(lab,substTyCon S ty)) row))
| TyTuple tys => (loc,TyTuple (map (substTyCon S) tys))
| TyCon(tys,longid) => (loc,TyCon(map (substTyCon S) tys, longid))
| TyClass _ => ty

and substTyVar S (ty as (loc,prety)) =
case prety of
  TyVar id => 
  let
    fun find [] = ty
      | find ((id',ty)::S) = if Symbol.equal(id,id') then ty else find S
  in
    find S
  end
| TyCon (args, longid) => (loc, TyCon(map (substTyVar S) args, longid))
| TyFun(ty1,ty2) => (loc,TyFun(substTyVar S ty1, substTyVar S ty2))
| TyRecord row => (loc,TyRecord(map (fn (lab,ty) =>(lab,substTyVar S ty)) row))
| TyTuple tys => (loc,TyTuple (map (substTyVar S) tys))
| TyClass _ => ty

(*----------------------------------------------------------------------*)
(* Check pattern expression and collect free type variables.   		*)
(* The variable ie is bound to the current infix environment.           *)
(*----------------------------------------------------------------------*)
fun checkPat 
  (env as (level,ie))
  (pat as (loc, prepat) : Syntax.Pat) 
  (acc as (errors, tyvars)) =

case prepat of
  (PatWild | PatVar _) =>
  (pat, acc)

(*......................................................................*)
(* Section 2.9: no pattern row may bind the same lab twice.             *)
(*......................................................................*)
| PatRecord (flex, patrow) =>
  let 
    val (labels, pats) = ListPair.unzip patrow
    val errors = 
      checkDupAtoms (loc, labels, "duplicate labels in record pattern") errors
    val (pats, acc) = checkPats env pats (errors, tyvars)
  in
    ((loc,PatRecord (flex, ListPair.zip(labels, pats))), acc)
  end

| PatConstraint(pat, ty) => 
  let
    val (pat, acc) = checkPat env pat (checkTy level ty acc)
  in
    ((loc,PatConstraint(pat, ty)), acc)
  end

| PatLayer(id, tyopt, pat) => 
  let
    val (pat, acc) = checkPat env pat (checkTyOpt level tyopt acc)
  in
    ((loc,PatLayer(id, tyopt, pat)), acc)
  end

| PatTuple pats =>
  let
    val (pats, acc) = checkPats env pats acc
  in
    ((loc,PatTuple pats), acc)
  end

| PatList pats => 
  let
    val (pats, acc) = checkPats env pats acc
  in
    ((loc,PatList pats), acc)
  end

| OrPat pats => 
  let
    val (pats, acc) = checkPats env pats acc
  in
    ((loc,OrPat pats), acc)
  end

| PatParen pat =>
  checkPat env pat acc

(*......................................................................*)
(* Section 2.9: no real constant may occur in a pattern			*)
(*......................................................................*)
| PatSCon (SCon.RealCon _) =>
  (pat, (Error.error(loc, "real constant in pattern")::errors, tyvars))

| PatSCon _ =>
  (pat, acc)

| PatCon _ =>
  Debug.fail "SyntaxCheck.checkPat: found unary constructor"

| FlatPat pats =>
  let
    val (pats, acc as (errors,tyvars)) = checkPats env pats acc
  in
    (ResolvePat.resolve (ie, pats), acc) handle ResolvePat.InfixError s =>
    (pat, (Error.error(loc, s)::errors, tyvars))
  end

(*----------------------------------------------------------------------*)
(* Check several patterns.						*)
(*----------------------------------------------------------------------*)
and checkPats env [] acc = ([], acc)
  | checkPats env (pat::pats) acc =
    let
      val (pat,acc) = checkPat env pat acc
      val (pats,acc) = checkPats env pats acc
    in
      (pat::pats, acc)
    end

(*----------------------------------------------------------------------*)
(* Syntactic restrictions on type/datatype declarations.         	*)
(*----------------------------------------------------------------------*)
fun checkTypBind level (loc, datbindopt, typbindopt) (acc as (errors, tyvars))=
let 
  val tycons1 =
    case datbindopt of 
      NONE => [] 
    | SOME datbind => map (fn (x,y,z) => (x,y)) datbind

  val tycons2 = 
    case typbindopt of 
      NONE => []
    | SOME typbind => map (fn (x,y,z) => (x,y)) typbind

  val (tyvarseqs, tycons) = ListPair.unzip(tycons1 @ tycons2)

(*
  val errors =
    case datbindopt of
      NONE => errors
    | SOME datbind => 
      let
        val S = getOpt(typbindopt, [])
        val ids = map #2 tycons1
      in
        List.concat (map (fn (tyvarseq, tycon, conbinds) =>
        let 
          val argss = 
            List.concat 
              (map (fn (_,NONE) => [] | (_,SOME ty) => 
                getTyCons ids (substTyCon S ty)) conbinds)
          fun checkArgs (loc,args) =
          let
            fun check ([],[]) = true
              | check (tyvar::tyvars, (_,TyVar tyvar')::tys) =
                Symbol.equal(tyvar,tyvar') andalso check (tyvars, tys)
              | check _ = false
          in
            if check (tyvarseq, args)
            then NONE
            else SOME (Error.error(loc, 
              "datatype is used non-uniformly [MLJ restriction]: "
              ^ Pretty.simpleVec "," Pretty.idToString tyvarseq ^ " against "
              ^ Int.toString (length args)))
          end              
        in
          List.mapPartial checkArgs argss
        end) datbind) @ errors
      end
*)
      
  (*..................................................................*)
  (* No typbind/datbind may bind the same type identifier twice.      *)
  (*..................................................................*)
  val errors = checkDupAtoms (loc, tycons, "duplicate type definition") errors

  (*..................................................................*)
  (* Type variables must be distinct				      *)
  (*..................................................................*)
  val errors = foldl (fn (tyvarseq,errors) => 
    checkTyVars (loc, tyvarseq) errors) errors tyvarseqs

  val (cons, tyopts) = 
    case datbindopt of 
      NONE => ([], [])
    | SOME datbind => ListPair.unzip(List.concat (map #3 datbind))

  (*..................................................................*)
  (* No datbind may bind the same value identifier twice.	      *)
  (*..................................................................*)
  val errors = checkDupAtoms (loc, map (fn (_,a) => a) cons, 
    "duplicate constructor names in datatype declaration") errors

  val tyopts = 
    case typbindopt of 
      NONE => tyopts 
    | SOME typbind => map (SOME o #3) typbind @ tyopts

in
  foldl (fn (tyopt, acc) => checkTyOpt level tyopt acc) (errors, tyvars) tyopts
end

(*----------------------------------------------------------------------*)
(* Syntactic restrictions on expressions.                   		*)
(*----------------------------------------------------------------------*)
fun checkExp 
  (env as (level,ie)) 
  (exp as (loc, preexp) : Exp) 
  (acc as (errors, tyvars)) =

case preexp of
  LongVid _ =>
  (exp, acc)

| SCon _ =>
  (exp, acc)

| App _ =>
  Debug.fail "SyntaxCheck.checkExp: function application found"

| FlatApp exps =>
  let
    val (exps,acc as (errors, tyvars)) = checkExps env exps acc
  in
     (ResolveExp.resolve (ie,exps), acc) 
     handle ResolveExp.InfixError s => 
     (exp, (Error.error(loc, s)::errors, tyvars))
  end

| Fn match => 
  let
    val (match, acc) = checkMatch env match acc
  in
    ((loc, Fn match), acc)
  end

| Let(dec, exp) =>
  let
    val (dec, acc, ie) = checkDec env dec acc
    val (exp, acc) = checkExp (level,ie) exp acc
  in
    ((loc, Let(dec, exp)), acc)
  end

| Constraint(exp, ty) => 
  let
    val (exp, acc) = checkExp env exp acc
    val acc = checkTy level ty acc
  in
    ((loc, Constraint(exp, ty)), acc)
  end

| ConstraintGt(exp, ty) => 
  let
    val (exp, acc) = checkExp env exp acc
    val acc = checkTy level ty acc
  in
    ((loc, ConstraintGt(exp, ty)), acc)
  end

| Handle(exp, match) => 
  let
    val (exp, acc) = checkExp env exp acc
    val (match, acc) = checkMatch env match acc
  in
    ((loc, Handle(exp, match)), acc)
  end

| Raise exp => 
  let
    val (exp, acc) = checkExp env exp acc
  in
    ((loc, Raise exp), acc)
  end

(*......................................................................*)
(* Section 2.9: no expression row may bind the same lab twice.		*)
(*......................................................................*)
| Record exprow => 
  let 
    val (labels, exps) = ListPair.unzip exprow
    val errors = 
      checkDupAtoms (loc, labels, "duplicate labels in record") errors
    val (exps, acc) = checkExps env exps (errors, tyvars)
  in
    ((loc, Record(ListPair.zip(labels, exps))), acc)
  end

| Java(jop, tyopt, idopt, exps) =>
  let
    val acc = checkTyOpt level tyopt acc
    val (exps, (errors,tyvars)) = checkExps env exps acc
    val errors = 
      if List.exists (fn jop' => jop' = jop) newJava 
      orelse not (Controls.isOn "deprecatedMLj")
      then errors
      else Error.warning(loc, "old-style Java construct")::errors
  in
    ((loc, Java(jop, tyopt, idopt, exps)), (errors,tyvars))
  end

| Tuple exps =>
  let
    val (exps, acc) = checkExps env exps acc
  in
    ((loc, Tuple exps), acc)
  end

| (Hash _ | DotHash _ | DotHashHash _) =>
  (exp, acc)

| Case(exp, match) =>
  let
    val (exp, acc) = checkExp env exp acc
    val (match, acc) = checkMatch env match acc
  in
    ((loc, Case(exp, match)), acc)
  end

| If (exp1, exp2, exp3) =>
  let
    val (exp1, acc) = checkExp env exp1 acc
    val (exp2, acc) = checkExp env exp2 acc
    val (exp3, acc) = checkExp env exp3 acc
  in
    ((loc, If(exp1, exp2, exp3)), acc)
  end

| Orelse (exp1, exp2) =>
  let
    val (exp1, acc) = checkExp env exp1 acc
    val (exp2, acc) = checkExp env exp2 acc
  in
    ((loc, Orelse(exp1, exp2)), acc)
  end

| Andalso (exp1, exp2) =>
  let
    val (exp1, acc) = checkExp env exp1 acc
    val (exp2, acc) = checkExp env exp2 acc
  in
    ((loc, Andalso(exp1, exp2)), acc)
  end

| Sequence exps =>
  let
    val (exps, acc) = checkExps env exps acc
  in
    ((loc, Sequence exps), acc)
  end

| While (exp1, exp2) =>
  let
    val (exp1, acc) = checkExp env exp1 acc
    val (exp2, acc) = checkExp env exp2 acc
  in
    ((loc, While(exp1, exp2)), acc)
  end

| List exps =>
  let
    val (exps, acc) = checkExps env exps acc
  in
    ((loc, List exps), acc)
  end

and checkExps env [] acc = ([], acc)
  | checkExps env (exp::exps) acc =
    let
      val (exp,acc) = checkExp env exp acc
      val (exps,acc) = checkExps env exps acc
    in
      (exp::exps, acc)
    end

(*----------------------------------------------------------------------*)
(* Check a match							*)
(*----------------------------------------------------------------------*)
and checkMatch env [] acc = 
    ([], acc)

  | checkMatch env ((pat,exp)::match) acc = 
    let
      val (pat,acc) = checkPat env pat acc
      val (exp, acc) = checkExp env exp acc
      val (match, acc) = checkMatch env match acc
    in
      ((pat,exp)::match, acc)
    end

and makeBound level ({ explicit, implicit }, freetyvars) = 
  { explicit = explicit, 
    implicit = 
      List.filter 
      (fn tyvar => not (List.exists 
        (fn tyvar' => Symbol.equal(tyvar,tyvar')) explicit))
      (map #1 (Symbol.OrdMap.listItemsi 
        (Symbol.OrdMap.filter (fn level' => level' = level+1) freetyvars)))
  }

(*----------------------------------------------------------------------*)
(* Check a declaration item.               			        *)
(* Return a transformed decitem, accumulated type variables/errors list *)
(* and a new infix environment.                                         *)
(*----------------------------------------------------------------------*)
and checkDecItem 
  (env as (level,ie)) 
  (decitem as (loc, predecitem) : Syntax.DecItem) 
  (acc as (errors, tyvars)) =

case predecitem of
  Val(boundtyvars, match) =>
  let
    val (match, acc as (errors,freetyvars)) = checkMatch (level+1,ie) match acc
  in
    ((loc, Val(makeBound level (boundtyvars, freetyvars), match)), acc, ie)
  end
          
| ValRec(boundtyvars, match) =>
  let
    val (match, acc as (errors,freetyvars)) = checkMatch (level+1,ie) match acc
  in
    ((loc, ValRec(makeBound level (boundtyvars, freetyvars), match)), acc, ie)
  end

| FlatFun(boundtyvars, fvalbinds) =>
  let
    val (fvalbinds, acc as (errors,freetyvars)) = 
      checkFValBind (level+1,ie) fvalbinds acc
  in
    ((loc, Fun(makeBound level (boundtyvars, freetyvars), fvalbinds)), acc, ie)
  end

| Fun _ =>
  Debug.fail "SyntaxCheck.checkDecItem: found fun dec"

| Type typbind =>
  (decitem, checkTypBind level (loc, NONE, SOME typbind) acc, ie)

| Datatype (datbind, typbindopt) =>
  (decitem, checkTypBind level (loc, SOME datbind, typbindopt) acc, ie)

| DatatypeCopy _ =>
  (decitem, acc, ie)
  
| Abstype (datbind, typbindopt, dec) =>
  let
    val acc = checkTypBind level (loc, SOME datbind, typbindopt) acc
    val (dec, acc, ie) = checkDec env dec acc
  in
    ((loc, Abstype (datbind, typbindopt, dec)), acc, ie)
  end

| Exception exbind =>
  let
    val errors = checkDupAtoms (loc, map (#2 o #1) exbind, 
      "duplicate exception declaration") errors
    val acc = checkExBind level exbind (errors,tyvars)
  in
    (decitem, acc, ie)
  end

| Local (dec1, dec2) =>
  let
    val (dec1, acc, ie1) = checkDec env dec1 acc
    val (dec2, acc, ie2) = checkDec (level, ie1) dec2 acc
  in
    ((loc, Local (dec1, dec2)), acc, ie2)
  end

| Open strids =>
  (decitem, acc, ie)

| JavaDec javadec =>
  let
    val (javadec, acc) = checkJavaDec false env (loc, javadec) acc
  in
    ((loc, JavaDec javadec), acc, ie)
  end

| Structure strbinds =>
  let
    val errors = 
      checkDupAtoms (loc, map #1 strbinds, 
        "duplicate structure identifiers") errors

    val (strbinds, acc) = 
      foldr (fn (strbind, (strbinds,acc)) =>
      let 
        val (strbind,acc) = checkStrBind env strbind acc
      in
        (strbind::strbinds, acc)
      end) ([],(errors,tyvars)) strbinds
  in
    ((loc, Structure strbinds), acc, ie)
  end

| Signature sigbinds =>
  let
    val errors = 
      checkDupAtoms (loc, map #1 sigbinds, 
        "duplicate signature identifiers") errors

    val (sigbinds, acc) = 
      foldr (fn (sigbind, (sigbinds,acc)) =>
      let 
        val (sigbind,acc) = checkSigBind env sigbind acc
      in
        (sigbind::sigbinds, acc)
      end) ([],(errors,tyvars)) sigbinds
  in
    ((loc, Signature sigbinds), acc, ie)
  end

| Infix (prec, symbols) =>
  (decitem, acc, Fixity.updateEnv(ie, symbols, Fixity.Infix(prec,false)))

| Infixr (prec, symbols) =>
  (decitem, acc, Fixity.updateEnv(ie, symbols, Fixity.Infix(prec,true)))

| Nonfix symbols =>
  (decitem, acc, Fixity.updateEnv(ie, symbols, Fixity.Nonfix))

and checkDec (env as (level,ie)) [] acc = ([], acc, ie)
  | checkDec (env as (level,ie)) (decitem::dec) acc = 
    let
      val (decitem,acc,ie1) = checkDecItem env decitem acc
      val (dec,acc,ie2) = checkDec (level, ie1) dec acc
    in
      (decitem::dec, acc, ie2)
    end

and checkStrBind env (strid, strexp, siginfo) acc =
let
  val (strexp, acc) = checkStrExp env strexp acc
  val (siginfo, acc) = checkSigInfo env siginfo acc
in
  ((strid, strexp, siginfo), acc)
end

and checkSigBind env (sigid, sigexp) acc =
let
  val (sigexp, acc) = checkSigExp env sigexp acc
in
  ((sigid, sigexp), acc)
end

and checkSigInfo env SigNone acc = 
    (SigNone, acc)

  | checkSigInfo env (SigConcrete sigexp) acc = 
    let 
      val (sigexp, acc) = checkSigExp env sigexp acc
    in 
      (SigConcrete sigexp, acc) 
    end

  | checkSigInfo env (SigAbstract sigexp) acc = 
    let 
      val (sigexp, acc) = checkSigExp env sigexp acc     
    in 
      (SigAbstract sigexp, acc) 
    end

(*----------------------------------------------------------------------*)
(* Check a Java declaration/specification				*)
(*----------------------------------------------------------------------*)
and checkJavaDec insig env (loc,javadec) (acc as (errors, tyvars)) =

let
(*----------------------------------------------------------------------*)
(* Check a class item in a _classtype declaration			*)
(*----------------------------------------------------------------------*)
fun checkClassItem (item as (loc, preitem)) (acc as (errors, tyvars)) = 
case preitem of

  (*..................................................................*)
  (* Check that the field modifiers are distinct (8.3.1,JLS) and do   *)
  (* not contain more than one of _public, _protected and _private.   *)
  (*..................................................................*)
  Field { modifiers, name, ty, initial } => 
  let
    val errors = checkAccessMods (loc, modifiers) errors
    val errors = 
      checkDupMods (loc, modifiers, "duplicate field modifiers") errors
    val errors = 
      if insig andalso List.exists (fn m => m=JavaFlags.PRIVATE) modifiers
      then Error.error(loc, 
        "field declaration in signature cannot be private")::errors
      else errors
    
    val errors = 
      if List.exists (fn m => m=JavaFlags.STATIC) modifiers
      andalso Controls.isOn "deprecatedMLj"
      then Error.warning(loc, 
        "static fields now deprecated: export a structure instead")::errors
      else errors

    val errors =
      if JavaString.is_identifier name then errors
      else Error.error (loc,"illegal Java field identifier")::errors

    val (errors, tyvars) = checkTy 0 ty (errors, tyvars)

    val (initial, acc) = 
      case initial of 
        NONE => 
        (NONE, (errors,tyvars))

      | SOME exp => 
        if insig 
        then (NONE, 
          (Error.error(loc, 
            "field declaration in signature cannot have initialiser")::errors,
           tyvars))
        else
        let 
          val (exp, acc) = checkExp env exp (errors, tyvars)
        in
          (SOME exp, acc)
        end

  in
    ((loc, 
     Field { modifiers = modifiers, name = name, ty = ty, initial = initial }),
    acc)
  end

  (*..................................................................*)
  (* Check that the method modifiers are distinct (8.4.3,JLS),        *)
  (* do not contain more than one of _public, _protected and _private,*)
  (* and do not contain both _abstract and any one of _private,       *)
  (*_static, _final, _native, or _synchronized.                       *)
  (* Check that no two argument names are the same (8.4.1,JLS).       *)
  (* Check that abstract methods do not have bodies (8.4.3.1,JLS).    *)
  (*..................................................................*)
| Method { modifiers, name, args, body, result } =>
  let
    val errors = checkAccessMods (loc, modifiers) errors
    val errors = 
      checkDupMods (loc, modifiers, "duplicate method modifiers") errors

    val errors = 
      if List.exists (fn m => m=JavaFlags.STATIC) modifiers
      andalso Controls.isOn "deprecatedMLj"
      then Error.warning(loc, 
        "static methods now deprecated: export a structure instead")::errors
      else errors

    val errors =       
      if insig andalso List.exists (fn m =>m=JavaFlags.PRIVATE) modifiers
      then Error.error(loc,
        "method declaration in signature cannot be private")::errors
      else errors
   
    val errors = 
      if List.exists (fn m => m=JavaFlags.ABSTRACT) modifiers
      then
        if List.exists (fn m => m=JavaFlags.PRIVATE orelse m=JavaFlags.STATIC
          orelse m=JavaFlags.FINAL orelse m=JavaFlags.SYNCHRONIZED) modifiers
        then Error.error(loc,
          "abstract method cannot be private/static/final/synchronized")
          ::errors
        else 
          if isSome body 
          then Error.error(loc,"abstract method cannot have body")::errors
          else errors
      else errors

    val errors =
      if JavaString.is_identifier name 
      then errors
      else Error.error(loc,"illegal Java method identifier")::errors

    val errors = checkDupAtoms (loc, List.mapPartial #1 args,
      "duplicate argument names") errors

    val (errors,tyvars) = 
      checkTys 0 (map #2 args) (checkTyOpt 0 result (errors,tyvars))

    val (body, acc) = 
      case body of
        NONE => 
        (NONE, (errors,tyvars))

      | SOME exp => 
        if insig 
        then (NONE, (Error.error(loc,
          "method declaration in signature cannot have body")::errors,tyvars))
        else
        let
          val (exp, acc) = checkExp env exp (errors,tyvars)
        in
          (SOME exp, acc)
        end
  in
    ((loc, Method { modifiers = modifiers, name = name, args = args,
      body = body, result = result }), acc)
  end

  (*..................................................................*)
  (* Check that the constructor modifiers are distinct (8.6.3,JLS),   *)
  (* do not contain more than one of _public, _protected and _private.*)
  (*..................................................................*)
| Constructor { modifiers, args, body, inits } =>
  let
    val errors = checkAccessMods (loc, modifiers) errors
    val errors = 
      checkDupMods (loc, modifiers, "duplicate constructor modifiers") errors

    val errors = checkDupAtoms (loc, List.mapPartial #1 args,
      "duplicate argument names") errors

    val acc as (errors,tyvars) = 
      checkTys 0 (map #2 args) (errors,tyvars)

    val (inits, acc as (errors,tyvars)) =
      case inits of
        NoInit =>
        if not insig then (NoInit, (Error.error(loc,
        "missing initialisers in constructor declaration")::errors,tyvars))
        else (NoInit, acc)

      | ThisInvoc exps =>
        if insig then (NoInit, (Error.error(loc, 
          "constructor declaration in signature cannot have initialisers")
          ::errors,tyvars))
        else
        let 
          val (exps,acc) = checkExps env exps acc
        in 
          (ThisInvoc exps, acc) 
        end        

      | SuperInvoc (exps, fldinits) =>
        if insig 
        then (NoInit, (Error.error(loc,
          "constructor declaration in signature cannot have initialisers")::
          errors, tyvars))
        else
        let 
(*
          val errors = checkDupAtoms (loc, map #1 fldinits,
            "duplicate field identifiers in field initialisation") errors
*)

          val (fldinits, acc) = 
            foldr (fn ((id,exp), (fldinits,acc)) =>
            let 
              val (exp, acc as (errors,tyvars)) = checkExp env exp acc
              val (id, acc) = 
                if JavaString.is_identifier id
                then (id, acc)
                else (id, (Error.error(loc,
                  "illegal Java field identifier")::errors, tyvars))
            in 
              ((id,exp)::fldinits, acc) 
            end) ([], (errors,tyvars)) fldinits

          val (exps, acc) = checkExps env exps acc
        in 
          (SuperInvoc (exps,fldinits), acc)
        end

    val (body, acc) = 
      case body of
        NONE => 
        (NONE, acc)

      | SOME exp => 
        if insig 
        then (NONE, (Error.error(loc,
          "constructor declaration in signature cannot have body")::errors,
          tyvars))
        else
        let
          val (exp, acc) = checkExp env exp acc
        in
          (SOME exp, acc)
        end
  in
    ((loc, Constructor { args = args, body = body, inits = inits,
             modifiers = modifiers }), 
    acc)
  end

in
case javadec of
  (*..................................................................*)
  (* Check that the class modifiers are distinct (8.1,JLS) and do     *)
  (* not contain both _abstract and _final (8.1.2.2, JLS).            *) 
  (*..................................................................*)
  ClassType { modifiers, tycon, super, implements, body } =>
  let
    val errors = 
      checkDupMods (loc, modifiers, "duplicate class modifiers") errors
 
    val errors = 
      if List.exists (fn m => m=JavaFlags.ABSTRACT) modifiers
        andalso List.exists (fn m => m=JavaFlags.FINAL) modifiers
      then Error.error(loc,"class is both abstract and final")::errors
      else errors

    val acc = checkTys 0 implements (checkTyOpt 0 super (errors,tyvars))

    val (body, acc) = 
      foldr (fn (item, (body,acc)) => 
      let
        val (item,acc) = checkClassItem item acc
      in
        (item::body, acc)
      end) ([],acc) body
  in
    (ClassType { modifiers = modifiers, tycon = tycon, 
                 super = super, implements = implements, body = body },
     acc)
  end

| ClassException(tycon, classid) =>
  if JavaString.is_classname classid
  then 
    if Controls.isOn "deprecatedMLj"
    then (javadec, (Error.warning(loc, 
      "construct now deprecated: remove quotes from Java exception class")
      ::errors, tyvars))
    else (ClassException(tycon, classid), acc)
  else (javadec, 
    (Error.error(loc,"illegal Java class identifier")::errors,tyvars))
end  

(*----------------------------------------------------------------------*)
(* Check a fun binding							*)
(*----------------------------------------------------------------------*)
and checkFValBind 
  (env as (level,ie))
  fvalbind
  acc =
let 
  fun checkOneFValBind binds acc =
  let
    fun check (fvar,len) [] acc = ([], acc)

      | check (fvar,len) ((loc, pats, exp, tyopt)::rest) acc =         
        let 
          val (exp, acc) = checkExp env exp acc
          val acc = checkTyOpt level tyopt acc
          fun default () =
            case pats of
              (_,PatVar(Short f | OpShort f))::pats => 
              let
                val (pats, acc) = checkPats env pats acc
              in
                (f, pats, acc)
              end

          val (fvar', pats, acc) =
            case pats of

              (* Check for infix operator of form x ++ y ... *)
              (pat1::(loc,PatVar(Short f))::pat2::pats 
              | (_,PatParen (_,FlatPat [pat1, (loc,PatVar(Short f)), pat2])) :: pats) =>
              (case Fixity.lookup(ie, f) of
                Fixity.Infix _ =>
                let
                  val (pat1, acc) = checkPat env pat1 acc
                  val (pat2, acc) = checkPat env pat2 acc
                  val (pats, acc) = checkPats env pats acc
                in
                  (f, (loc,PatTuple [pat1,pat2])::pats, acc)
                end
              | _ => default ())

            | (_,FlatPat pats)::pats' =>
              let
                val (pats, acc) = checkPats env pats acc
                val (pats', acc) = checkPats env pats' acc
                val pat = ResolvePat.resolve (ie, pats)
              in
                case pat of
                  (_,PatCon([f],pat)) => (f, pat::pats', acc)
              end        

            | _ => default ()

          val (rest, acc as (errors,tyvars)) = 
            check (SOME fvar',length pats) rest acc        

          val errors = 
            if null pats 
            then Error.error(loc, "missing arguments in function declaration")
              ::errors
            else errors

          val errors = 
            case fvar of 
              NONE => errors
            | SOME fvar => 
              if not(Symbol.equal(fvar,fvar'))
              then Error.error(loc, "clauses don't all have function name " ^ 
                Pretty.idToString fvar)::errors
              else if len = length pats 
              then errors
              else Error.error(loc, 
                "clauses don't all have same number of patterns")::errors
        in
          ((loc, fvar', pats, exp, tyopt)::rest, (errors,tyvars))
        end
  in
    check (NONE,0) binds acc
  end

  fun checkAll [] acc = ([], acc)
    | checkAll (bind::binds) acc =
      let
        val (bind, acc) = checkOneFValBind bind acc
        val (binds, acc) = checkAll binds acc
      in
        (bind::binds, acc)
      end

  val (fvalbind, (errors,tyvars)) = checkAll fvalbind acc
  val errors = checkDupAtoms (#1 (hd (hd fvalbind)), map (#2 o hd) fvalbind,
    "duplicate function names in fun declaration") errors
in
  (fvalbind, (errors,tyvars))
end

(*----------------------------------------------------------------------*)
(* Syntactic restrictions on exception declarations.		        *)
(* Section 2.9: no binding exbind may bind the same identifier twice.   *)
(*----------------------------------------------------------------------*)
and checkExBind level [] acc = acc
  | checkExBind level ((_,bind)::rest) acc =
    case bind of
      ExDesc tyopt => checkExBind level rest (checkTyOpt level tyopt acc)
    | _ => checkExBind level rest acc

(*----------------------------------------------------------------------*)
(* Check a specification item						*)
(*----------------------------------------------------------------------*)
and checkSpecItem env (specitem as (loc,prespecitem) : Syntax.SpecItem) acc =
case prespecitem of
  ValDesc valdesc => 
  (specitem, checkTys 0 (map #2 valdesc) acc)

| TypeDesc typdesc => 
  (specitem, foldl (fn ((tyvarseq,_,tyopt),acc as (errors,tyvars)) =>
    checkTyOpt 0 tyopt (checkTyVars (loc,tyvarseq) errors, tyvars)) 
    acc typdesc)
       
| EqTypeDesc eqtypdesc =>
  (specitem, foldl (fn ((tyvarseq,_),acc as (errors,tyvars)) =>
    (checkTyVars (loc,tyvarseq) errors,tyvars)) acc eqtypdesc)

| DatatypeDesc (datbind, typbindopt) =>
  (specitem, checkTypBind 0 (loc, SOME datbind, typbindopt) acc)

| DatatypeDescCopy _ =>
  (specitem, acc)

| ExceptionDesc exdesc =>
  (specitem, foldl (fn ((_,tyopt),acc) => checkTyOpt 0 tyopt acc) acc exdesc)

| JavaDesc dec =>
  let 
    val (dec, acc) = checkJavaDec true env (loc,dec) acc
  in 
    ((loc,JavaDesc dec), acc)
  end

| StructureDesc strdesc =>
  let
    val (strdesc, acc) = 
      foldr (fn ((strid,sigexp), (strdesc,acc)) =>
      let
        val (sigexp,acc) = checkSigExp env sigexp acc
      in
        ((strid,sigexp)::strdesc, acc)
      end) ([],acc) strdesc
  in 
    ((loc,StructureDesc strdesc), acc)
  end

| Include sigexp =>
  let
    val (sigexp, acc) = checkSigExp env sigexp acc
  in
    ((loc,Include sigexp), acc)
  end

| Sharing longids =>
  (specitem, acc)

| SharingType longids =>
  (specitem, acc)

(*----------------------------------------------------------------------*)
(* Check a specification						*)
(*----------------------------------------------------------------------*)
and checkSpec env spec (errors,tyvars) =
let 
  val (vars, tycons) = specIds spec

  (*..................................................................*)
  (* No description may describe the same value identifier twice      *)
  (*..................................................................*)
(*
  val errors =
    checkDupLocAtoms 
    (vars, "duplicate specifications for variable or constructor") errors
*)

  (*..................................................................*)
  (* No description may describe the same type identifier twice       *)
  (*..................................................................*)
  val errors = 
    checkDupLocAtoms 
    (tycons, "duplicate specifications for type constructor") errors

in
  foldr (fn (specitem, (spec,acc)) =>
    let
      val (specitem,acc) = checkSpecItem env specitem acc
    in
      (specitem::spec, acc)
    end) ([], (errors,tyvars)) spec
end

(*----------------------------------------------------------------------*)
(* Check a signature expression						*)
(*----------------------------------------------------------------------*)
and checkSigExp env (sigexp as (loc,presigexp) : Syntax.SigExp) acc =
case presigexp of
  SigSpec spec => 
  let
    val (spec, acc) = checkSpec env spec acc
  in
    ((loc,SigSpec spec), acc)
  end

| Sigid _ =>
  (sigexp, acc)

| Where(sigexp, tyvars, longtycon, ty) =>
  let
    val (sigexp, acc) = checkSigExp env sigexp acc
    val acc = checkTy 0 ty acc
  in
    ((loc, Where(sigexp, tyvars, longtycon, ty)), acc)
  end

(*----------------------------------------------------------------------*)
(* Check a structure expression						*)
(*----------------------------------------------------------------------*)
and checkStrExp 
  (env as (level,ie)) 
  (strexp as (loc,prestrexp) : Syntax.StrExp) 
  acc =
case prestrexp of
  Struct dec => 
  let
    val (dec, acc, ie) = checkDec env dec acc
  in
    ((loc, Struct dec), acc)
  end

| Strid longid =>
  (strexp, acc)

| StrTransparent(strexp, sigexp) => 
  let
    val (strexp, acc) = checkStrExp env strexp acc
    val (sigexp, acc) = checkSigExp env sigexp acc
  in
    ((loc, StrTransparent(strexp, sigexp)), acc)
  end

| StrOpaque(strexp, sigexp) => 
  let
    val (strexp, acc) = checkStrExp env strexp acc
    val (sigexp, acc) = checkSigExp env sigexp acc
  in
    ((loc, StrOpaque(strexp, sigexp)), acc)
  end

| FunApp(id, strexp) => 
  let
    val (strexp, acc) = checkStrExp env strexp acc
  in
    ((loc, FunApp(id, strexp)), acc)
  end

| StrLet(dec, strexp) => 
  let
    val (dec, acc, ie) = checkDec env dec acc
    val (strexp, acc) = checkStrExp (level,ie) strexp acc
  in
    ((loc, StrLet(dec, strexp)), acc)
  end


  (* DO IT ! *)
  val (AST, (errors,_), _) = 
    checkDec (0,Fixity.initialEnv) AST ([], Symbol.OrdMap.empty)

  val errors =
    case (#1 entity, AST) of
      (Entity.Str, [(loc, Structure [(id, _, _)])]) =>
      errors

    | (Entity.Sig, [(loc, Signature [(id, _)])]) =>
      errors
   
    | (Entity.Fun, [(loc, Functor [(id, _, _, _)])]) =>
      errors

    | (_, (loc, _)::_) =>
      Error.error(loc, 
        "expected single structure, signature or functor binding")::errors    
    
in
  {
    AST = AST,
    errors = errors
  }
end (* of let *)

end (* of struct *)
