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
(* Sharing of types (ML base types -> Java, sum types -> unisum,        *)
(*      enumeration types -> ints)                                      *)
(*======================================================================*)
structure Share :> TRANSFORMER =
struct

local 
  open MILTerm  
in

(*----------------------------------------------------------------------*)
(* Is the type the universal sum type?      				*)
(*----------------------------------------------------------------------*)
fun isUniSum ty =
  case MILTy.fromCon ty of
    SOME [] => true
  | _ => false

fun isUniSum' ty =
case MILTy.fromSum ty of
  SOME ([[], [ty]] | [[ty], []]) => 
  false

| SOME tyss =>
  not (List.all List.null tyss)

| _ => false



(*----------------------------------------------------------------------*)
(* Apply a transformation to the term ce				*)
(*----------------------------------------------------------------------*)
fun transform tyenv ce =
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
  fun transVal (env as (tyenv,kindenv)) ve =
  case ve of

(*......................................................................*)
(* Constant introduction                    				*)
(*......................................................................*)
  SCon(ty, c) => 
  (SCon (ty, transCon c), ty, [])

(*......................................................................*)
(* Variables								*)
(*......................................................................*)
| Var x => 
  (ve, Var.lookup(tyenv, x), [])

(*......................................................................*)
(* Mu introduction							*)
(*......................................................................*)
| Fold(ve, muty) =>
  let 
    val (ve,subty,b) = transVal env ve
    val resultve = 
      if isUniSum muty 
      then ve
      else Fold(ve, muty)
  in
    (resultve, muty, b)
  end

(*......................................................................*)
(* Mu elimination							*)
(*......................................................................*)
| Unfold ve =>
  let
    val (ve, subty, b) = transVal env ve 

    (* The type of the subterm must be a mu type *)
    val a =
      case MILTy.fromMu subty of
        NONE => Debug.fail "Share.transVal: expected mu type"
      | SOME a => a

    (* Unfold this type one level *)
    val unfoldty = MILTy.unfold a

    val resultve = 
      if isUniSum subty 
      then ve
      else Unfold ve
  in
    (resultve, unfoldty, b)
  end

(*......................................................................*)
(* Sum introduction                                			*)
(*......................................................................*)
| Inj(ty, i, velist) => 
  let 
    val (velist, tys, b) = transVals env velist
  in
    (Inj(ty, i, velist), ty, b)
  end

(*......................................................................*)
(* Universal sum/exn coercion						*)
(*......................................................................*)
| Coerce(ve, ty) =>
  let
    val (ve,_,b) = transVal env ve
  in
    (Coerce(ve, ty), ty, b)
  end

(*......................................................................*)
(* Exception introduction                                               *)
(*......................................................................*)
| ExCon(exname, velist) => 
  let
    val (velist,tys,b) = transVals env velist
  in
    (ExCon(exname, velist), MILTys.topExn, b)
  end

(*......................................................................*)
(* Product introduction                                           	*)
(*......................................................................*)
| Tuple velist => 
  let
    val (velist,tys,b) = transVals env velist
  in
    (Tuple velist, MILTy.prod tys, b)
  end

(*......................................................................*)
(* Product elimination                                    		*)
(*......................................................................*)
| Proj(i, ve) => 
  let
    val (ve,prodty,b) = transVal env ve
  in
    case MILTy.fromProdCon prodty of
      NONE => 
      MILPretty.failVal ve 
        "Share.transVal: expected product/constructor type"

    | SOME tys =>
      let 
        val resultty = List.nth(tys, i)
      in 
        (Proj(i, ve), resultty, b) 
      end
  end

(*......................................................................*)
(* Quantifier elimination						*)
(*......................................................................*)
| TApp(ve, tys) => 
  let
    val (ve, polyty, b) = transVal env ve
    val SOME a = MILTy.fromForall polyty
    val ty = MILTy.app (MILTy.abs a, tys)
  in
    (TApp(ve, tys), ty, b)
  end

(*......................................................................*)
(* Quantifier introduction						*)
(*......................................................................*)
| TAbs(tyvars, ve) =>
  let
    val (ve, ty, b) = transVal (tyenv, Var.extend(kindenv,tyvars)) ve
  in
    (TAbs(tyvars, ve), MILTy.forall(tyvars, ty), b)
  end

| _ =>
  MILPretty.failVal ve "Share.transVal: illegal value term"

and transVals env [] = 
    ([],[],[])

  | transVals env (v::vs) =
    let
      val (v,ty,b) = transVal env v
      val (vs,tys,b') = transVals env vs
    in
      (v::vs, ty::tys, b@b')
    end

and transCmp (env as (tyenv, kindenv)) ce =
let
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
  App(ve, velist) =>
  let
    val (ve, funty, b) = transVal env ve
    val (velist, tys, b') = transVals env velist
  in
  case MILTy.fromArrow funty of
    NONE => 
    MILPretty.failCmp ce "Share.transCmp: expected arrow type"

  | SOME (_, cty) => 
    let
      val (eff,restys) = MILTy.fromCmp cty
    in
      (makeBinds (b, App(ve, velist)), cty)
    end
  end

(*......................................................................*)
(* Java operation							*)
(*......................................................................*)
| Java(j as (jop, tyopt, nameopt), velist, cty) =>
  let
    val (velist, tys, b) = transVals env velist
  in
    (makeBinds (b, Java((jop, tyopt, nameopt), velist, cty)), cty)
  end

(*......................................................................*)
(* Moggi-let								*)
(*......................................................................*)
| Let(ce1, (typedvars, ce2)) =>
  let
    val (ce1, ce1ty) = transCmp env ce1
    val (effect, tys) = MILTy.fromCmp ce1ty
    val (ce2, ce2ty) = 
      transCmp (Var.extend(tyenv, typedvars), kindenv) ce2
  in
    (Let(ce1, (typedvars, ce2)), MILTy.unionCmpTypes(ce1ty,ce2ty))
  end
    
(*......................................................................*)
(* Value bindings.                                                      *)
(*......................................................................*)
| LetVal(v, ve, ce) =>
  let
    val (ve, ty, b) = transVal env ve
    val tyenv = Var.Map.insert(tyenv, v, ty)
    val (ce, cty) = transCmp (tyenv, kindenv) ce
  in
    (makeBinds (b, LetVal(v, ve, ce)), cty)
  end

(*......................................................................*)
(* Moggi-val								*)
(*......................................................................*)
| Triv velist =>
  let
    val (velist, tys, b) = transVals env velist
  in
    (makeBinds (b, Triv velist), MILTy.noeffect tys)
  end

(*......................................................................*)
(* Encapsulated computation						*)
(*......................................................................*)
| Encap ce =>
  let
    val (ce, cty) = transCmp env ce
    val (eff,tys) = MILTy.fromCmp cty
  in
    (Encap ce, MILTy.cmp(Effect.allocs, tys))
  end


(*......................................................................*)
(* Sum elimination							*)
(*......................................................................*)
| Case(ve, true, cases, optdefault) =>
  let 
    val (ve, ty, b) = transVal env ve
    val tyss = 
      case MILTy.fromSum ty of
        SOME tyss => tyss
      | NONE => MILPretty.failVal ve "Share.transCmp: expected sum type"
    val (cases, optdefault, cty) = 
      transCases (fn i => List.nth(tyss, i)) (cases, optdefault)
  in
    if isUniSum' ty
    then
      let
        val cases' = map (fn (i, (vars, ce)) =>
          let
            val convar = Census.freshVar (length vars)
          in
            (i, ([convar], 
              Gen.foldri 
                (fn (i, x, ce) => LetVal(x, Proj(i, Var convar), ce))
                ce vars))
          end) cases
        in
          (makeBinds(b, Case(ve, false, cases', optdefault)), cty)
        end
    else
      (makeBinds(b, Case(ve, true, cases, optdefault)), cty)             
  end

(*......................................................................*)
(* Exn elimination							*)
(*......................................................................*)
| CaseExCon(ve, bindargs, cases, optdefault) =>
  let 
    val (ve, ty, b) = transVal env ve

    fun tysFor ty = if bindargs then MILTys.exnTys ty else [ty]

    val (cases, optdefault, cty) = transCases tysFor (cases, optdefault)
  in
    if bindargs then 
    let
      val cases' = map (fn (i, (vars, ce)) =>
      let
        val convar = Census.freshVar (length vars)
      in
        (i, ([convar], 
          Gen.foldri 
            (fn (i, x, ce) => LetVal(x, Proj(i, Var convar), ce))
            ce vars))
      end) cases
    in
      (makeBinds(b, CaseExCon(ve, false, cases', optdefault)), cty)
    end
    else
      (makeBinds(b, CaseExCon(ve, bindargs, cases, optdefault)), cty)
  end

(*......................................................................*)
(* Constant elimination							*)
(*......................................................................*)
| CaseSCon (ve, bindargs, cases, optdefault) =>
  let
    val (ve, ty, b) = transVal env ve
    val (cases, optdefault, cty) = transCases (fn _ =>[]) (cases,optdefault)
  in
    if MILTy.eq (ty, MILTy.java Types.BYTE)
    then
    let
      val x = Census.freshVar 1
    in 
      (makeBinds (b,Let(Java((Java.Cast, NONE, NONE), [ve],
            MILTy.cmp(Effect.none, [MILTy.java Types.BYTE])), 
            ([(x, MILTy.java Types.BYTE)], 
              CaseSCon(Var x, bindargs, 
               map (fn (c, abs) => (transCon c, abs)) cases,
              optdefault)))), cty)
    end
    else
      (makeBinds (b, CaseSCon(ve, bindargs, cases, optdefault)), cty)
  end

(*......................................................................*)
(* Exception raising							*)
(*......................................................................*)
| Throw(ve, tys, loc) =>
  let
    val (ve, _, b) = transVal env ve
  in
    (makeBinds (b, Throw(ve, tys, loc)), MILTy.cmp(Effect.throws, tys))
  end

(*......................................................................*)
(* Exception handling							*)
(*......................................................................*)
| TryLet(ce0, tabss, (vs, ce2)) =>
  let
    val (ce0, cty0) = transCmp env ce0
    val (ce2, cty2) = transCmp (Var.extend(tyenv, vs), kindenv) ce2
    fun transHandler ((typedvars, ce), (result, cty)) =
        let
          val (ce, cty') = 
            transCmp (Var.extend(tyenv, typedvars), kindenv) ce
        in
          ((typedvars, ce)::result, MILTy.unionCmpTypes(cty,cty'))
        end

    fun default () =   
      let
        val (tabss, cty1) = foldr transHandler ([], cty2) tabss
      in
        (TryLet(ce0, tabss, (vs, ce2)), MILTy.unionCmpTypes(cty0,cty1))
      end
  in

  case tabss of
    [([(exnvar, exnty)], CaseExCon(Var v, true, cases, SOME def))] =>
    if Var.eq(v, exnvar)
    then
    let
      val tyenv = Var.Map.insert(tyenv, v, exnty)
      fun transHandler ((exty, (vars, e)), (result,cty)) =
        let
          val tys = MILTys.exnTys exty
          val tyenv = Var.extend(tyenv, ListPair.zip(vars, tys))
          val thisexnvar = Census.freshVar (length vars + 1)
          val (e, cty') = transCmp (tyenv,kindenv) e
        in
            (([(thisexnvar, exty)], 
            Gen.foldri 
              (fn (i, var, e) => LetVal(var,Proj(i,Var thisexnvar), e))
              (LetVal(exnvar, Coerce(Var thisexnvar, MILTys.topExn), e)) 
              vars)::result, MILTy.unionCmpTypes(cty,cty'))
        end
      val (def, defcty) = transCmp (tyenv, kindenv) def
      val (handlers, cty1) = foldr transHandler ([], defcty) cases
      val extra = [([(exnvar, MILTys.topExn)], def)]    
    in
      Census.addVar(exnvar, ~1);
      let
        val extra = case def of
          Throw(Var v', _, _) => 
          if Var.eq(v,v') 
          then (Census.addVar(v,~1); [])
          else extra

          | _ => extra
      in
        (TryLet(ce0, handlers @ extra, (vs, ce2)), 
          MILTy.unionCmpTypes(MILTy.unionCmpTypes(cty0,cty1), cty2))
      end
    end
    else default ()

  | _ => default ()
 

  end

(*......................................................................*)
(* Conditional                                                          *)
(* When sharing types, turn an equality test on ML strings/intinfs into *)
(* a call to the "equals" method.                                       *)
(* Convert a test on ML Word8.word's into a normalised test.            *)
(*......................................................................*)
| Cond(t, ve1, ve2, ce1, ce2) =>
  let
    val (ve1, ty1, b1) = transVal env ve1
    val (ve2, ty2, b2) = transVal env ve2
    val (ce1, cty1) = transCmp env ce1
    val (ce2, cty2) = transCmp env ce2
    val cty = MILTy.unionCmpTypes(cty1,cty2)  
  in
    if t=MLEq andalso MILTy.eq (ty1, MILTy.java Types.BYTE)
    then
    let
      val x1 = Census.freshVar 1
      val x2 = Census.freshVar 1
    in
      (makeBinds (b1@b2, Let(Java((Java.Cast, NONE, NONE), [ve1],
            MILTy.cmp(Effect.none, [MILTy.java Types.BYTE])), 
            ([(x1, MILTy.java Types.BYTE)],
            Let(Java((Java.Cast, NONE, NONE), [ve2],
            MILTy.cmp(Effect.none, [MILTy.java Types.BYTE])), 
            ([(x2, MILTy.java Types.BYTE)],
            Cond(t, Var x1, Var x2, ce1, ce2)))))), cty)
    end
    else
      (makeBinds (b1@b2, Cond (t,ve1,ve2,ce1,ce2)), cty)
  end
  
(*......................................................................*)
(* Allocation       							*)
(*......................................................................*)
| Alloc (f,ve) =>
  let
    val (ve, ty, b) = transVal env ve
  in
    (makeBinds (b, Alloc(f,ve)), MILTy.cmp(Effect.allocs, [MILTy.refty ty]))
  end

(*......................................................................*)
(* Dereferencing							*)
(*......................................................................*)
| Deref ve =>
  let
    val (ve, refty, b) = transVal env ve
  in
    case MILTy.fromRefty refty of
      SOME ty => 
      (makeBinds (b,Deref ve), MILTy.cmp(Effect.reads, [ty]))

    | NONE => 
      MILPretty.failCmp ce "Share.transCmp: expected ref type"
  end

(*......................................................................*)
(* Assignment      							*)
(*......................................................................*)
| Assign(ve1, ve2) =>
  let
    val (ve1,_,b1) = transVal env ve1
    val (ve2,_,b2) = transVal env ve2
  in 
    (makeBinds (b1@b2, Assign(ve1,ve2)), MILTy.cmp(Effect.writes, []))
  end

(*......................................................................*)
(* Internal Java class definition					*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, ce) =>
  let
    fun transMethod (name, mods, tys, tyopt, optabs) =
        (name, mods, tys, tyopt,
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

    val (ce, cty) = transCmp env ce
    val methods = map transMethod methods
    val (eff, tys) = MILTy.fromCmp cty
  in
    (LetClass(classname, classinfo, fields, methods, ce), 
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

    fun transDef (f, g, (typedvars, ce), cty) =
    let
      val (ce, _) = 
        transCmp (Var.extend(defntyenv,typedvars), defnkindenv) ce
    in
      (f, g, (typedvars, ce), cty)
    end

    val recbinds = map transDef recbinds
    val (ce, cty) = transCmp (bodytyenv, kindenv) ce
  in
    (LetFun(tyvars, funkind, RecFun recbinds, ce), cty)
  end

(*......................................................................*)
(* Non-recursive functions.                                             *)
(*......................................................................*)
| LetFun(tyvars, funkind, Fun (f, (typedvars,e1)), e2) =>
  let
    (* First translate the body of the function *)
    val (e1, cty) = transCmp (Var.extend(tyenv, typedvars),
      Var.extend(kindenv, tyvars)) e1

    val tabs' = (typedvars, e1)

    val bodytyenv = 
      Var.Map.insert(tyenv, f, MILTy.forall(tyvars, 
        MILTy.arrow(map #2 typedvars,cty)))

    val (e2, cty) = transCmp (bodytyenv, kindenv) e2
  in
    (LetFun(tyvars, funkind, Fun (f, tabs'), e2), cty)
  end

| _ =>
  MILPretty.failCmp ce "Share.transCmp: illegal term"
  


end

val _ = Counters.reset ()
val (ce,cty) = transCmp (tyenv, Var.Map.empty) ce

in
  Counters.printCounts (); ce
end

end (* of local open *)
end (* of struct *)

