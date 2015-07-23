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
(* Various useful operations on MIL terms				*)
(*======================================================================*)
structure MILTermOps :> MILTERMOPS =
struct


local open MILTerm Gen in

(*----------------------------------------------------------------------*)
(* Make a type abstraction term.                                        *)
(*----------------------------------------------------------------------*)
fun tabs (tyvars, ve) =
  if null tyvars then ve else TAbs(tyvars, ve)

(*----------------------------------------------------------------------*)
(* Make a type application term.					*)
(*----------------------------------------------------------------------*)
fun tapp (ve, tys) =
  if null tys then ve else TApp(ve, tys)

fun isLocal LocalFun = true
  | isLocal _ = false

val trueVal = Inj(MILTys.bool, 1, [])
val falseVal = Inj(MILTys.bool, 0, [])

(*----------------------------------------------------------------------*)
(* Does a type variable occur free in a value/computation term?       	*)
(*----------------------------------------------------------------------*)
fun occurs n ty = Var.Set.member(MILTy.tyvars ty, n)

fun tyVarOccursVal n ve = 
case ve of
  Inj(ty, i, args) => occurs n ty orelse tyVarOccursValList n args
| ExCon(excon, args) => tyVarOccursValList n args
| Tuple velist => tyVarOccursValList n velist
| Proj(i, ve) => tyVarOccursVal n ve
| TApp(ve, tys) => tyVarOccursVal n ve 
  orelse List.exists (occurs n) tys
| TAbs(Ks, ve) => tyVarOccursVal n ve
| Coerce (ve, ty) => 
  tyVarOccursVal n ve orelse occurs n ty
| Fold (ve, ty) => 
  tyVarOccursVal n ve orelse occurs n ty
| Unfold ve => tyVarOccursVal n ve
| Null ty => occurs n ty
| _ => false

and tyVarOccursCmp n ce =
let
  fun tyVarOccursCases (ve, bindargs, cases, ceopt) =
  tyVarOccursVal n ve 
  orelse List.exists (fn (i,abs) => tyVarOccursAbstr n abs) cases
  orelse (case ceopt of NONE => false | SOME ce => tyVarOccursCmp n ce)

in
case ce of
  App(ve, velist) => tyVarOccursVal n ve orelse tyVarOccursValList n velist
| Java(j, velist, cty) => tyVarOccursValList n velist
| Let(ce, tabs) => tyVarOccursCmp n ce orelse tyVarOccursTAbstr n tabs
| Triv velist => tyVarOccursValList n velist
| Case cases => tyVarOccursCases cases
| CaseSCon cases => tyVarOccursCases cases
| CaseExCon cases => tyVarOccursCases cases
| Cond(t,ve1,ve2,ce1,ce2) => 
  tyVarOccursVal n ve1 orelse tyVarOccursVal n ve2
  orelse tyVarOccursCmp n ce1 orelse tyVarOccursCmp n ce2
| Throw(ve, tys, loc) =>
  tyVarOccursVal n ve orelse List.exists (occurs n) tys
| TryLet(ce, tabss, tabs) => 
  tyVarOccursCmp n ce orelse List.exists (tyVarOccursTAbstr n) tabss
  orelse tyVarOccursTAbstr n tabs
| LetFun(tyvars, kind, defs, ce) => 
  tyVarOccursCmp n ce orelse tyVarOccursFunDef n defs 
| LetClass(_,_,_,_,ce) =>
  tyVarOccursCmp n ce
| Deref ve => tyVarOccursVal n ve
| Alloc (f,ve) => tyVarOccursVal n ve
| Assign(ve1,ve2) => tyVarOccursVal n ve1 orelse tyVarOccursVal n ve2
| Encap e => tyVarOccursCmp n e
| LetVal(v,ve,ce) => tyVarOccursVal n ve orelse tyVarOccursCmp n ce
end

and tyVarOccursValList n velist = List.exists (tyVarOccursVal n) velist
and tyVarOccursTAbstr n (vs, body) = 
  tyVarOccursCmp n body orelse List.exists (occurs n) (map #2 vs)
and tyVarOccursAbstr n (vs, body) = tyVarOccursCmp n body

and tyVarOccursFunDef n (Fun(_,tabs)) = tyVarOccursTAbstr n tabs
  | tyVarOccursFunDef n (RecFun defs) =  
    List.exists (tyVarOccursTAbstr n) (map #3 defs)

(*----------------------------------------------------------------------*)
(* Instantiate type variables?                                       	*)
(*----------------------------------------------------------------------*)
fun substVal S v =
let
  val st = MILTy.subst S
  val sv = substVal S
in
case v of
  Inj(ty, i, args) => Inj(st ty, i, map sv args)
| ExCon(excon, args) => ExCon(excon, map sv args)
| Tuple args => Tuple (map sv args)
| Proj(i, v) => Proj(i, sv v)
| TApp(v, tys) => TApp(sv v, map st tys)
| TAbs(tyvars, v) => TAbs(tyvars, sv v)
| Coerce (v, ty) => Coerce(sv v, st ty)
| Fold (v, ty) => Fold(sv v, st ty)
| Unfold v => Unfold(sv v)
| Null ty => Null(st ty)
| Closure(i, vs) => Closure(i, map sv vs)
| (SCon _ | Var _) => v
end

and substCmp S e =
let
  val st = MILTy.subst S
  fun sct cty = 
  let val (eff, tys) = MILTy.fromCmp cty
  in MILTy.cmp(eff, map st tys) end

  val sv = substVal S
  val sc = substCmp S

  fun instCases (v, bindargs, cases, eopt) = (sv v, bindargs, 
    map (fn (i, (vars, e)) => (i, (vars, sc e))) cases, 
    Option.map sc eopt)

  fun instTAbstr (typedvars, e) = 
    (map (fn (var,ty) => (var, st ty)) typedvars, sc e)

in
case e of
  App(v, vs) => App(sv v, map sv vs)
| Java(j, vs, cty) => Java(j, map sv vs, sct cty)
| Let(e1, tabs) => Let(sc e1, instTAbstr tabs)
| Triv vs => Triv (map sv vs)
| Case cases => Case(instCases cases)
| CaseSCon cases => CaseSCon(instCases cases)
| CaseExCon cases => CaseExCon(instCases cases)
| Cond(t,v1,v2,e1,e2) =>Cond(t, sv v1, sv v2, sc e1, sc e2)
| Throw(v, tys, loc) => Throw(sv v, map st tys, loc)
| TryLet(e1, handlers, tabs) => TryLet(sc e1, map instTAbstr handlers, 
  instTAbstr tabs)
| LetFun(tyvars, kind, def, e) => 
  LetFun(tyvars, kind, 
    case def of
      Fun (f, tabs) => Fun (f, instTAbstr tabs)
    | RecFun defs => RecFun (
      map (fn (f,g,tabs,cty) => (f,g,instTAbstr tabs,sct cty)) defs),
    sc e)
| LetClass(class,info,fields,methods,e) => 
  LetClass(class,info,fields,methods,sc e)
| Deref v => Deref(sv v)
| Alloc (f,v) => Alloc(f, sv v)
| Assign(v1,v2) => Assign(sv v1, sv v2)
| LetVal(var, v, e) => LetVal(var, sv v, sc e)
| Encap e => Encap(sc e)
end

(*----------------------------------------------------------------------*)
(* Is the size of ve larger than n?        				*)
(*----------------------------------------------------------------------*)
fun sizeVal n ve =
  if n < 0 then n
  else
  (case ve of
    Var _ => n
  | SCon _ => n-1
  | Inj (_,_,ves) => sizeVals (n-1) ves
  | Coerce(ve,_) => sizeVal (n-1) ve
  | ExCon (_,ves) => sizeVals (n-1) ves
  | Tuple ves => sizeVals (n-1) ves
  | Proj(i, ve) => sizeVal (n-1) ve
  | TApp(ve,_) => sizeVal n ve
  | TAbs(_,ve) => sizeVal n ve
  | Fold(ve,_) => sizeVal n ve
  | Unfold ve => sizeVal n ve
  | Null _ => n-1)

and sizeVals n [] = n
  | sizeVals n (ve::ves) =
    let
      val n' = sizeVal n ve
    in
      if n' < 0 then n' else sizeVals n' ves
    end

and sizeCmp n ce =
let
  fun sizeCases (ve, bindargs, cases, ceopt) =
    let
      val n' = sizeVal (n-1) ve
      fun loop n [] = 
          (if n < 0 then n else 
          case ceopt of
            SOME ce => sizeCmp n ce
          | NONE => n)

        | loop n ((i,(_,ce))::rest) =
          let
            val n' = sizeCmp n ce
          in
            if n' < 0 then n' else loop n' rest
          end       
    in
      if n' < 0 then n' else loop n' cases
    end
in
  if n < 0 then n
  else
  (case ce of
    App(ve,ves) => sizeVals (n-1) (ve::ves)
  | Java(_,ves,_) => sizeVals (n-1) ves
  | Let(ce1,(_,ce2)) => sizeCmps n [ce1,ce2]
  | Triv ves => sizeVals n ves
  | Case cases => sizeCases cases
  | CaseSCon cases => sizeCases cases
  | CaseExCon cases => sizeCases cases
  | Cond(_,ve1,ve2,ce1,ce2) => 
    let
      val n' = sizeVals (n-1) [ve1,ve2]
    in
      if n' < 0 then n'
      else sizeCmps n' [ce1,ce2]
    end
  | Throw(ve,_,_) => sizeVal (n-1) ve
  | TryLet(ce1, tabss, (_,ce3)) => sizeCmps (n-1) ([ce1,ce3] @ (map #2 tabss))
  | LetFun(_,kind,def,ce) => if not (isLocal kind) then ~1
    else
    let
      fun loop n [] = sizeCmp n ce
        | loop n ((_,_,(_,ce),_)::rest) =
          let
            val n' = sizeCmp n ce
          in
            if n' < 0 then n' else loop n' rest
          end
    in
      case def of
        Fun (_, (_,ce')) => sizeCmps n [ce,ce']
      | RecFun defs => loop n defs
    end
  | LetClass(_,_,fields,methods,ce) => ~1
  | Alloc (f,ve) => sizeVal (n-1) ve
  | Deref ve => sizeVal (n-1) ve
  | Assign(ve1,ve2) => sizeVals (n-1) [ve1,ve2]
  | Encap e => sizeCmp (n-1) e
  | Init(x,i,v,e) =>
    let
      val n' = sizeVal n v
    in
      if n' < 0 then n'
      else sizeCmp n' e
    end
  | LetVal(v, ve, ce) => 
    let
      val n' = sizeVal n ve
    in
      if n' < 0 then n'
      else sizeCmp n' ce
    end)
end

and sizeCmps n [] = n
  | sizeCmps n (ce::ces) =
    let
      val n' = sizeCmp n ce
    in
      if n' < 0 then n' else sizeCmps n' ces
    end

fun valBigger (ve,n) = sizeVal n ve < 0
fun cmpBigger (ce,n) = sizeCmp n ce < 0


(*----------------------------------------------------------------------*)
(* Is the size of ve larger than n?        				*)
(*----------------------------------------------------------------------*)
fun absSizeVal ve =
  case ve of
    Var _ => 0
  | SCon _ => 1
  | Inj (_,_,ves) => 1 + absSizeVals ves
  | Coerce(ve,_) => 1 + absSizeVal ve
  | ExCon (_,ves) => 1 + absSizeVals ves
  | Tuple ves => 1 + absSizeVals ves
  | Proj(i, ve) => 1 + absSizeVal ve
  | TApp(ve,_) => absSizeVal ve
  | TAbs(_,ve) => absSizeVal ve
  | Fold(ve,_) => absSizeVal ve
  | Unfold ve => absSizeVal ve
  | Null _ => 1

and absSizeVals [] = 0
  | absSizeVals (ve::ves) = absSizeVal ve + absSizeVals ves

and absSizeCmp ce =
let
  fun sizeCases (ve, bindargs, cases, ceopt) =
    1 + absSizeVal ve + (case ceopt of NONE => 0 | SOME e => absSizeCmp e) +
    foldr op+ 0 (map (fn (i,(_,e)) => absSizeCmp e) cases)
in
  case ce of
    App(ve,ves) => 1 + absSizeVal ve + absSizeVals ves
  | Java(_,ves,_) => 1 + absSizeVals ves
  | Let(ce1,(_,ce2)) => absSizeCmp ce1 + absSizeCmp ce2
  | Triv ves => absSizeVals ves
  | Case cases => sizeCases cases
  | CaseSCon cases => sizeCases cases
  | CaseExCon cases => sizeCases cases
  | Cond(_,ve1,ve2,ce1,ce2) => 
    absSizeVal ve1 + absSizeVal ve2 + absSizeCmp ce1 + absSizeCmp ce2
  | Throw(ve,_,_) => 1 + absSizeVal ve
  | TryLet(ce1, tabss, (_,ce3)) => 
    1 + absSizeCmp ce1 + absSizeCmp ce3 +
    foldr op+ 0 (map (absSizeCmp o #2) tabss)
  | LetFun(_,kind,def,ce) => 
    absSizeCmp ce + 
    (case def of
        Fun (_, (_,ce')) => absSizeCmp ce'
      | RecFun defs => foldr op+ 0 (map (absSizeCmp o #2 o #3) defs)
    )
  | LetClass(_,_,fields,methods,ce) =>
    absSizeCmp ce
  | Alloc (f,ve) => 1 + absSizeVal ve
  | Deref ve => 1 + absSizeVal ve
  | Assign(ve1,ve2) => 1 + absSizeVal ve1 + absSizeVal ve2
  | Encap e => absSizeCmp e
  | Init(x,i,v,e) => 1 + absSizeVal v + absSizeCmp e
  | LetVal(v, ve, ce) => absSizeVal ve + absSizeCmp ce
end

val absSize = absSizeCmp

(*----------------------------------------------------------------------*)
(* Are two values (semantically) equal?					*)
(*   SOME true    => yes                                                *)
(*   SOME false   => no                                                 *)
(*   NONE         => don't know                                         *)
(*----------------------------------------------------------------------*)
fun valEq (v1,v2) =
case (v1,v2) of
  (Var x, Var y) => 
  if Var.eq(x,y) then SOME true else NONE

| (Var _, _) => 
  NONE

| (_, Var _) => 
  NONE

| (SCon(ty1,c1), SCon(ty2,c2)) => 
  SOME (Constants.equal(c1,c2,true))

| (Inj(ty1,i1,vs1), Inj(ty2,i2,vs2)) =>
  if i1<>i2 then SOME false 
  else valsEq (vs1,vs2)

| (Tuple vs1, Tuple vs2) =>
  valsEq (vs1, vs2)

| (Fold(v1,ty1), Fold(v2,ty2)) =>
  valEq (v1,v2)

| (Unfold v1, Unfold v2) =>
  valEq (v1,v2)

| (Null ty1, Null ty2) =>
  SOME true

| _ =>
  NONE

and valsEq ([],[]) = SOME true
  | valsEq (x::xs,y::ys) = 
    (case valEq (x,y) of
      SOME false => SOME false
    | SOME true => valsEq (xs,ys)
    | NONE => 
      case valsEq (xs,ys) of
        NONE => NONE
      | SOME false => SOME false
      | SOME true => NONE)
  | valsEq _ = SOME false

(*----------------------------------------------------------------------*)
(* Idea: e is the body of a (possibly recursive) function that will be  *)
(* `localised' but which requires its `return' to be altered to a       *)
(* continuation function (contfunvar) application. Hence its return     *)
(* type is also changed. So, replace:                                   *)
(*   val vs    by f(vs);                                                *)
(* [v not local function variable]                                      *)
(*   v vs      by let xs <= v vs in f(xs);                              *)
(*             likewise for alloc v, !v, v1:=v2, new, java ops, throw   *)
(* Any local blocks used inside e must also be declared inside e.       *)
(*----------------------------------------------------------------------*)
fun addContinuation (locals,contfunvar,cty) e =
let
  fun addCont locals e =
  let
    fun addContCases (v, bindargs, cases, default) =
      let
        val default = Option.map (addCont locals) default
        val cases = map (fn (i, (xs,e)) => (i, (xs, addCont locals e))) cases
      in
        (v, bindargs, cases, default)
      end

    val (_,restys) = MILTy.fromCmp cty

    fun default () =
      let
        val xs = map (fn ty => (Census.freshVar 1, ty)) restys
      in
        Census.addVar(contfunvar, 1);
        Let(e, (xs, App(Var contfunvar, map (Var o #1) xs)))
      end
  in
    case e of
      Let(e1, (vars, e2)) =>
      Let(e1, (vars, addCont locals e2))

    | Triv vs =>
      (Census.addVar(contfunvar,1);
      App(Var contfunvar, vs))

    | Case cases => 
      Case (addContCases cases)

    | CaseSCon cases =>
      CaseSCon (addContCases cases) 
  
    | CaseExCon cases =>
      CaseExCon (addContCases cases) 

    | Cond(t,v1,v2,e1,e2) =>
      Cond(t,v1,v2,addCont locals e1,addCont locals e2)

    | TryLet(e1,handlers,(vars,e2)) =>
      TryLet(e1,handlers,(vars,addCont locals e2))

    | LetFun(tyvars,kind,def,e) => 
      if isLocal kind
      then
      let
        val (def, locals) = 
        case def of
          RecFun defs => 
          let
            val locals' = Var.Set.addList(locals, map #2 defs)
          in
            (RecFun (map (fn (f,g,(typedvars,e),_) => 
                             (f,g,(typedvars,addCont locals' e),cty)) defs),
            Var.Set.addList(locals, map #1 defs))
          end

        | Fun (f, (xs, e)) =>
          (Fun (f, (xs, addCont locals e)), Var.Set.add(locals, f))
      in
        LetFun(tyvars, kind, def, addCont locals e)
      end
      else LetFun(tyvars,kind, def, addCont locals e)

    | LetClass(classname,info,fields,methods,e) =>
      LetClass(classname,info,fields,methods,addCont locals e)
  
    | LetVal(var,v,e) =>
      LetVal(var,v,addCont locals e)

    | App(Var f, vs) =>
      if Var.Set.member(locals, f) then e else default ()
 
    | Init(x, i, v, e) =>
      Init(x, i, v, addCont locals e)

    | _ =>  
      default ()
  end
in
  addCont locals e
end

end (* of local open MILTerm *)

end (* of struct *)


