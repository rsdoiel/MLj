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
(* Global free variable counts for `current' term.			*)
(* Assumption 1: bound variables in the term are distinct.              *)
(* Assumption 2: the census is used in a single threaded way; there's   *)
(* only one census so no backtracking is allowed.                       *)
(*======================================================================*)
structure Census :> CENSUS = 
struct

local 
  open MILTerm
in

(*----------------------------------------------------------------------*)
(* A census is a count of free occurrences of each variable.		*)
(*----------------------------------------------------------------------*)
type Census = DynIntArray.array

(* First unused variable in the census *)
val supply = ref Var.initial

(* Start of free list; zero indicates no free spaces *)
val free = ref 0

(* The census array itself *)
val census = DynIntArray.array 0

(*----------------------------------------------------------------------*)
(* Add n to the census count of variable x.				*)
(*----------------------------------------------------------------------*)
fun inc(x,n) = 
  DynIntArray.update(census, x, DynIntArray.sub(census, x) + n)

(*----------------------------------------------------------------------*)
(* Add n to the census counts for variables in value term v.       	*)
(*----------------------------------------------------------------------*)
fun censusVal (n,v) = 
case v of
  Var x => 
  inc(x,n)

| (SCon _ | Null _ | Closure _) => 
  ()

| (Inj(_, _, vs) | ExCon(_,vs) | Tuple vs) => censusValList (n,vs)

| (Unfold v | Fold(v,_) | Proj(_,v) | Coerce(v,_) | TApp(v,_) | TAbs(_,v)) => 
  censusVal (n,v)

(*----------------------------------------------------------------------*)
(* Add n to the census counts for variables in computation term v.     	*)
(*----------------------------------------------------------------------*)
and censusCmp (n,e) =
let
  fun censusCases (v, bindargs, cases, eopt) =
    (censusVal (n,v);
     (case eopt of NONE => () | SOME e => censusCmp (n,e));
     app (fn (i,(xs,body)) => censusCmp (n,body)) cases)
in
case e of

  App(v, vs) =>
  (censusVal (n,v); censusValList (n,vs))

| Java(j, vs, cty) => 
  censusValList (n,vs)

| Let(e1, (xs,e2)) => 
  (censusCmp (n,e1); censusCmp (n,e2))

| Triv vs => censusValList (n,vs)
| Case cases => censusCases cases
| CaseSCon cases => censusCases cases
| CaseExCon cases => censusCases cases
| Cond(t, v1, v2, e1, e2) => 
  (censusVal (n,v1); censusVal (n,v2); censusCmp (n,e1); censusCmp (n,e2))
| TryLet(e, tabss, (_,body)) =>
  (censusCmp (n,e);
  app (fn tabs => censusTAbstr (n,tabs)) tabss;
  censusCmp (n,body))
| LetFun(tyvars, kind, def, e) => 
  (censusCmp (n,e);
  case def of
    Fun (_,(_,e)) => censusCmp (n,e)
  | RecFun defs => app (fn (_,_,(_,e),_) => censusCmp (n,e)) defs)
| LetClass(classname,info,fields,methods,e) =>
  (censusCmp (n,e); 
  app (fn (_,_,_,_,SOME (_,(_,e))) => censusCmp (n,e)
        | _ => ()) methods)

| (Alloc (_,v) | Deref v | Throw(v,_,_)) => censusVal (n,v)
| Assign(v1, v2) => (censusVal (n,v1); censusVal (n,v2))
| LetVal(x, v, e) => (censusVal (n,v); censusCmp (n,e))
| Encap e => censusCmp (n,e)
| Init(x,i,v,e) => (censusVal(n,v); censusCmp(n,e); inc(x,n))

end

(*----------------------------------------------------------------------*)
(* Add n to the census counts for variables in value terms vs.       	*)
(*----------------------------------------------------------------------*)
and censusValList (n,vs) = app (fn v => censusVal (n,v)) vs

(*----------------------------------------------------------------------*)
(* Add n to the census counts for variables in a typed abstraction.    	*)
(*----------------------------------------------------------------------*)
and censusTAbstr (n, (xs, body)) = censusCmp (n,body)

fun maxVar () = !supply

fun clearCensus () = (DynIntArray.clear census; free := 0)

fun freshVar n =
if !free = 0
then 
let
  val v = !supply
in
  DynIntArray.update(census, v, n);
  supply := v+1;
  v
end
else 
let
  val v = !free
in
  free := DynIntArray.sub(census, v);
  DynIntArray.update(census, v, n);
  v
end

fun addCmp (ce,0) = ()
  | addCmp (ce,n) = censusCmp (n, ce)

fun addVal (ve,0) = ()
  | addVal (ve,n) = censusVal (n, ve)

fun addVar (v,0) = ()
  | addVar (v,n) = inc (v, n)

fun initCensus (e, s) = 
  (supply := s; free := 0; clearCensus (); addCmp (e,1))

(*----------------------------------------------------------------------*)
(* Mark a variable as inlined.						*)
(*----------------------------------------------------------------------*)
fun inlineVar x = DynIntArray.update(census, x, ~1)

(*----------------------------------------------------------------------*)
(* Remove a variable from the census; it must not appear in the term,	*)
(* even as a bound variable.                                            *)
(*----------------------------------------------------------------------*)
fun removeVar x = 
if x=0 then () else
(
  DynIntArray.update(census, x, !free);
  free := x
)

(*----------------------------------------------------------------------*)
(* Remove all bound variables from the census.                          *)
(* Note that we must kill the bound variables *after* traversing the    *)
(* term in which they occur free.                                       *)
(*----------------------------------------------------------------------*)
fun removeCmp e =
let
  fun removeCases ((v, bindargs, cases, eopt) : 'a Cases) =
    (censusVal(~1,v); 
     (case eopt of NONE => () | SOME e => removeCmp e);
     app (removeAbstr o #2) cases)
in
case e of
  Let(e, tabs) =>
  (removeCmp e; removeTAbstr tabs)
| Case cases => removeCases cases
| CaseSCon cases => removeCases cases
| CaseExCon cases => removeCases cases
| Cond(t, v1, v2, e1, e2) => 
  (censusVal(~1,v1); censusVal(~1,v2); removeCmp e1; removeCmp e2)
| TryLet(e, tabss, tabs) =>
  (removeCmp e; app removeTAbstr tabss; removeTAbstr tabs)
| LetFun(tyvars, kind, def, e) => 
  (removeCmp e;
  case def of
    Fun (f,tabs) => (removeTAbstr tabs; removeVar f)
  | RecFun defs => 
    (app (removeTAbstr o #3) defs;
     app (fn (f,g,_,_) => (removeVar f; removeVar g)) defs)
  )
| LetClass(classname,info,fields,methods,e) =>
  (removeCmp e;
  app (fn (_,_,_,_,SOME (f,abs)) => (removeAbstr abs; removeVar f)
        | _ => ()) methods)

| LetVal(x, v, e) => (censusVal(~1,v); removeCmp e; removeVar x)
| Encap e => removeCmp e
| Init(x,i,v,e) => (inc(x, ~1); censusVal(~1,v); removeCmp e)
| e => censusCmp(~1, e)

end

and removeTAbstr (xs,e) = (removeCmp e; app (removeVar o #1) xs)
and removeAbstr (xs,e) = (removeCmp e; app removeVar xs)

fun getVar x = DynIntArray.sub(census, x)

(*----------------------------------------------------------------------*)
(* Rename all bound variables in a term					*)
(* Don't change the dummy ones!                                         *)
(* Also add the new (and unchanged) variables into the census.          *)
(*----------------------------------------------------------------------*)
fun freshVars (r, vs) =
  foldr (fn (v, (r,vs)) => 
    if v=0 then (r, v::vs)
    else
      let val v' = freshVar 0
      in (Var.Map.insert(r, v, v'), v'::vs) end)
    (r,[]) vs
  
fun freshTypedVars (r, vs) =
  foldr (fn ((v,ty), (r,vs)) => 
    if v=0 then (r, (v,ty)::vs)
    else
      let val v' = freshVar 0 
      in (Var.Map.insert(r, v, v'), (v',ty)::vs) end)
    (r,[]) vs

fun renameVar r v =    
  (case Var.Map.find(r, v) of
    SOME v' => 
    (inc (v', 1); v')

  | NONE => 
    (inc (v, 1); v)
  )

fun renameVal r ve =
let 
  val rc = renameCmp r
  val rv = renameVal r
in
case ve of
  Var v =>  
  Var (renameVar r v)

| SCon _ => 
  ve

| Inj(ty, i, ves) => 
  Inj(ty, i, map rv ves)

| ExCon(excon, ves) => 
  ExCon(excon, map rv ves)

| Tuple ves => 
  Tuple(map rv ves)

| Proj(i, ve) => 
  Proj(i, rv ve)

| TApp(ve, tys) => 
  TApp(rv ve, tys)

| TAbs(tyvars, ve) => 
  TAbs(tyvars, rv ve)

| Coerce(ve, ty) => 
  Coerce(rv ve, ty)

| Fold(ve, ty) => 
  Fold(rv ve, ty)

| Unfold ve => 
  Unfold(rv ve)

| Null _ =>
  ve

| Closure _ =>
  ve

end

and renameTAbstr r (vs,ce) =
  let val (r, vs) = freshTypedVars (r,vs)
  in
    (vs, renameCmp r ce)
  end

and renameAbstr r (vs,ce) =
  let val (r, vs) = freshVars (r,vs)
  in
    (vs, renameCmp r ce)
  end

and renameCmp r ce =
let 
  val rc = renameCmp r
  val rv = renameVal r
  fun renameCase (i, abs) = (i, renameAbstr r abs)
    
  fun renameCases (ve, bindargs, cases, ceopt) =
    (rv ve, bindargs, map renameCase cases, Option.map rc ceopt)

  fun renameLetFun (tyvars, kind, Fun (f, tabs), ce) =
      let
        val f' = freshVar 0
        val r' = Var.Map.insert(r, f, f')
      in
        (tyvars, kind, Fun (f', renameTAbstr r tabs), renameCmp r' ce)
      end

      (* Allow the source term to have identical external and internal fvars *)
    | renameLetFun (tyvars, kind, RecFun defs, ce) =
      let
        fun rename1 ((f, g, tabs, cty), (rin,rout,result)) =
        let
          val f' = freshVar 0
          val g' = freshVar 0
        in
          (Var.Map.insert(rin, g, g'), Var.Map.insert(rout, f, f'),
            (f', g', tabs, cty)::result)
        end
        val (rin,rout,defs) = foldr rename1 (r, r, []) defs
      in
        (tyvars, kind, RecFun (map (fn (f, g, tabs, cty) => 
            (f, g, renameTAbstr rin tabs, cty)) defs), renameCmp rout ce)
      end

in
case ce of
  App(ve, ves) => 
  App(rv ve, map rv ves)

| Java(jop, ves, cty) => 
  Java(jop, map rv ves, cty)

| Let(ce, tabs) => 
  Let(rc ce, renameTAbstr r tabs)

| Triv ves => 
  Triv(map rv ves)

| Case cases => 
  Case(renameCases cases)

| CaseSCon cases => 
  CaseSCon(renameCases cases)

| CaseExCon cases => 
  CaseExCon(renameCases cases)

| Cond(t, ve1, ve2, ce1, ce2) => 
  Cond(t, rv ve1, rv ve2, rc ce1, rc ce2)

| Throw(ve, tys, loc) =>
  Throw(rv ve, tys, loc)

| TryLet(ce, tabss, tabs) =>
  TryLet(rc ce, map (renameTAbstr r) tabss, renameTAbstr r tabs)

| LetFun args =>
  LetFun (renameLetFun args)

| LetClass(classname, classinfo, fields, methods, ce) =>
  let
    val methods = map (fn (arg as (s, m, tys, tyopt, absopt)) => 
    case absopt of
      NONE => 
      arg

    | SOME (f,abs) => 
      let
        val f' = freshVar 0
      in
        (s, m, tys, tyopt, SOME (f', renameAbstr r abs))
      end)
     methods
  in
    LetClass(classname, classinfo, fields, methods, rc ce)
  end

| Alloc (f,ve) => 
  Alloc (f, rv ve)

| Deref ve => 
  Deref (rv ve)

| Assign(ve1,ve2) => 
  Assign(rv ve1, rv ve2)

| LetVal(v, ve, ce) =>
  let
    val v' = freshVar 0
  in
    LetVal(v', rv ve, renameCmp (Var.Map.insert(r, v, v')) ce)
  end

| Encap e =>
  Encap (rc e)

| Init(x,i,v,e) =>
  Init(renameVar r x, i, rv v, rc e)


end (* of let *)


val renameCmp = renameCmp Var.Map.empty
val renameTAbstr = renameTAbstr Var.Map.empty

(*----------------------------------------------------------------------*)
(* Check that the counts for variables in e are correct.	        *)
(*----------------------------------------------------------------------*)
fun checkCmp e = 
  let
    (* Trash the free list so that it doesn't confuse the check *)
    fun gather (0,result) = result
      | gather (x,result) = 
        let
          val old = DynIntArray.sub(census, x)
        in
          DynIntArray.update(census, x, 0);
          gather(old, x::result)
        end

    val frees = gather (!free, [])

    (* Restore the free list later *)
    fun restore (y,[]) = 
        ()

      | restore (y,x::xs) = 
        (DynIntArray.update(census, y, x); restore (x, xs))
      
    fun check x = 
      if x < 0 then ()
      else      
      (if DynIntArray.sub(census, x) <> 0
      then Debug.print ("\nCensus incorrect by " ^ 
        Int.toString (DynIntArray.sub(census, x)) ^ " for variable " ^ 
        Pretty.indexToString x)
      else (); 
      check (x-1))
    
    val n = !supply

    fun total (x,result) =
      if x < 0 
      then Debug.print (
        "Census: " ^ Int.toString result ^ 
        " used from " ^ Int.toString (n+1) ^ 
        " including " ^ Int.toString (length frees) ^ " free.\n")
      else
      if DynIntArray.sub(census, x) <> 0 
      then total(x-1,result+1)
      else total(x-1,result)
  in
    addCmp (e, ~1);  
    check n;
    addCmp (e, 1);
    total (n,0);
    (case frees of [] => ()
                 | x::xs => (restore (x,xs); free := x))
  end

end (* of local open *) 

end (* of struct *)

