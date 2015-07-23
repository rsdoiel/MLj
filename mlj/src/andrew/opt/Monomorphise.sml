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
(* Monomorphise a MIL term.                 				*)
(* Just for an experiment, we use refs instead of a state monad.        *)
(*======================================================================*)
structure Monomorphise :> TRANSFORMER =
struct

local 
  open MILTerm 
in

(*----------------------------------------------------------------------*)
(* A specialisation is a map from variables to information about the    *)
(* types at which the variable is used.                                 *)
(*----------------------------------------------------------------------*)
type ValSpecInfo =
{
  (* Set if var appears uninstantiated *)
  uninstantiated : bool,      
 
  funs : int,

  (* Instantiations: [(tyss_1, x_1), ..., (tyss_n, x_n)] *)
  (* An element ([ty_1,...,ty_m],x) of this list represents the replacement *)
  (* of some term x [ty_1',...,ty_m'] for ty_1' <= ty_1, ..., ty_m' <= ty_m *)
  (* by x_i [ty_1',...,ty_m'] *)
  instances : (MILTy.Type list * Var.Var list) list
}
 
type ValSpec = (int*ValSpecInfo ref) Var.Map.map

(*----------------------------------------------------------------------*)
(* Monomorphise the (necessarily closed) computation term e.        	*)
(* The type environment tyenv is ignored -- it should be empty.         *)
(*----------------------------------------------------------------------*)
fun transform (tyenv) e = 
let

val spec = ref (Var.Map.empty : ValSpec)

(*----------------------------------------------------------------------*)
(* Given a value term x {tys} replace by its specialised variable x',   *)
(* adding to !spec if necessary.                                        *)
(*----------------------------------------------------------------------*)
fun applySpec kindenv (x,tys) =
  case Var.Map.find(!spec, x) of
    NONE => 
    Debug.fail ("Monomorphise.applySpec: cannot find variable " ^ 
    Var.toString x)
 
  | SOME (i, r as ref { uninstantiated, instances, funs }) =>
    let
      val tys = map (MILTy.forceBounds kindenv) tys
      fun find ([],_) = 
          let 
            fun gen 0 = []
              | gen n = Census.freshVar 0 :: gen (n-1)
            val xs' = gen funs        
          in
            r := { uninstantiated = uninstantiated, funs = funs,
                   instances = (tys, xs')::instances };
            List.nth(xs', i)
          end

        | find ((inst as (tys',xs'))::rest, prefix) =
          let
            fun find' ([],[],result) = 
                (r := { uninstantiated = uninstantiated, funs = funs,
                        instances = (rev result, xs')::rest @ prefix };
                 List.nth(xs',i))

              | find' (typar::typars, ty::tys, result) =
                case MILTy.lub (ty,typar) of
                  NONE => find (rest, inst::prefix)
                | SOME ty' => 
                  find' (typars, tys, ty'::result)
          in
            find' (tys,tys',[])
          end

      val x' = find (instances, [])
    in
      if Controls.isOn "showSpecs"
      then Debug.print ("\n" ^ Var.toString x ^ " {" ^ Pretty.simpleVec ","
        MILTy.toString tys ^ "} |-> " ^ Var.toString x') else ();
      x'
    end

(*----------------------------------------------------------------------*)
(* Add polymorphic variables xs to !spec, initially with no instances.	*)
(*----------------------------------------------------------------------*)
fun initInstances xs =
    let 
      val info = ref { uninstantiated = false, instances=[], funs = length xs }
      fun loop ([], i, spec) = spec
        | loop (x::xs, i, spec) = 
          loop (xs, i+1, Var.Map.insert(spec, x, (i, info)))
    in
      spec := loop (xs, 0, !spec)
    end

(*----------------------------------------------------------------------*)
(* Add an uninstantiated polymorphic variable x to !spec.		*)
(*----------------------------------------------------------------------*)
fun addUninst x =
  case Var.Map.find(!spec, x) of
    SOME (i, r as ref { uninstantiated = false, instances, funs }) => 
    r := { uninstantiated = true, instances = instances, funs = funs }

  | _ => ()

(*----------------------------------------------------------------------*)
(* Get the type instances at which a variable x is used.		*)
(*----------------------------------------------------------------------*)
fun getInstances x =
  case Var.Map.find(!spec, x) of
    NONE => 
    Debug.fail ("Monomorphise.getInstances: cannot find variable " ^ 
      Var.toString x)
  | SOME (i, ref p) => p

(*----------------------------------------------------------------------*)
(* Environment passed around monomorphisation functions			*)
(*----------------------------------------------------------------------*)
type Env =
{
  kindenv : MILTy.Kind Var.Map.map,
  renaming : Var.Var Var.Map.map,
  renamebound : bool
}

(*----------------------------------------------------------------------*)
(* Extend kind environment with bound kinds				*)
(*----------------------------------------------------------------------*)
fun envPlusBounds { kindenv, renaming, renamebound } (tyvars, tys) =
{
  kindenv = ListPair.foldl (fn ((x,_), ty, kindenv) =>
    Var.Map.insert(kindenv, x, MILTy.Bound ty)) kindenv (tyvars, tys),
  renaming = renaming,
  renamebound = renamebound
}

val emptyEnv = 
{ kindenv = Var.Map.empty, renaming = Var.Map.empty, renamebound = false }

fun envPlusRenaming (env as { kindenv, renaming, renamebound }) xs =
if renamebound 
then
{
  kindenv = kindenv,
  renaming = 
    foldl (fn (x, r) => 
      let val x' = Census.freshVar 0 in Var.Map.insert(r, x, x') end) 
    renaming xs,
  renamebound = renamebound
}
else env

(*----------------------------------------------------------------------*)
(* Monomorphise a value term and rename variables where appropriate	*)
(*----------------------------------------------------------------------*)
fun monoVal (env : Env) v =
let 
  val mc = monoCmp env
  val mv = monoVal env
in
case v of
  Var x => 
  (addUninst x; v)

| SCon _ => 
  v

| Inj(ty, i, vs) => 
  Inj(ty, i, map mv vs)

| Coerce(v, ty) =>
  Coerce(mv v, ty)

| ExCon(excon, vs) => 
  ExCon(excon, map mv vs)

| Tuple vs => 
  Tuple (map mv vs)

| Proj(i, v) => 
  Proj(i, mv v)

| TApp(Var x, tys) => 
  TApp(Var(applySpec (#kindenv env) (x, tys)), tys)

| Fold(v, ty) => 
  Fold(mv v, ty)

| Unfold v => 
  Unfold (mv v)

| _ =>
  MILPretty.failVal v "Monomorphise.monoVal: illegal value term"

end

(*----------------------------------------------------------------------*)
(* Monomorphise a computation term and rename variables 		*)
(*----------------------------------------------------------------------*)
and monoCmp (env : Env) e = 
let 
  val mc = monoCmp env
  val mv = monoVal env

  fun monoAbstr (xs,e) = (xs, monoCmp (envPlusRenaming env xs) e)
  fun monoTAbstr (xs:MILTerm.TypedVar list,e) = 
    (xs, monoCmp (envPlusRenaming env (map #1 xs)) e)

  fun monoCase (i, abs) = (i, monoAbstr abs)
    
  fun monoCases (v, bindargs, cases, def) =
    (mv v, bindargs, map monoCase cases, Option.map mc def)
in
case e of
  App(v, vs) => 
  App(mv v, map mv vs)

| Java(jop, vs, cty) =>
  Java(jop, map mv vs, cty)

| Let(e, abs) =>
  Let(mc e, monoTAbstr abs)

| Triv vs => 
  Triv (map mv vs)

| Encap e =>
  Encap (mc e)

| Case cases => 
  Case (monoCases cases)

| CaseSCon cases => 
  CaseSCon (monoCases cases)

| CaseExCon cases => 
  CaseExCon (monoCases cases)

| Cond(t, v1, v2, e1, e2) => 
  Cond(t, mv v1, mv v2, mc e1, mc e2)

| Throw(v, tys, loc) =>
  Throw(mv v, tys, loc)

| TryLet(e, handlers, body) =>
  TryLet(mc e, map monoTAbstr handlers, monoTAbstr body)

(*......................................................................*)
(* Specialise a letrec.							*)
(* First recurse on the body, gathering uses of the function vars.      *)
(* If tyvars is empty, do the obvious thing on the definitions.         *)
(* Otherwise, determine what distinct instances _any_ of the functions  *)
(* is used at in the body, and put these in a list.                     *)
(*......................................................................*)
| LetFun(tyvars, kind, def, e) => 
  let
    val _ = 
    case def of
      RecFun defs => initInstances (map #1 defs)
    | Fun (f, _) => initInstances [f]

    val e = mc e
  in
  case def of
    RecFun defs =>
    if null tyvars 
    then 
      let 
        val defs = 
          map (fn (f, g, tabs, cty) => (f, g, monoTAbstr tabs, cty)) defs
      in
        LetFun(tyvars, kind, RecFun defs, e)
      end
    else 
    let
      fun specialise (tys,fs') =
      let
        val env' = envPlusBounds env (tyvars, tys)

        fun applyToDef ((f, g, (xs,e), cty), f') =
          (f', g, (xs, monoCmp env' e), cty)

        val tyvars' = 
          ListPair.map (fn ((x,_),ty) => (x,MILTy.Bound ty)) (tyvars,tys)
      in
        (tyvars', ListPair.map applyToDef (defs,fs'))
      end
      val { uninstantiated, instances, funs } = getInstances (#1 (hd (defs)))
      val pairs = map specialise instances
    in
      if uninstantiated
      then 
        let 
          val defs = 
            map (fn (f, g, tabs, cty) => (f, g, monoTAbstr tabs, cty)) defs
        in
          (foldr (fn ((tyvars,defs),e) => LetFun(tyvars, kind, RecFun defs, e))
          (LetFun(tyvars, kind, RecFun defs, e)) pairs)
        end
      else
        (foldr (fn ((tyvars,defs),e) => LetFun(tyvars, kind, RecFun defs, e)) 
          e pairs)
    end

  | Fun (f, tabs as (xs,e')) =>
    if null tyvars 
    then 
      LetFun(tyvars, kind, Fun (f, monoTAbstr tabs), e)
    else
    let
      fun specialise (tys,[f']) =
      let
        val env' = envPlusBounds env (tyvars, tys)
        val tyvars' = 
          ListPair.map (fn ((x,_),ty) => (x,MILTy.Bound ty)) (tyvars,tys)
      in
        (tyvars', (f', (xs, monoCmp env' e')))
      end
      val { uninstantiated, instances, funs } = getInstances f
      val defs = map specialise instances
    in
      if uninstantiated
      then 
        foldr (fn ((tyvars, def), e) => LetFun(tyvars, kind, Fun def, e))
          (LetFun(tyvars, kind, Fun (f,tabs), e)) defs
      else
        foldr (fn ((tyvars, def), e) => LetFun(tyvars, kind, Fun def, e)) 
          e defs
    end
  end

(*......................................................................*)
(* The types of fields and methods should be monomorphic already.	*)
(*......................................................................*)
| LetClass(classname, classinfo, fields, methods, e) =>
  let 
    val methods = map (fn (s, m, tys, tyopt, absopt) => 
    (s, m, tys, tyopt, 
      case absopt of
        NONE => NONE

      | SOME (f,abs) => SOME (f, monoAbstr abs))) methods

  in
    LetClass(classname, classinfo, fields, methods, mc e)
  end

| Alloc (f,v) => 
  Alloc (f,mv v)

| Deref v => 
  Deref (mv v)

| Assign(v1,v2) => 
  Assign(mv v1, mv v2)

(*......................................................................*)
(* Specialise let x = Fn tyvars.v in e.					*)
(* First recurse on the body, gathering uses of x.                      *)
(* If tyvars is empty, do the obvious thing on the definition.          *)
(* Otherwise, determine at what distinct instances x is used in the     *)
(* body, and monomorphise the defn.                                     *)
(*......................................................................*)
| LetVal(x, TAbs(tyvars, v), body) =>
  let
    val _ = initInstances [x]
    val body = mc body
    val { uninstantiated, instances, funs } = getInstances x
    fun specialise (tys, [x]) =
      let
        val env' = envPlusBounds env (tyvars, tys)
        val tyvars' = 
          ListPair.map (fn ((x,_),ty) => (x,MILTy.Bound ty)) (tyvars,tys)
      in
        (x, TAbs(tyvars', monoVal env' v))
      end
    val r = map specialise instances
  in    
    (foldl (fn ((x,v),body) => LetVal(x, v, body))
      (if uninstantiated then LetVal(x, TAbs(tyvars, v), body) else body) r)
  end
    
| LetVal(x, v, e) =>
  LetVal(x, mv v, mc e)

end 

val _ = Census.clearCensus ();
val oldsize = MILTermOps.absSize e
val e = Census.renameCmp (monoCmp emptyEnv e)
val newsize = MILTermOps.absSize e

val _ = if Controls.isOn "showSpecs" 
        then Debug.print("\nGrowth of term: " ^ 
          Int.toString ((newsize*100) div oldsize - 100) ^ "%  ")
        else ()

in
  e
end 

end (* of local open MILTerm *)

end (* of struct *)
