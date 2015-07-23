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
(* Closure convert the whole program.                                   *)
(*======================================================================*)
structure ClosConv :> CLOSCONV =
struct

local 
  open MILTerm FVInfoOps

  structure ClosClassArray = MonoArrayFn(type elem = ClosConvTypes.ClosDef)
  structure DynArray = DynamicArrayFn(ClosClassArray)
in

(*----------------------------------------------------------------------*)
(* Closure convert a term ce.                                           *)
(*----------------------------------------------------------------------*)
fun conv ce =
let

  (*..................................................................*)
  (* Do a flow analysis for closure functions.			      *)
  (*..................................................................*)
  val { appmeths, methtys } = 
    PrintManager.process ("Analysing flow", false)
      (fn () => ClosFlow.gather ce)

  (*..................................................................*)
  (* Traverse the term gathering free variable info.	              *)
  (*..................................................................*)
  val { fvs, funs, resulttys } = 
    PrintManager.process ("Gathering free variable info", false)
    (fn () => FVInfo.gather ce)

  (*..................................................................*)
  (* Collect all putative globals used anywhere other than top-level. *)
  (*..................................................................*)
  val requiredglobvars = 
    Var.Map.foldl (fn ({ fvs = { globvars, ...}, ... },globvars')=>
       Var.Map.unionWith #1 (globvars, globvars')) Var.Map.empty funs

  val requiredglobvars =
    Var.Map.foldli (fn (v,ty,m) => Var.Map.insert(m, v,(Census.freshVar 0,ty)))
      Var.Map.empty requiredglobvars

  (*..................................................................*)
  (* Report top-level fv info and global variable info.		      *)
  (*..................................................................*)
  val _ = 
    if Controls.isOn "showClosInfo"
    then 
    (
    Debug.print ("\nInfo for clinit: " ^ varsInfoToString fvs);
    Debug.print "\nRequired global vars: ";
    Debug.print (
      Pretty.simpleVec "," (fn (v,(i,ty)) => Var.toString v)
        (Var.Map.listItemsi requiredglobvars));
    Debug.print "\n"
    )
    else ()

  type Env = { tyvars : (Var.Var*MILTy.Kind) list, 
               globfuns : Var.Var Var.Map.map,
               extraTyVars : (Var.Var*MILTy.Kind) list Var.Map.map }

  (*..................................................................*)
  (* List the extra arguments required for function variable f.	      *)
  (*..................................................................*)
  fun getExtra f =
    case Var.Map.find(funs, f) of
      SOME { kind = kindopt, args = vs, fvs = 
        { fvs, locals, known, globvars }, cty, ...} => 
      (case kindopt of
        SOME KnownFun =>
        (List.filter (fn (x,_) => not(isSome (Var.Map.find(globvars, x))))
          (Var.Map.listItemsi fvs), globvars)

      | SOME (LocalFun) =>
        (Var.Map.listItemsi (Var.Map.unionWith #2 (fvs,globvars)), globvars)

      | SOME AnyFun =>
        (List.filter (fn (x,_) => not (isSome (Var.Map.find(globvars, x))))
         (Var.Map.listItemsi fvs), globvars)

      | NONE =>          
        ([], globvars))

    | NONE => 
      ([], Var.Map.empty)

  (*..................................................................*)
  (* Closure-convert an application				      *)
  (*..................................................................*)
  fun closeApp (env : Env) (x,tys) =
    case Var.Map.find(funs, x) of
      SOME 
      { kind, args, fvs = { fvs, locals, known, globvars }, cty, tyvars,...} =>
      let
        val (tyvars',fvs) =
        case kind of
          SOME KnownFun =>
          (getOpt (Var.Map.find(#extraTyVars env, x), []),
            List.filter (fn (x,_) => not(isSome(Var.Map.find(globvars, x))))
            (Var.Map.listItemsi fvs))

        | SOME LocalFun =>
          ([], Var.Map.listItemsi (Var.Map.unionWith #2 (fvs,globvars)))

        | SOME AnyFun =>
          ([] (* getOpt (Var.Map.find(#extraTyVars env, x), []) *), [])

        fun tapp (v, tyvars : (Var.Var*MILTy.Kind) list) =
          MILTermOps.tapp(v, tys @ map (MILTy.tyvar o #1) tyvars)
      in
        case Var.Map.find(#globfuns env, x) of
          NONE => (tapp (Var x, tyvars'), fvs)
        | SOME x' => (tapp (Var x', tyvars'), fvs)
      end

    | NONE =>
      (Var x, [])

  (*..................................................................*)
  (* Closure-convert a variable 				      *)
  (*..................................................................*)
  fun closeVar (env : Env) (x,tys) =
(*
    case Var.Map.find(funs, x) of
      SOME 
      { kind, args, fvs = { fvs, locals, known, globvars }, cty, tyvars} =>
      let
        val tyvars' =
        case kind of
          SOME AnyFun =>
          getOpt (Var.Map.find(#extraTyVars env, x), [])

        fun tapp (v, tyvars : (Var.Var*MILTy.Kind) list) =
          MILTermOps.tapp(v, tys @ map (MILTy.tyvar o #1) tyvars)
      in
        tapp (Var x, tyvars')
      end

    | NONE =>
*)
      MILTermOps.tapp(Var x, tys)


(*----------------------------------------------------------------------*)
(* Generate code to dereference the global variables listed.		*)
(*----------------------------------------------------------------------*)
fun readGlobals globvars e =
  foldl (fn ((x,ty),e) => 
    case Var.Map.find(requiredglobvars, x) of
      NONE => e
    | SOME (x',_) => 
      Let(Deref (Var x'), ([(x,ty)], e)))
    e globvars

(*----------------------------------------------------------------------*)
(* Generate code to assign the global variables listed.			*)
(*----------------------------------------------------------------------*)
fun writeGlobals globvars e =
  foldl (fn (x,e) => 
     case Var.Map.find(requiredglobvars, x) of
       NONE => e
     | SOME (x',_) =>
       Let(Assign (Var x', Var x), ([], e))) e globvars

(*----------------------------------------------------------------------*)
(* Closure classes							*)
(*----------------------------------------------------------------------*)
val closclasses = DynArray.array (0, { fvtys = [], meths = IMap.empty })

(*----------------------------------------------------------------------*)
(* Find a closure class for a closure function.				*)
(*----------------------------------------------------------------------*)
fun closClass (tyvars,f,g,(xs,e),cty) =
  case Var.Map.find(funs, f) of
    SOME { kind = SOME AnyFun, args = vs, 
           fvs = { fvs, locals, known, globvars }, cty,
           ... } => 
    let
      val S = foldl (fn ((x,MILTy.Bound ty),S) => Var.Map.insert(S,x,ty))
        Var.Map.empty tyvars
      val fvstys = Var.Map.listItems fvs
     
      val fvstysi = map (fn (x,ty) => (x,MILTy.subst S ty))
        (Var.Map.listItemsi fvs)
      val funty = MILTy.arrow (map #2 xs, cty)

      val e' = Gen.foldli (fn (i, (x,ty), e) =>
        LetVal(x, Proj(i, Var f), e)) 
          (if Var.isDummy g then e
           else LetVal(g, Coerce(Var f, funty), e)) (Var.Map.listItemsi fvs)

      val appnum = 
        case Var.Map.find(appmeths, f) of
          SOME i => i
        | NONE => Debug.fail ("ClosConv.closClass: app method missing for " ^
          Var.toString f)

      fun update (apps,i,lubtys,fvtys) = 
        (DynArray.update(closclasses, i, 
          { fvtys = lubtys, meths = IMap.insert(apps, appnum, 
             { fvtys = fvtys, tyvars = tyvars, 
               fundef = 
               (f,(xs,readGlobals (Var.Map.listItemsi globvars) e'),cty)
             })
          }); i)

      fun loop i =
        if i > DynArray.bound closclasses
        then update (IMap.empty, i, map #2 fvstysi, fvstys)
        else
        let
          val { fvtys, meths } = DynArray.sub(closclasses, i)
        in   
          case MILTy.lubs (fvtys, map #2 fvstysi) of
            NONE => loop (i+1)
          | SOME tys =>
            if isSome (IMap.find(meths, appnum))
            then loop (i+1)
            else update (meths, i, tys, fvstys)
        end
    in
      (loop 0, fvstysi)
    end  

  | _ =>
    Debug.fail "ClosConv.addClos: variable not a closure function"

(*----------------------------------------------------------------------*)
(* Global function definitions and environment.	                        *)
(* We forget the dependency structure and use a global set of mutually  *)
(* recursive functions.                                                 *) 
(*----------------------------------------------------------------------*)
val fundefs = ref (Var.Map.empty : 
  ((Var.Var*MILTy.Kind) list * Var.Var * MILTerm.TAbstr * MILTy.CmpType) 
  Var.Map.map)

(*----------------------------------------------------------------------*)
(* Global refs								*)
(*----------------------------------------------------------------------*)
val globalrefs = ref (Var.Map.empty : MILTy.Type Var.Map.map)

(*----------------------------------------------------------------------*)
(* Class definitions.                                                   *)
(*----------------------------------------------------------------------*)
val classdefs = ref ([] : 
  (MILTy.Type * ClassInfo * FieldInfo list * MethodInfo list) list)


fun makeClosTerm (tyvars,extratyvars,recbinds) e =
  let    
    fun find (x, f, i, [] : MILTerm.RecFunDef) = NONE
      | find (x, f, i, (f',g,(vs,_),cty)::rest) =
        if Var.eq(x,g) 
        then SOME(f, i, Coerce(Var f', 
          MILTy.forall(tyvars, MILTy.arrow(map #2 vs, cty))))
        else find (x,f,i,rest)

    val e = foldl (fn ((f,g,(vs,_),cty), e) =>
      LetVal(f, Coerce(Var f, 
        MILTy.forall(tyvars, MILTy.arrow(map #2 vs, cty))), e)) e recbinds

    fun loop ([], extras) = 
        foldl (fn ((f,i,v),e) => Init(f,i,v,e)) e extras

      | loop ((f,g,tabs as (vs,e):MILTerm.TAbstr,cty)::rest, extras) =
    let
      val (i, fvs) = closClass (tyvars @ extratyvars, f, g, tabs, cty)

      fun makeVals (pos, [], vals, extras) = 
        LetVal(f, Closure(i, rev vals), loop (rest, extras))

        | makeVals (pos, (x,ty)::fvs, vals, extras) =
          let
            val extraopt = find (x, f, pos, recbinds)
          in
            case extraopt of
              NONE => makeVals (pos+1, fvs, Var x::vals, extras)
            | SOME extra => makeVals (pos+1, fvs, Null ty::vals, extra::extras)
          end
    in
      makeVals (0, fvs, [], extras)
    end
  in
    loop (recbinds, [])
  end


and closeVal env v =
case v of
  Var x =>
  closeVar env (x, [])

| TApp(Var x, tys) =>
  closeVar env (x, tys)

| Inj (ty, i, vs) =>
  Inj (ty, i, map (closeVal env) vs)

| Coerce(v, ty) =>
  Coerce(closeVal env v, ty)

| ExCon(exname, vs) =>
  ExCon(exname, map (closeVal env) vs)

| Tuple vs =>
  Tuple (map (closeVal env) vs)

| Proj(i, v) =>
  Proj(i, closeVal env v)

| TApp(v, tys) =>
  TApp(closeVal env v, tys)

| TAbs(tyvars, v) =>
  TAbs(tyvars, closeVal env v)

| Fold(v, ty) =>
  Fold(closeVal env v, ty)

| Unfold v =>
  Unfold (closeVal env v)

| _ =>
  v

(*----------------------------------------------------------------------*)
(* Close known/local function definitions by adding extra arguments.    *)
(* Close class methods by storing free variables in globals.            *)
(* Close closures by annotating with free variables.                    *)
(*                                                                      *)
(*----------------------------------------------------------------------*)
and closeCmp (env as { globfuns, tyvars = tyvarenv, extraTyVars }) ce = 
let
  val cl = closeCmp env
  val clv = closeVal env
  fun closeCases (ve, bindargs, cases, ceopt) =
    (ve, bindargs, map (fn (i,(vs,body)) => 
    (i,(vs,writeGlobals vs (cl body)))) cases, Option.map cl ceopt)
in
case ce of
  App(Var x, vs) =>   
  let
    val (v,fvs) = closeApp env (x,[])    
  in
    App(v, map (Var o #1) fvs @ map clv vs)
  end

| App(TApp(Var x, tys), vs) =>
  let
    val (v,fvs) = closeApp env (x,tys)
  in
    App(v, map (Var o #1) fvs @ map clv vs)
  end

| App(v,vs) =>
  App(clv v, map clv vs)

| Let(Alloc(GlobalRef,v), ([(x,ty)],e)) =>
  (globalrefs := Var.Map.insert(!globalrefs, x, ty);
   Let(Assign(Var x, clv v), ([], cl e)))
| Let(ce, (vs,body)) => 
  Let(cl ce, (vs, writeGlobals (map #1 vs) (cl body)))
| Case cases => Case(closeCases cases)
| CaseSCon cases => CaseSCon(closeCases cases)
| CaseExCon cases => CaseExCon(closeCases cases)
| Cond(t, ve1, ve2, ce1, ce2) => 
  Cond(t, clv ve1, clv ve2, cl ce1, cl ce2)
| TryLet(ce0, tabss, (vs2,ce2)) => 
  TryLet(cl ce0, map (fn (vs,ce) => (vs, cl ce)) tabss, (vs2, 
    writeGlobals (map #1 vs2) (cl ce2)))
| LetVal(v, ve, ce) => LetVal(v, clv ve, writeGlobals [v] (cl ce))
| LetFun(tyvars, kind, fundef, ce) =>
  let
    val defnenv = 
    { tyvars = tyvars @ tyvarenv, globfuns = 
      if kind = KnownFun then
      case fundef of
        Fun _ => globfuns
      | RecFun recbinds => foldl (fn ((f,g,_,_),globfuns) =>
          Var.Map.insert(globfuns, g, f)) globfuns recbinds
      else globfuns,
      extraTyVars = 
        if kind=LocalFun then extraTyVars
        else case fundef of
          Fun(f,_) => extraTyVars
        | RecFun recbinds => 
          foldl (fn ((f,g,_,_),ex) => Var.Map.insert(ex, g, 
            if kind = KnownFun then tyvars @ tyvarenv else tyvarenv))
          extraTyVars recbinds }

    val bodyenv =
    { tyvars = tyvarenv, globfuns = globfuns,
      extraTyVars =
        if kind=LocalFun then extraTyVars
        else case fundef of
          Fun(f,_) => Var.Map.insert(extraTyVars, f, tyvarenv)
        | RecFun recbinds =>
          foldl (fn ((f,g,_,_),ex) => Var.Map.insert(ex, f, tyvarenv))
          extraTyVars recbinds }
  in
  case kind of
    AnyFun => 
    (case fundef of
      RecFun recbinds =>
      makeClosTerm (tyvars, tyvarenv,
        map (fn (f,g,(xs,e),cty) => 
          (f,g,(xs,closeCmp defnenv e),cty)) recbinds) 
        (writeGlobals (map #1 recbinds) (closeCmp bodyenv ce))
   
    | Fun (f, (vs,e)) =>
      makeClosTerm (tyvars, tyvarenv,
        [(f, Var.dummy, (vs, closeCmp defnenv e), 
          valOf(Var.Map.find(resulttys, f)))])
        (writeGlobals [f] (closeCmp bodyenv ce))
    )

  | LocalFun =>
    let
      val fundef =
        case fundef of
          RecFun recbinds =>
          RecFun (map (fn (f, g, (vs, ce), cty) =>
          let 
            val (ffvs,globvars) = getExtra f
            in
              (f, g, 
              (ffvs @ vs, writeGlobals (map #1 vs) (closeCmp defnenv ce)), cty)
            end) recbinds)

        | Fun(f, (vs, ce)) =>
          let 
            val (ffvs,globvars) = getExtra f
          in
             Fun(f, (ffvs @ vs,writeGlobals (map #1 vs) (closeCmp defnenv ce)))
          end
    in
      LetFun(tyvars, kind, fundef, closeCmp bodyenv ce)
    end

  | KnownFun => 
    (case fundef of
      RecFun recbinds =>
      let
        val recbinds = 
          map (fn (f, g, (vs, ce), cty) =>
          let 
            val (ffvs,globvars) = getExtra f
            val ce = closeCmp defnenv ce
            val ce = readGlobals (Var.Map.listItemsi globvars) ce
          in
            (f, g, (ffvs @ vs, ce), cty)
          end) recbinds
      in
        fundefs := foldl (fn ((f,g,tabs,cty),m) => 
          Var.Map.insert(m, f, (tyvars @ tyvarenv,g,tabs,cty))) (!fundefs) recbinds;
        closeCmp bodyenv ce
      end
      
    | Fun(f, (vs, ce')) =>
      let 
        val (ffvs,globvars) = getExtra f
        val ce' = closeCmp defnenv ce'
        val ce' = readGlobals (Var.Map.listItemsi globvars) ce'
        val tabs = (ffvs @ vs, ce')
      in
        fundefs := Var.Map.insert(!fundefs, f, (tyvars @ tyvarenv,Var.dummy,tabs,
          valOf (Var.Map.find(resulttys, f))));
        closeCmp bodyenv ce
      end
    )
  end

| LetClass(classname, classinfo, fields, methods, ce) =>
  let
    fun closeMethod (n,ms,tys,tyopt,SOME (f,(vs,ce))) =
        let 
          val SOME { kind = NONE, fvs = { globvars, ... }, ...} = 
            Var.Map.find(funs, f)
          val ce = readGlobals (Var.Map.listItemsi globvars) (cl ce)
        in
          (n,ms,tys,tyopt, SOME (f, (vs, ce)))
        end

      | closeMethod other = other
  in
    classdefs := (classname, classinfo, fields, map closeMethod methods)::
      !classdefs;
    cl ce
  end
| Init(x,i,v,ce) =>
  Init(x,i,clv v,cl ce)
| Encap ce =>
  Encap (cl ce)
| Deref v =>
  Deref (clv v)
| Assign(v1,v2) =>
  Assign(clv v1, clv v2)
| Alloc (f,v) =>
  Alloc(f,clv v)
| Triv vs =>
  Triv(map clv vs)
| Java(info,vs,cty) =>
  Java(info, map clv vs, cty)
| _ => ce
end

  val clinit = 
    PrintManager.process ("Converting", false)
      (fn () => closeCmp { tyvars = [], globfuns = Var.Map.empty, 
                          extraTyVars = Var.Map.empty } ce)

  fun addGlobLocals (f, (tyvars, g, tabs, cty)) =
      (tyvars, f, tabs)

  fun addClosLocals i =
    if i > DynArray.bound closclasses then []
    else 
    let
      val result = DynArray.sub(closclasses, i)
    in
      result ::
      addClosLocals (i+1)
    end

  val rfundefs = map addGlobLocals (Var.Map.listItemsi (!fundefs))
  val closdefs = addClosLocals 0
in
  {
    fundefs = rfundefs,
    funenv = #2 (Var.Map.foldli (fn (f, (tyvars,g,(vs,_),cty), (i,funenv)) =>
      let val ty = MILTy.forall(tyvars, MILTy.arrow(map #2 vs, cty))
          val funenv = Var.Map.insert(funenv, f, (i,ty))
      in
        (i+1, 
        if Var.isDummy g then funenv else Var.Map.insert(funenv,g,(i,ty))
        )
      end) (0,Var.Map.empty) (!fundefs)),
        
    closdefs = closdefs,
    classdefs = !classdefs, 
    clinit = clinit,
    globvars = Var.Map.foldl (fn ((x,ty),m) => 
      Var.Map.insert(m,x,MILTy.refty ty)) (!globalrefs) requiredglobvars,
    appmeths = appmeths,
    methtys = methtys
  }
end (* of let following fun conv *)

end (* of local open *)
end (* of struct *)

