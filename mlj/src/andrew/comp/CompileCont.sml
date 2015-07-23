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

structure CompileCont :> COMPILECONT =
struct

local 
  open MILTerm JavaNames JavaRepOps CompileOps 
    CompileVal CompileCmp CompileReps
in

val js = JavaString.fromString
    
(*----------------------------------------------------------------------*)
(* Compile code for a continuation computation term.	        1/9/97	*)
(* Arguments:                                                           *)
(*   the term (ce)                                                      *)
(*   the type and kind environments env                                 *)
(* Results: (wrapped inside a state monad)                              *)
(*   the MIL type of ce                                                 *)
(*   the free variables of ce                                           *)
(*   a list of instructions -- the tail of a block                      *)
(*   an exit for this initial block                                     *)
(*----------------------------------------------------------------------*)
fun compile ce (env : Env) =
let

  fun lookupBlock (env : Env, Var x) = 
      Var.Map.find(#blocks env, x)

    | lookupBlock (env, TApp(Var x, tys)) =
      (case Var.Map.find(#blocks env, x) of
        SOME (block, catchblock, polyty) =>
        SOME (block, catchblock, 
          MILTy.app (MILTy.abs (valOf(MILTy.fromForall polyty)), tys))

      | NONE => NONE)
   
    | lookupBlock (env, _) = NONE

(*......................................................................*)
(* Compile a continuation to create a label.				*)
(*......................................................................*)
  fun compileLabel (App(v,vs)) (env : Env) =
  let
    val SOME (block, catchblock, funty) = lookupBlock(env, v) 
    val SOME (_, cty) = MILTy.fromArrow funty
    val (_, resulttys) = MILTy.fromCmp cty
    val (_, [], values) = compileVals vs env
  in
    (cty, (block, values))
  end

(*......................................................................*)
(* Compile a jump, possibly with a coercion first.			*)
(*......................................................................*)
  fun compileCoerceLabel (x,(value,rep),cast,App(v,vs)) = 
  let
    val SOME (block, catchblock, funty) = lookupBlock (env, v)
    val SOME (argtys, cty) = MILTy.fromArrow funty
    val (_, resulttys) = MILTy.fromCmp cty   
    val used = List.exists (fn Var x' => x=x' | _ => false) vs
       
    fun gather ([], instrs, formals, actuals, jumpargs) = 
        (instrs, formals, actuals, jumpargs)

      | gather (v::vs, instrs, formals, actuals, jumpargs) =
        let
          val formal = Blocks.new_value ()
        in
          if (case v of Var x' => x=x' | _ => false)
          then                
            let
              val (castinstrs, castvalue) = cast formal
            in
              gather (vs, castinstrs @ instrs,
                (formal,rep)::formals, value::actuals, castvalue::jumpargs)
            end
          else              
          let
            val (ty, [], value) = compileVal v env
            val rep = tyToRep env ty
            in
              gather (vs, instrs, (formal,rep)::formals, value::actuals, 
                formal::jumpargs)
            end
          end
  in
    if not used
    then 
      let 
        val (_, [], values) = compileVals vs env 
      in
        (cty, (block, values))
      end
    else
      let
        val (instrs, formals, actuals, jumpargs) = 
          gather (rev vs, [], [], [], [])

        val block' = 
          ref (SOME (newBlock (formals, instrs, goto (block,jumpargs))))
      in
        (cty, (block', actuals))
      end
  end

  (*....................................................................*)
  (* Compile code for an exception handler                              *)
  (*....................................................................*)
  fun compileHandler env ([(y,ty)], e) =
    let
      val exnvalue = Blocks.new_value ()
      val env' = extendTypes env (y, exnvalue, ty)
      val rep = tyToRep env ty
    in
      case e of
        App(Var x, vs) =>
        case Var.Map.find(#blocks env, x) of
          SOME (block, catchblock, funty) =>
          if List.all (SimplifyOps.isAtom (#kindenv env)) vs 
          andalso Controls.isOn "mergeHandlers"
          then
          let
            val (tys, instrs, values) = CompileVal.compileVals vs env'
            fun isTheExn (Var y') = Var.eq(y,y')
              | isTheExn _ = false
            val SOME (_,cty) = MILTy.fromArrow funty
          in
            (if isSome (!catchblock) 
            then () 
            else
            let
              (* Formal arguments to catch block *)
              val catchblockargs = ListPair.map
                (fn (v,ty) => 
                  if isTheExn v 
                  then (true, exnvalue, rep)
                  else (false, Blocks.new_value (), tyToRep env ty))
                (vs,tys)
            in
              catchblock := SOME (newCatchBlock((exnvalue, rep), 
                List.mapPartial (fn (b, value, rep) => 
                  if b then NONE else SOME (value,rep)) catchblockargs, [], 
                  goto (block, map #2 catchblockargs)))
            end);

            (cty, (rep, (catchblock, List.mapPartial 
              (fn (v,value) => if isTheExn v then NONE else SOME value)
              (ListPair.zip(vs,values)))))
          end
          else 
            Debug.fail "CompileCont.compileBlock: non-atomic args to handler"
    end

(*......................................................................*)
(* Compile a mutually-recursive set of local definitions.		*)
(*......................................................................*)
  fun compileBlock env (tyvars, RecFun recbinds) =
  let 
    (* The local blocks used for the defns *)
    val pairs = map 
      (fn recbind => (ref NONE : Blocks.block option ref, recbind)) recbinds

    (* The environment under which the definitions are compiled *)
    val defnenv =
      foldl
        (fn ((b, (_, g, (xs, _), cty)), env) =>
          extendBlocks env (g, b, MILTy.arrow(map #2 xs, cty)))
        (extendTyVars env tyvars)
        pairs

    (*..................................................................*)
    (* Compile one of the definitions              		        *)
    (*..................................................................*)
    fun compileDefn (block, (f, g, (xs, body), cty)) =
    let
      val args = map (fn _ => newHandle ()) xs
      val defnenv' = 
        ListPair.foldl 
          (fn ((x,ty), arg, env) =>  extendTypes env (x, arg, ty))
          defnenv
          (xs, args)
      val (argvars, argtys) = ListPair.unzip xs
      val argreps = map (tyToRep defnenv) argtys
      val (bodyty, bodyinstrs, bodyexit) = compile body defnenv'
      val typedargs = ListPair.zip(args, argreps)
    in
      block := SOME(newBlock(typedargs,bodyinstrs,bodyexit))
    end
  in
    app compileDefn pairs;

    (* The environment under which body should be compiled *)
    foldl
      (fn ((b, (f, _, (xs, _), cty)), env) =>
        extendBlocks env 
        (f, b, MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty))))
      env
      pairs
  end
 
(*......................................................................*)
(* Compile a non-recursive local block.					*)
(*......................................................................*)
  | compileBlock env (tyvars, Fun (f, (xs, body))) =
  let

    (* The environment under which the definitions are compiled *)
    val defnenv = extendTyVars env tyvars
    val args = map (fn _ => newHandle ()) xs
    val defnenv' = ListPair.foldl 
        (fn ((x,ty), arg, env) =>  extendTypes env (x, arg, ty))
        defnenv
        (xs, args)

    val (argvars, argtys) = ListPair.unzip xs
    val argreps = map (tyToRep defnenv) argtys
    val typedargs = ListPair.zip(args, argreps)
      
  in
    case body of
    
    (*..................................................................*)
    (* Compile an exception try block  			                *)
    (*..................................................................*)
    TryLet(try, handlers, (bodyxs, body)) =>
    let            

      (* Compile straight-line code for the try expression *)
      val (tryty,tryinstrs,tryvalues) = 
        CompileCmp.compileCmp true try defnenv'

      (* Compile the handler to produce a list of handlers *)
      val tryhandlers = map (#2 o compileHandler defnenv') handlers

      (* Compile the success expression *)
      val env'' = extendEnv defnenv' (map #1 bodyxs, tryvalues, tryty)

      val (tryexit, cty) = 
      case body of
        App(Var x, vs) =>
        case Var.Map.find(#blocks env, x) of
          SOME (bodyblock, _, funty) =>
          if List.all (SimplifyOps.isAtom (#kindenv env)) vs 
          then
          let
            val SOME (_, cty) = MILTy.fromArrow funty
            val (tys, instrs, values) = CompileVal.compileVals vs env''
          in
            (goto (bodyblock, values), cty)
          end
          else Debug.fail "CompileCont.compileBlock: non-atomic args to body"

      val b = SOME (newTryBlock(typedargs, tryinstrs, tryexit, tryhandlers))
    in
      extendBlocks env 
        (f, ref b, MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty)))      
    end           
  
    (*..................................................................*)
    (* Case on exception: throw and catch!  				*)
    (*..................................................................*)
  | CaseExCon(v, false, cases, optdefault) =>
    let
      val (ty, instrs, vvalue) = compileVal v defnenv'

      val handlers = 
        map (fn (ty, ([x], e)) => ([(x, ty)], e)) cases
        @ (case optdefault of 
             NONE => []
           | SOME e => [([(Var.dummy, MILTys.topExn)], e)])

      val pairs as ((cty,_)::_) = map (compileHandler defnenv') handlers

      val b = SOME (newTryBlock(typedargs, instrs, throw vvalue, 
        map #2 pairs))
    in
      extendBlocks env 
        (f, ref b, MILTy.forall(tyvars, MILTy.arrow(map #2 xs, cty)))
    end           

    (*..................................................................*)
    (* Compile an ordinary block  			                *)
    (*..................................................................*)
  | _ =>
    let
      val (bodyty, bodyinstrs, bodyexit) = compile body defnenv'
      val b = SOME(newBlock(typedargs,bodyinstrs,bodyexit))
    in
      extendBlocks env 
        (f, ref b, MILTy.forall(tyvars, MILTy.arrow(map #2 xs, bodyty)))
    end           
  end
  
in
case ce of

(*......................................................................*)
(* Value binding                           				*)
(*......................................................................*)
  LetVal(var, ve, body) =>
  let
    val (ty, instrs, value) = compileVal ve env
    val env' = extendTypes env (var, value, ty)
    val (cty, instrs', exit) = compile body env'
  in
    (cty, instrs @ instrs', exit)
  end


(*......................................................................*)
(* Moggi-let							5/9/97	*)
(*......................................................................*)
| Let(defn, (vars, body)) =>
  let
    val (defnty, defninstrs, defnvalues) =
      compileCmp (List.exists (fn (x,ty) => not (Var.isDummy x)) vars) 
        defn env
    val env' = extendEnv env (map #1 vars, defnvalues, defnty)
    val (bodyty, bodyinstrs, bodyexit) = compile body env'
  in
    (
      bodyty,
      defninstrs @ bodyinstrs,
      bodyexit
    )
  end

(*......................................................................*)
(* Local block definition						*)
(*......................................................................*)
| LetFun(tyvars, LocalFun, fundef, body) =>
  let
    val env' = compileBlock env (tyvars, fundef)
  in
    compile body env'
  end

(*......................................................................*)
(* Special constant case					3/9/97	*)
(*......................................................................*)
| CaseSCon (ve, bindargs, cases, default) =>
  let
    val (vety, veinstrs, vevalue) = compileVal ve env
    val verep = tyToRep env vety
  in
    if not (isInt verep)
    then 
      MILPretty.failCmp ce "CompileCont.compile: case on non-integer"
    else
      let
      (* Use the final case as the default expression if no default present *)
        val (cases, defaulte) = 
        case default of
          NONE => 
          let 
            val ((i, ([],e))::cases) = rev cases
          in 
            (rev cases, e) 
          end

        | SOME e => 
          (cases, e)
    
        fun itercomp [] = []

          | itercomp ((jcon, ([],e))::ps) = 
            let
              val (cty, label) = compileLabel e env
              val table = itercomp ps
            in
              (jconToInt jcon, label)::table
            end
        val (cty, defaultlabel) = compileLabel defaulte env
        val table = itercomp cases
      in
      (
        cty,
        veinstrs,
        switch
        (
          vevalue,
          table,
          defaultlabel
        )
      )
      end
  end

(*......................................................................*)
(* Case on universal sum					3/9/97	*)
(*......................................................................*)
| Case(v, false, cases, default) =>
  let
    val (ty, instrs, vvalue) = compileVal v env
    val rep = tyToRep env ty
    val SOME tyss = MILTy.fromSum ty

    (* Use the final case as the default expression if no default present *)
    val cases = 
      case default of
        NONE => cases
      | SOME e => (~1, ([Var.dummy],e))::cases
    
    fun makeLabel (i, ([x],e)) =
    let     
      val (cty, label) = compileCoerceLabel (x, (vvalue,rep),  
        fn fromval =>
        let
          val toval = Blocks.new_value ()
          val castty = MILTy.con (List.nth(tyss, i))
          val castrep = tyToRep env castty
        in
          ([checkcast (fromval, (toval, castrep))], toval)
        end, e)
    in      
      ((JavaInt.fromInt i, label), cty)
    end

    val ((_,defaultlabel), cty)::table = map makeLabel cases
    val tagvalue = Blocks.new_value ()
  in
    (
      cty,
      instrs @ [proj((vvalue, rep), js JavaNames.sumTag, true, 
        (tagvalue, int))],
      switch
      (
        tagvalue,
        map #1 table,
        defaultlabel
      )
    )
  end

(*......................................................................*)
(* Case on enumeration and 1+ types                                    	*)
(*......................................................................*)
| Case(v, true, cases, optdefault) =>
  let
    val (ty, instrs, value) = compileVal v env
  in
    case MILTy.fromSum ty of
  (*..................................................................*)
  (* 1+ types                                                         *)
  (*..................................................................*)
      SOME ([[], [ty']] | [[ty'], []]) =>
      let
        val (none,x,some) =
          case (cases, optdefault) of
            ([(_, ([], none)), (_, ([x], some))], NONE) => (none,x,some)
          | ([(_, ([x],some)), (_, ([], none))], NONE) => (none,x,some)
          | ([(_, ([], none))], SOME some) => (none,Var.dummy,some)
          | ([(_, ([x], some))], SOME none) => (none,x,some)
        val (nonecty, nonelabel) = compileLabel none env
        val (noneinstrs, nonevalue) = CompileOnePlus.none env ty'
        val rep = tyToRep env ty

        val (somecty, somelabel) =        
          if MILTy.someIsNop (#kindenv env) ty'
          then
            compileLabel some (extendTypes env (x, value, ty'))
          else
            compileCoerceLabel (x, (value, rep),
              fn fromval => CompileOnePlus.proj env (ty', fromval), some)
      in
      (
        MILTy.unionCmpTypes (nonecty, somecty),
        instrs @ noneinstrs,
        cond (Tests.eq, value, nonevalue, nonelabel, somelabel)
      )
      end

  (*..................................................................*)
  (* Must be enumeration types 					      *)
  (*..................................................................*)
    | SOME tyss =>
      let
      (* Use the final case as the default expression if no default present *)
      val cases = 
        case optdefault of
          NONE => cases
        | SOME e => (~1, ([],e))::cases
    
      fun makeLabel (i, (_,e)) = 
          let
            val (cty, label) = compileLabel e env
          in
            ((JavaInt.fromInt i, label), cty)
          end
      val ((_,defaultlabel), cty)::table = map makeLabel cases
    in
      (
        cty,
        instrs,
        switch
        (
          value,
          map #1 table,
          defaultlabel
        )
      )
    end
  end

(*......................................................................*)
(* Conditional                				        5/9/97	*)
(*......................................................................*)
| Cond(t, ve1, ve2, yesce, noce) =>
  let
    val (ty1, instrs1, value1) = compileVal ve1 env
    val (ty2, instrs2, value2) = compileVal ve2 env
    val rep = tyToRep env ty1
    val (nocty, nolabel) = compileLabel noce env
    val (yescty, yeslabel) = compileLabel yesce env
    val resvalue = Blocks.new_value ()
  in
    case t of
      MLEq =>      
      (case MILTy.fromJava ty1 of
        SOME (Types.CLASS _) =>
        (
          nocty,
          instrs1 @ instrs2 @ 
          [invokevirtual(js "equals", 
            [(value1, rep), (value2, JavaRep.Object)], 
            [(resvalue, JavaRep.Java Types.BOOLEAN)])],
          cond (Tests.eq, resvalue, constInt 0, nolabel, yeslabel)
        )

      | _ =>
        (
          nocty,
          instrs1 @ instrs2,
          cond (Tests.eq, value1, value2, yeslabel, nolabel)
        ))
    
    | JavaTest t =>
      (
        nocty,
        instrs1 @ instrs2,
        cond (t, value1, value2, yeslabel, nolabel)
      )  
  end
    
(*......................................................................*)
(* Throw an exception						3/9/97	*)
(*......................................................................*)
| Throw(ve, tys, (entity, {left, right})) =>
  let
    val (exnty, instrs, value) = compileVal ve env
  in
  (
    MILTy.cmp(Effect.throws, tys),
    instrs @ 
    (if Controls.isOn "exnLocs" andalso (left<>0 orelse right<>0)
    then 
      [putstatic(JavaNames.exnClass NONE, js JavaNames.exnLocMessage, false, 
      (constant (Constants.STRING (JavaString.fromString
        (" at " ^ Pretty.idToString (#2 entity) ^ ":" ^ 
         Int.toString (left+1) ^ "." ^ Int.toString (right+1)))), 
        JavaRep.Java (Types.CLASS ClassHandle.string)))]
    else []),
    throw value
  )
  end

(*......................................................................*)
(* Initialisation of null values					*)
(*......................................................................*)
| Init(x, i, v, e) =>
  let
    val (ty, instrs, value) = compileVal v env
    val fldrep = tyToRep env ty

    val (h,prodty) = 
      case Var.Map.find(#tyenv env, x) of
        SOME (h, ty) => (h, ty)
      | NONE => Debug.fail "CompileCont.compile: missing variable"

    val tys = case MILTy.fromProdCon prodty of
      SOME tys => tys
    | NONE => 
      MILPretty.failCmp ce 
      "CompileCont.compile: expected product/constructor type"

    val fldty = (List.nth (tys, i)) handle Subscript =>
      MILPretty.failVal v 
      ("CompileCont.compile: subscript " ^ Int.toString i ^ 
       " out of range: #fvs = " ^ Int.toString (length tys) ^ " for type "
       ^ MILTy.toString prodty)

    val env' = 
      extendTypes env (x, h, prodty 
(* MILTy.updateProdCon (prodty, i, fldty) *))

    val (cty,instrs',exit) = compile e env' 

  in
    (
      cty,
      instrs @ 
      (if Blocks.is_zero value then [] 
       else [putfield ((h, tyToRep env prodty), 
        js (JavaNames.argLabel i), true,
        (value, fldrep))]) @ instrs',
      exit
    )
  end    

(*......................................................................*)
(* Tail function call 						5/9/97	*)
(*......................................................................*)
| App(v, ves) =>
  (case lookupBlock (env, v) of
    SOME (block, catchblock, funty) =>
    let
      val cty = case MILTy.fromArrow funty of
        SOME (_,cty) => cty
      | NONE => MILPretty.failCmp ce "CompileCont.compile: not function type"
      val (_, resulttys) = MILTy.fromCmp cty
      val (tys, instrs, values) = compileVals ves env
    in
      (cty, instrs, goto (block, values))
    end

  | NONE =>
    let
      val (cty, instrs, values) = compileCmp true ce env
    in
      (cty, instrs, Blocks.return values)
    end
  )
    
(*......................................................................*)
(* Not a continuation-only term						*)
(*......................................................................*)
| _ =>
  let
    val (cty, instrs, values) = compileCmp true ce env
  in
    (cty, instrs, Blocks.return values)
  end
  
      
end



end (* of local open *)  
end
