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

structure CompileCmp :> COMPILECMP =
struct

local 
  open MILTerm JavaNames JavaRepOps CompileOps CompileVal CompileReps
in


val js = JavaString.fromString 
(*----------------------------------------------------------------------*)
(* Look up variable that might be a global ref.				*)
(*----------------------------------------------------------------------*)
fun lookupGlobVar (Var x) (env : Env) = 
    (case Var.Map.find(#globenv env, x) of
      SOME (i, ty) =>
      SOME (i, valOf (MILTy.fromRefty ty))
    | NONE => 
      NONE)
  | lookupGlobVar _ env = NONE


(*----------------------------------------------------------------------*)
(* Compile code for a non-continuation computation term.	3/9/97	*)
(* Arguments:                                                           *)
(*   the term (ce)                                                      *)
(*   the type and kind environments env                                 *)
(* Results: (wrapped inside a state monad)                              *)
(*   the MIL type of ce                                                 *)
(*   the free variables of ce                                           *)
(*   a list of instructions                                             *)
(*   the resulting basic block handles                                  *)
(*----------------------------------------------------------------------*)
fun compileCmp resultrequired ce (env : Env) =
case ce of

(*......................................................................*)
(* Moggi-val							1/9/97	*)
(*......................................................................*)
  Triv ves =>
  let
    val (tys, instrs, values) = compileVals ves env
  in
  (
    MILTy.noeffect tys,
    instrs,
    values
  )
  end

(*......................................................................*)
(* Allocate a reference cell       			        3/9/97	*)
(*......................................................................*)
| Alloc (kind,ve) =>
  let
    val (ty, instrs, value) = compileVal ve env 
    val refty = MILTy.refty ty
    val refvalue = Blocks.new_value ()
    val refrep = tyToRep env refty
    val rep = tyToRep env ty
  in
    case kind of
      AnyRef =>
      (
        MILTy.cmp(Effect.allocs, [refty]),
        instrs @ [init ([(value, rep)], (refvalue, refrep))],
        [refvalue]
      )
    | GlobalRef =>
      (
        MILTy.cmp(Effect.allocs, [refty]),
        instrs,
        [value]
      ) 
  end

(*......................................................................*)
(* Dereference a reference cell       				24/9/97	*)
(*......................................................................*)
| Deref ve =>
  let
    val value = Blocks.new_value ()
  in
    case lookupGlobVar ve env of
    NONE =>
    let
      val (refty, instrs, refvalue) = compileVal ve env
      val ty = 
        case MILTy.fromRefty refty of
          SOME a => a
        | NONE => MILPretty.failCmp ce "CompileCmp.compileCmp: not a ref type"
      val rep = tyToRep env ty
      val refrep = tyToRep env refty
      val instr = (Blocks.getfield
      (
        fref (toClass refrep, js (argLabel 0), rep, false),
        Blocks.MUSTDO
      ), { input = [refvalue], output = [value] })
    in
      (
        MILTy.cmp(Effect.reads, [ty]),
        instrs @ [instr],
        [value]
      )
    end

  | SOME (i, ty) =>
    (
      MILTy.cmp(Effect.reads, [ty]),
      [getstatic(JavaNames.globalClass, js (JavaNames.argLabel i), false,
        (value, tyToRep env ty))],
      [value]
    )
  end


(*......................................................................*)
(* Assign to a reference cell       			        24/9/97	*)
(*......................................................................*)
| Assign (refterm, term) =>
  (case lookupGlobVar refterm env of
    NONE =>
    let
      val (ty, instrs, value) = compileVal term env
      val rep = tyToRep env ty
      val (refty,  refinstrs, refvalue) = compileVal refterm env
      val refrep = tyToRep env refty
    in
    (
      MILTy.cmp(Effect.writes, []),
      refinstrs @ instrs @ 
        [putfield ((refvalue, refrep), js (argLabel 0), false, (value, rep))],
      []
    )
    end

  | SOME (i, ty) =>
    let
      val (ty, instrs, value) = compileVal term env
      val rep = tyToRep env ty
    in
    (
      MILTy.cmp(Effect.writes, []),
      instrs @ 
        [putstatic (JavaNames.globalClass, js (JavaNames.argLabel i), false,
          (value, rep))],
      []
    )
    end
  )
    

(*......................................................................*)
(* Function application  				        8/9/97 	*)
(*......................................................................*)
| App(ve, ves) =>
  let
    fun compApp () =
    let
      val f = 
        case ve of 
          Var f => f 
        | TApp(Var f, _) => f
        | Unfold (Var f) => f
        | Unfold (TApp(Var f, _)) => f
        | _ => MILPretty.failCmp ce 
          "CompileCmp:compileCmp: expected variable application"

      val (funty, funinstrs, funvalue) = compileVal ve env
      val (argtys, arginstrs, argvalues) = compileVals ves env
      val argreps = map (tyToRep env) argtys
      val funrep = tyToRep env funty
      val cty = 
        case MILTy.fromArrow funty of
          SOME (_, cty) => cty
        | _ => 
          MILPretty.failCmp ce "CompileCmp.compileCmp: expected function type"
      val (_, resulttys) = MILTy.fromCmp cty
      val resultreps = map (tyToRep env) resulttys
      val resultvalues = map (fn rep => (Blocks.new_value (), rep)) resultreps
      val a = 
        case whichAppMethod (env, f) of
          SOME i => JavaNames.appMethod i
        | NONE => "$"
    in
        (
          cty,
          funinstrs @ arginstrs @ 
            [invokevirtual(js a, 
             (funvalue, funrep)::ListPair.zip(argvalues, argreps), 
                resultvalues
            )],
          map #1 resultvalues
        )
    end

    fun compGlobalApp (i, funty) =
    let
      val (argtys, arginstrs, argvalues) = compileVals ves env
      val argreps = map (tyToRep env) argtys
      val SOME (_, cty) = MILTy.fromArrow funty
      val (_,resulttys) = MILTy.fromCmp cty
      val resultreps = map (tyToRep env) resulttys
      val resultvalues = map (fn _ => newHandle ()) resultreps
    in
      (
        cty,
        arginstrs @ 
        [instr (Blocks.invoke_static(
          mref 
          (
            JavaNames.globalClass,
            js (JavaNames.globalMethod i),
            argreps,
            Gen.hd resultreps
          )), argvalues, resultvalues)],
        resultvalues
      ) 
    end

  in
    case ve of
      Var v =>
      (case Var.Map.find(#known env, v) of
        SOME (i, funty) =>
        compGlobalApp (i, funty)

      | _ => compApp ())

    | TApp(Var v, tys) =>
      (case Var.Map.find(#known env, v) of
        SOME (i, polyty) =>
        compGlobalApp (i, MILTy.app (MILTy.abs (valOf(MILTy.fromForall polyty)), tys))

      | _ => compApp ())

    | _ => compApp ()
  end

(*......................................................................*)
(* Java operations                                           	 4/9/97	*)
(*......................................................................*)
| Java(jop as (optype, optty, optname), ves, cty) =>
  let
    val (effect, restys) = MILTy.fromCmp cty
    val (tys, instrs, values) = compileVals ves env
    val reps = map (externalTyToRep env) tys
    val resreps = map (externalTyToRep env) restys
    val optrep = Option.map (tyToRep env) optty
    val (instrs', resopt) = 
      CompileJavaOp.compile resultrequired ((optype, optrep, optname), 
        values, reps, Gen.hd resreps, effect)
  in
  ( 
    cty,
    instrs @ instrs',
    Gen.optToList resopt
  )
  end

| _ =>
  MILPretty.failCmp ce "CompileCmp.compileCmp: continuation-only term"
  
    

end (* of local open *)  
end
