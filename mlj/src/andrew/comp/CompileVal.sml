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

structure CompileVal :> COMPILEVAL =
struct

local 
  open MILTerm JavaNames JavaRepOps CompileOps CompileReps
in

fun isDummyVar (Var x) = Var.isDummy x
  | isDummyVar _ = false

(*----------------------------------------------------------------------*)
(* Compile code for a value expression.				1/9/97	*)
(* Arguments:                                                           *)
(*   the value term (ve)                                                *)
(*   the type and kind environments env                                 *)
(* Results: (wrapped inside a state monad)                              *)
(*   the MIL type of ve                                                 *)
(*   the free variables of ve                                           *)
(*   a list of instructions                                             *)
(*   the resulting basic block handle                                   *)
(*----------------------------------------------------------------------*)
fun compileVal (v : Val) (env : Env) =
case v of

(*......................................................................*)
(* Java constants compile directly to new_constant_value	2/9/97  *)
(*......................................................................*)
  SCon (ty, jcon) => 
  (
    ty,
    [],
    constant jcon
  )
    
(*......................................................................*)
(* A variable:                                                  2/9/97  *)
(* Note: only locals are considered `free'.                             *)
(*......................................................................*)
| Var x =>
  (
    case Var.Map.find(#tyenv env, x) of
      SOME (h, ty) =>
      (
        ty, 
        [], 
        h
      )

    | NONE => 
      MILPretty.failVal v "CompileTerm.compileVal: variable not in environment"
  )

(*......................................................................*)
(* Injection into a sum type			                        *)
(*......................................................................*)
| Inj(ty, i, vs) =>
  let
    val (tys, instrs, args) = compileVals vs env
  in
    case MILTy.fromSum ty of

  (*..................................................................*)
  (* 1+ types                                                         *)
  (*..................................................................*)
      SOME ([[], [ty']] | [[ty'], []]) =>
      (case args of
        [] =>
        let
          val (instrs, result) = CompileOnePlus.none env ty'
        in
          (ty, instrs, result)
        end

      | [arg] =>
        let
          val (instrs, result) = CompileOnePlus.some env (ty', arg)
        in
          (ty, instrs, result)
        end

      | _ =>
        Debug.fail "CompileVal.compile: multiple args to 1+ type"
      )

  (*..................................................................*)
  (* Universal sums and enumerations      			      *)
  (*..................................................................*)
    | SOME tyss =>
      if List.all List.null tyss
      then (ty, [], constInt i)
      else
      let
        val argreps = map (tyToRep env) tys
        val rep = JavaRep.Con NONE
        val h = Blocks.new_value ()
        val conty = MILTy.con tys
        val conrep = tyToRep env conty
      in
        (
          ty,
          instrs @
          [init (ListPair.zip(args, argreps) @ [(constInt i, JavaRepOps.int)], 
            (h, conrep))],
          h
        )
      end
 
  end

(*......................................................................*)
(* Exception constructors:                                       3/9/97 *)
(*......................................................................*)
| ExCon(ty, vs) =>
  let
    val h = Blocks.new_value ()
    val (argtys, instrs, args) = compileVals vs env
    val exnrep = tyToRep env ty
    val reps = map (tyToRep env) argtys
  in
    (
      MILTys.topExn,
      instrs @ [init (ListPair.zip(args, reps), (h, exnrep))],
      h
    )
  end


(*......................................................................*)
(* Tuples 							3/9/97	*)
(*......................................................................*)
| Tuple vs =>
  let
    val (tys, instrs, values) = compileVals vs env
    val reps = map (tyToRep env) tys
    val prodty = MILTy.prod tys
    val prodrep = tyToRep env prodty
  in
    if null vs
  (*..................................................................*)
  (* The empty tuple (that is, () : unit) is represented by NULL      *)
  (*..................................................................*)
    then
    (
      prodty,
      [],
      constNull
    )

  (*..................................................................*)
  (* Otherwise create an object of the appropriate product class      *)
  (*..................................................................*)
    else
    let
      val h = Blocks.new_value ()
    in
    (
      prodty,
      instrs @ [init (ListPair.zip(values, reps), (h, prodrep))],
      h
    )
    end
  end
     
(*......................................................................*)
(* Projection from a tuple/constructor/exn constructor		3/9/97	*)
(* Special case for single string argument exception constructors.      *)
(*......................................................................*)
| Proj (i, v') =>
  let
    val (ty, instrs, value) = compileVal v' env
    val tys = case MILTy.fromProdCon ty of
      SOME tys => tys
    | NONE => 
      MILPretty.failVal v
      "CompileVal.compileVal: expected product/constructor type"

    val fldty = (List.nth (tys, i) handle Subscript =>
      MILPretty.failVal v "CompileVal.compileVal: subscript out of range")

    val h = Blocks.new_value ()
    val isStringExn =
      isSome (MILTy.fromExn ty) andalso length tys = 1
      andalso MILTy.eq (hd tys, MILTy.java (Types.CLASS ClassHandle.string))
    val fldrep = tyToRep env fldty
  in
    (
      fldty,
      instrs @ 
      [if isStringExn
       then invokevirtual(JavaString.fromString "getMessage", 
       [(value, JavaRep.Java (Types.CLASS ClassHandle.throwable))], 
         [(h,fldrep)])
       else proj((value, tyToRep env ty), JavaString.fromString (argLabel i),
         true, (h, fldrep))],
      h
    )
  end

(*......................................................................*)
(* Mu-introduction           					1/9/97	*)
(*......................................................................*)
| Fold (v, ty') => 
  let
    val (ty, instrs, value) = compileVal v env
  in
    (ty', instrs, value)
  end

(*......................................................................*)
(* Mu elimination                   				1/9/97 	*)
(*......................................................................*)
| Unfold v =>
  let
    val (ty, instrs, value) = compileVal v env
    val a = case MILTy.fromMu ty of
      SOME a => a
    | NONE => 
      MILPretty.failVal v "CompileVal.compileVal: expected recursive type"
  in
    (
      MILTy.unfold a,
      instrs,
      value
    )
  end

(*......................................................................*)
(* Type application              					*)
(*......................................................................*)
| TApp(v, tys) =>
  let
    val (polyty, instrs, value) = compileVal v env
    val a as (tyvars, ty) = case MILTy.fromForall polyty of 
      SOME a => a
    | NONE => MILPretty.failVal v "CompileVal:compileVal: expected poly type"
  in
    (
      MILTy.app (MILTy.abs a, tys),
      instrs,
      value
    )
  end

(*......................................................................*)
(* Type abstraction              					*)
(*......................................................................*)
| TAbs(tyvars, v) =>
  let
    val (ty, instrs, value) = compileVal v (extendTyVars env tyvars)
  in
    (
      MILTy.forall (tyvars, ty),
      instrs,
      value
    )
  end

(*......................................................................*)
(* Closures 							1/7/98	*)
(*......................................................................*)
| Closure(i, vs) =>
  let
    val (fvtys, instrs, values) = compileVals vs env
    val reps = map (tyToRep env) fvtys
    val closurety = MILTy.closure (SOME i, fvtys)
    val h = Blocks.new_value ()
  in
    (
      closurety,
      instrs @ [init(ListPair.zip(values,reps), (h,JavaRep.Closure (SOME i)))],
      h
    )
  end
     
(*......................................................................*)
(* Nop-op coercions.                                            1/9/97  *)
(*......................................................................*)
| Coerce(v, ty') =>
  let
    val (ty, instrs, value) = compileVal v env
  in
    (ty', instrs, value)
  end

(*......................................................................*)
(* Null value of given type					1/7/98	*)
(*......................................................................*)
| Null ty =>
  let
    val rep = tyToRep env ty
  in
    (ty, [], constant (nullValue rep))
  end


(*----------------------------------------------------------------------*)
(* Compile a list of value expressions, accumulating code.	1/9/97  *)
(*----------------------------------------------------------------------*)
and compileVals (vs : Val list) env =
case vs of
  [] => 
  (
    [],
    [],
    []
  )

| v::vs =>
  let
    val (ty, instrs, value) = compileVal v env
    val (tys, instrs', values) = compileVals vs env
  in
  (
    ty::tys,
    instrs @ instrs',
    value::values
  )
  end


end (* of local open *)  
end (* of struct *)
