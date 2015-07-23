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
(* Compile a closure class.                                             *)
(*======================================================================*)
structure CompileClosure :> COMPILECLOSURE =
struct

local 
  open JavaRepOps CompileOps CompileFixedOps CompileReps
in

fun compile env (i, { fvtys, meths }) =
let

  fun compileApp (appmeth, { tyvars, fvtys, fundef = (f, (xs, e), cty) }) =
    let
      val _ = if Controls.isOn "showClasses" 
              then PrintManager.print (Var.toString f ^ "=" ^ 
                JavaNames.closClassName (SOME i) ^ "." ^ 
                JavaNames.argLabel appmeth ^ " ")
              else ()
      val env = extendTyVars env tyvars

      val (argvars, argtys) = ListPair.unzip xs
      val args = map (fn _ => Blocks.new_value ()) xs
      val closval = Blocks.new_value ()
      val argreps = map (tyToRep env) argtys

      val env = 
        ListPair.foldl 
        (fn ((x,ty), arg, env) => extendTypes env (x, arg, ty))
        env (xs, args)

      val env = extendTypes env (f, closval, 
        MILTy.closure (SOME i, fvtys))
      
      val (eff,restys) = MILTy.fromCmp cty
      val funty = MILTy.arrow(argtys, cty)

      val (bodyty, bodyinstrs, bodyexit) = CompileCont.compile e env

      val resrep = Option.map (tyToRep env) (Gen.hd restys)
      val funrep = tyToRep env funty

      val code = Blocks.compile_block 
        (newBlock ((closval,funrep)::ListPair.zip(args,argreps), 
        bodyinstrs, bodyexit))

    in
      Method.simple
      {
        name = JavaString.fromString (JavaNames.argLabel appmeth),
        flags = [Method.PROTECTED, Method.FINAL],
        attributes = Attribute.setCode (code, Attribute.empty),
        method_type = 
          Descriptors.M (Option.map toJava resrep, map toJava argreps)
      }
    end

in
  Class.middling
  {
    this = JavaNames.closClass (SOME i),
    flags = [Class.FINAL], 
    super = SOME (JavaNames.closClass NONE),
    attributes = Attribute.empty,
    interfaces = [ClassHandle.serializable],
    fields = CompileFixedOps.makeProdFields [Field.PROTECTED] 
      (map (toJava o tyToRep env) fvtys),
    methods = makeInitMethod 
      (JavaNames.closClass (SOME i), JavaNames.closClass NONE, 
         Gen.mapi (fn (i,ty) => (JavaString.fromString (JavaNames.argLabel i), (tyToRep env ty)))
           fvtys, [], true)
      :: map compileApp (IMap.listItemsi meths)
  }
end

(*----------------------------------------------------------------------*)
(* Create the single class definition used for all functions		*)
(*----------------------------------------------------------------------*)
fun makeTopFun apps =
let
  val class = JavaNames.closClass NONE
in
  Class.middling
  {
    this = class,
    super = SOME ClassHandle.object,
    interfaces = [ClassHandle.serializable],
    attributes = Attribute.empty,
    fields = [],
    flags = [],
    methods = 
    makeInitMethod (class, ClassHandle.object, [], [], false) ::
    Gen.mapi (fn (i, funty) =>
    let
      val SOME (argtys, cty) = MILTy.fromArrow funty
      val (_, restys) = MILTy.fromCmp cty
      val argreps = map (tyToRep CompileOps.empty) argtys
      val resty = Gen.hd restys
      val resrep = Option.map (tyToRep CompileOps.empty) resty
    in
      Method.simple
      {
        name = JavaString.fromString (JavaNames.appMethod i), 
        flags = [Method.PROTECTED],
        method_type = Descriptors.M(Option.map toJava resrep, 
          map toJava argreps),
        attributes = Attribute.setCode(Blocks.compile_block (newBlock (
          map (fn rep => (Blocks.new_value (), rep)) 
           (JavaRep.Closure NONE :: argreps), [], 
          Blocks.athrow [Blocks.new_constant_value Constants.NULL])), 
        Attribute.empty)
      }
    end) apps
  }
end



end (* of local open *)
end (* of struct *)
