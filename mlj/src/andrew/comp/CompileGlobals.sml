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
(* Compile the G class, given global variable types, global function    *)
(* definitions, and a <clinit> term.                                    *)
(*======================================================================*)
structure CompileGlobals :> COMPILEGLOBALS =
struct

local 
  open MILTerm JavaNames JavaRepOps CompileOps CompileReps
in

fun compile env (globvars, globfuns, clinit) =

let
  fun compileFun (i, (tyvars, f, (xs, e))) =
    let
      val _ = if Controls.isOn "showClasses" 
              then PrintManager.print (Var.toString f ^ "=" ^ 
                JavaNames.globalClassName ^ "." ^ 
                JavaNames.globalMethod i ^ " ")
              else ()

      val env = extendTyVars env tyvars
      val (argvars, argtys) = ListPair.unzip xs
      val args = map (fn _ => Blocks.new_value ()) xs
      val argreps = map (tyToRep env) argtys
     
      val env = 
        ListPair.foldl 
        (fn ((v,ty), arg, env) => extendTypes env (v, arg, ty))
        env (xs, args)

      val (bodyty, bodyinstrs, bodyexit) = CompileCont.compile e env

      val (eff,restys) = MILTy.fromCmp bodyty
      val resrep = Option.map (tyToRep env) (Gen.hd restys)

      val block = newBlock (ListPair.zip(args,argreps), bodyinstrs, bodyexit)
      val code = Blocks.compile_block block
    in
      Method.simple
      {
        name = JavaString.fromString (JavaNames.globalMethod i),
        flags = [Method.PROTECTED, Method.STATIC],
        attributes = Attribute.setCode (code, Attribute.empty),
        method_type = 
          Descriptors.M (Option.map toJava resrep, map toJava argreps)
      }
    end

  fun makeGlobField (i,ty) =  
  let
    val rep = tyToRep env (valOf (MILTy.fromRefty ty))
  in
    Field.simple 
    {
      name = JavaString.fromString (JavaNames.argLabel i),
      flags = [Field.PROTECTED, Field.STATIC],
      field_type = toJava rep,
      attributes = Attribute.empty
    }
  end

  (* Must be the last thing to be compiled as nones are required *)
  fun compileClinit ce =
    let
      val (bodyty, bodyinstrs, bodyexit) = CompileCont.compile ce env

      val noneinstrs = CompileOnePlus.makeNones ()

      val instrs = noneinstrs @ bodyinstrs
    in
      if null noneinstrs andalso (case ce of Triv [] => true | _ => false)
      then []
      else
      let
        val b = newBlock ([], instrs, bodyexit)
        val code = Blocks.compile_block b
      in
        [Method.simple
        {
          name = JavaString.fromString "<clinit>",
          flags = [Method.PUBLIC, Method.STATIC],
          attributes = Attribute.setCode (code, Attribute.empty),
          method_type = Descriptors.M (NONE, [])
        }]
      end
    end

  val globmethods = Gen.mapi compileFun globfuns
  val methods = compileClinit clinit @ globmethods
  val fields = CompileFixedOps.makeDiagFields () @
               Gen.mapi makeGlobField globvars @ 
               CompileOnePlus.makeNoneFields () @
               MicrosoftBug.kludgeFields ()
in
  if null fields andalso null methods then []
  else
  [Class.middling
  {
    this = JavaNames.globalClass,
    interfaces = [],
    attributes = Attribute.empty,
    flags = [],
    super = SOME ClassHandle.object,
    fields = fields,
    methods = methods
  }]
end

end (* of local open *)
end (* of struct *)
