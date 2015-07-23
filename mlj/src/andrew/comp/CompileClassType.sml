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
(* Compile a programmer-defined class.                                  *)
(*======================================================================*)
structure CompileClassType :> COMPILECLASSTYPE =
struct

local 
  open JavaRepOps CompileOps CompileReps
in

fun compile env (classty, (flags, super, implements), fields, methods) =
let
  val classrep = tyToRep env classty

  val superrep = Option.map (tyToRep env) super
  val implementsreps = map (tyToRep env) implements

  fun compileMethod (name, flags, argtys, resty, absopt) =
    let

      
      (* All argument types including "this" *)
      val isstatic = List.exists (fn Method.STATIC => true | _ => false) flags
      val argreps = map (externalTyToRep env) argtys

      val (argtys', argreps') =
        if isstatic 
        then (argtys, argreps)
        else (classty::argtys, classrep :: argreps)

      val resrep = Option.map (externalTyToRep env) resty
      val attributes =
      case absopt of
        NONE => Attribute.empty

      | SOME (f, (argvars, ce)) =>
        let
          val _ = if Controls.isOn "showClasses" 
                  then PrintManager.print (Var.toString f ^ "=" ^
                    JavaRepOps.toString classrep ^ "." ^ 
                    JavaString.toMLString name ^ " ")
                  else ()

          val args = map (fn rep => (Blocks.new_value (), rep)) argreps'
          val env = 
            ListPair.foldl 
            (fn ((v, ty), (value,_), env) => extendTypes env (v, value, ty))
            env (ListPair.zip(argvars, argtys'), args)

          val (bodyty, bodyinstrs, bodyexit) = CompileCont.compile ce env

        in
          Attribute.setCode (
            Blocks.compile_block (newBlock (args, bodyinstrs, bodyexit)),
            Attribute.empty)
        end
    in
      Method.simple
      {
        name = name,
        flags = flags,
        attributes = attributes,
        method_type = 
          Descriptors.M (Option.map toJava resrep, map toJava argreps)
      }
    end

  fun compileField (name, flags, ty, constopt) =
  let
    val rep = externalTyToRep env ty
  in
    Field.simple 
    {
      name = name,
      flags = flags,
      field_type = toJava rep,
      attributes = 
        case constopt of
          NONE => Attribute.empty
        | SOME c => Attribute.setConstantValue(c, Attribute.empty)
    }
  end
in
  Class.middling
  {
    this = toClass classrep,
    flags = flags, 
    attributes = Attribute.empty,
    super = 
      SOME (case superrep of NONE => ClassHandle.object 
                           | SOME rep => toClass rep),
    interfaces = map toClass implementsreps,
    fields = map compileField fields,
    methods = map compileMethod methods
  }
end

end (* of local open *)
end (* of struct *)
