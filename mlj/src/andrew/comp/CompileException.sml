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
(* Compile an exception class       					*)
(*======================================================================*)
structure CompileException :> COMPILEEXCEPTION =
struct

local  
  open JavaRepOps CompileOps CompileFixedOps
in


val strty = Types.F(0, Types.CLASS ClassHandle.string)
val strrep = JavaRep.Java (Types.CLASS ClassHandle.string)

(*----------------------------------------------------------------------*)
(* Create an exnMessage method.                                         *)
(*----------------------------------------------------------------------*)
fun makeExnMessageMethod (class, str, holdsstring) =
let
  val obj = Blocks.new_value ()
  val str = if holdsstring then str ^ ": " else str
  val consthandle = Blocks.new_constant_value (Constants.STRING 
      (JavaString.fromString str))
  val reshandle = if holdsstring then Blocks.new_value ()
                  else consthandle
  val arghandle = Blocks.new_value ()
in 
  Method.simple
  {
    name = JavaString.fromString JavaNames.exnMessageMethod, 
    flags = [Method.PUBLIC],  
    method_type = Descriptors.M(SOME strty, []),
    attributes = Attribute.setCode (Blocks.compile_block
    (
      newBlock ([(obj, JavaRep.Java (Types.CLASS class))],
        if holdsstring 
        then
        [
          instr(Blocks.invoke_virtual
          (
            mref (ClassHandle.throwable, JavaString.fromString "getMessage", 
              [], SOME strrep)
          ), [obj], [arghandle]),
          instr(Blocks.invoke_virtual
          (
            mref (ClassHandle.string, JavaString.fromString "concat", 
              [strrep], SOME strrep)
          ), [consthandle, arghandle], [reshandle])
        ]
        else [],
        Blocks.return [reshandle]
      )
    ), Attribute.empty)
  }
end

(*----------------------------------------------------------------------*)
(* Create the "toString" method for the top exception class.            *)
(*----------------------------------------------------------------------*)
fun makeToStringMethod () =
let
  val obj = Blocks.new_value ()
  val reshandle = Blocks.new_value ()
  val reshandle2 = Blocks.new_value ()
  val lochandle = Blocks.new_value ()
in 
  Method.simple 
  {
    name = JavaString.fromString "toString",
    flags = [Method.PUBLIC, Method.FINAL],  
    method_type = Descriptors.M(SOME strty, []),
    attributes = Attribute.setCode (Blocks.compile_block
    (
      newBlock ([(obj, JavaRep.Java (Types.CLASS (JavaNames.exnClass NONE)))],
      instr (Blocks.invoke_virtual
      (
        mref (JavaNames.exnClass NONE, 
          JavaString.fromString JavaNames.exnMessageMethod, [], SOME strrep)
      ), [obj], [reshandle]) ::
      (if Controls.isOn "exnLocs"
      then
      [
      instr(Blocks.getstatic
      (
        fref (JavaNames.exnClass NONE, 
          JavaString.fromString JavaNames.exnLocMessage, strrep, true),
        Blocks.OPTIONAL
      ), [], [lochandle]),
      instr(Blocks.invoke_virtual
      (
        mref (ClassHandle.string, 
         JavaString.fromString "concat", [strrep], SOME strrep)
      ), [reshandle, lochandle], [reshandle2])
      ] else []),
      Blocks.return 
        [if Controls.isOn "exnLocs" then reshandle2 else reshandle]
      )
    ), Attribute.empty)
  }
end

(*----------------------------------------------------------------------*)
(* Create the "fillInStackTraceMethod" method for the top exn class.    *)
(*----------------------------------------------------------------------*)
fun makeFillInStackTraceMethod () =
let
  val obj = Blocks.new_value ()
in 
  Method.simple
  {
    name = JavaString.fromString "fillInStackTrace",
    flags = [Method.PUBLIC, Method.FINAL],  
    method_type = 
      Descriptors.M(SOME (Types.F(0, Types.CLASS ClassHandle.throwable)), []),
    attributes = Attribute.setCode (Blocks.compile_block 
    (
      newBlock ([(obj, JavaRep.Java (Types.CLASS (JavaNames.exnClass NONE)))],
      [],
      Blocks.return [obj]
      )
    ), Attribute.empty)
  }
end

(*----------------------------------------------------------------------*)
(* Create an exnName method.                                            *)
(*----------------------------------------------------------------------*)
fun makeExnNameMethod (class, str) =
let
  val obj = Blocks.new_value ()
  val consthandle = Blocks.new_constant_value (Constants.STRING 
      (JavaString.fromString str))
in 
  Method.simple
  { 
    name = JavaString.fromString JavaNames.exnNameMethod, 
    flags = [Method.PUBLIC],
    method_type = Descriptors.M(SOME strty, []),
    attributes = Attribute.setCode (Blocks.compile_block
    (
      newBlock ([(obj, JavaRep.Java (Types.CLASS class))],
        [], Blocks.return [consthandle])
    ), Attribute.empty)
  }
end

fun makeTopExnClass () =
Class.middling
{
  this = JavaNames.exnClass NONE,
  super = SOME ClassHandle.Exception, 
  attributes = Attribute.empty,
  interfaces = [ClassHandle.serializable],
  flags = [Class.ABSTRACT],
  fields = 
      if Controls.isOn "exnLocs" then
      [Field.simple 
       { 
         name = JavaString.fromString (JavaNames.exnLocMessage), 
         flags = [Field.PROTECTED, Field.STATIC], 
         field_type = Types.F(0, Types.CLASS ClassHandle.string),
         attributes = Attribute.setConstantValue (
           Constants.STRING (JavaString.fromString " at ?"), Attribute.empty)
       }] 
      else [],

  methods = 
  [
    makeInitMethod(JavaNames.exnClass NONE, ClassHandle.Exception,[],[],false),
    makeInitMethod(JavaNames.exnClass NONE, ClassHandle.Exception,[],[strrep],
      false),

    Method.simple
    {
      name = JavaString.fromString JavaNames.exnNameMethod,
      flags = [Method.PUBLIC, Method.ABSTRACT], 
      method_type = Descriptors.M(SOME strty, []),
      attributes = Attribute.empty
    },

    Method.simple
    {
      name = JavaString.fromString JavaNames.exnMessageMethod, 
      flags =[Method.PUBLIC, Method.ABSTRACT], 
      method_type = Descriptors.M(SOME strty, []), 
      attributes = Attribute.empty
    },

    makeToStringMethod ()
  ] 
  @ 
  (if Controls.isOn "fillInStackTrace" then [] 
  else [makeFillInStackTraceMethod ()])
}

fun makeExnClass (i, reps, longid) =
let
  val class = JavaNames.exnClass (SOME i)
  val jtys = map toJava reps
  val holdsstring = length reps = 1 andalso
    JavaRepOps.eq (hd reps, JavaRep.Java (Types.CLASS ClassHandle.string))
in
  Class.middling
  {
    this = class,
    super = SOME (JavaNames.exnClass NONE), 
    interfaces = [ClassHandle.serializable],
    attributes = Attribute.empty,
    flags = [Class.FINAL],
    fields = makeProdFields [Field.PROTECTED] 
      (if holdsstring then [] else jtys),
    methods = 
      [makeInitMethod (class, JavaNames.exnClass NONE,
        if holdsstring then [] 
        else Gen.mapi (fn (i,rep) => 
          (JavaString.fromString (JavaNames.argLabel i), rep)) reps, 
        if holdsstring then reps else [], true),
      makeExnMessageMethod (class, Pretty.longidToString longid, holdsstring),
      makeExnNameMethod (class, Pretty.longidToString longid)]
      @ 
      (if holdsstring then [] 
       else makeProdSelectors class [Method.PROTECTED, Method.FINAL] 
       (Gen.mapi (fn (i,rep) =>
         (JavaString.fromString (JavaNames.argLabel i), rep)) reps))
  }
end


end (* of local open *)  
end
