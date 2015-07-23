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
(* Auxiliary operations for compilation of fixed classes (E*, R*, etc.)	*)
(*======================================================================*)
structure CompileFixedOps =
struct

local open JavaRepOps CompileOps
in

fun makeDiagFields () =
if Controls.isOn "countInits"
then 
 [Field.simple
 {
   name = JavaString.fromString JavaNames.allocCount,
   flags = [Field.PUBLIC, Field.STATIC],
   field_type = Types.F(0, Types.INT),
   attributes = Attribute.empty
 }]
 else []

(*----------------------------------------------------------------------*)
(* Make a list of numbered field definitions				*)
(*----------------------------------------------------------------------*)
fun makeProdFields flags jtys =
  makeDiagFields () @
  Gen.mapi (fn (i, jty) => 
  Field.simple 
  {
    name = JavaString.fromString (JavaNames.argLabel i), 
    flags = flags, 
    field_type = jty,
    attributes = Attribute.empty
  }) jtys

(*----------------------------------------------------------------------*)
(* Make a list of selector functions			                *)
(*----------------------------------------------------------------------*)
fun makeProdSelectors class flags pairs =
  if Controls.isOn "MicrosoftBug"
  then map (fn (label, rep) => 
  let
    val obj = Blocks.new_value ()
    val resval = Blocks.new_value ()
  in
    Method.simple
    {
      name = label,
      flags = flags,
      method_type = Descriptors.M(SOME (toJava rep), []),
      attributes = Attribute.setCode (
      Blocks.compile_block (newBlock (
        [(obj, JavaRep.Java (Types.CLASS class))],
        [instr 
         (Blocks.getfield (fref (class, label, rep, true), Blocks.OPTIONAL),
            [obj], [resval])],
        Blocks.return [resval])), Attribute.empty)
    }
  end) pairs else []

(*----------------------------------------------------------------------*)
(* Create an <init> method which calls the superclass <init> and fills  *)
(* in instance variables with the parameters it has been passed.        *)
(*----------------------------------------------------------------------*)
fun makeInitMethod (class, superclass, pairs, passon, docount) =
let
  val typedargs = map (fn (fld,ty) => (Blocks.new_value (), fld, ty)) pairs
  val passonargs = map (fn ty => Blocks.new_value ()) passon
  val obj = Blocks.new_value ()
in 
Method.simple
{
  name = JavaString.fromString "<init>", 
  flags = [Method.PROTECTED],  
  method_type = 
    Descriptors.M(NONE, map (toJava o #2) pairs @ map toJava passon),

  attributes = Attribute.setCode (Blocks.compile_block (newBlock (
    (obj, JavaRep.Java (Types.CLASS class)) :: 
      ListPair.zip (map #1 typedargs, map #3 typedargs) @
      ListPair.zip (passonargs, passon),
      (
        (if null passon andalso Controls.isOn "omitInits"
        then []
        else [instr
        (Blocks.invoke_special (mref (superclass, 
          JavaString.fromString "<init>", passon, NONE)),
          obj::passonargs, [])]) @

        (if Controls.isOn "countInits" andalso docount
         then 
         let
           val v = Blocks.new_value ()
           val v' = Blocks.new_value ()
           val v'' = Blocks.new_value ()
           val v''' = Blocks.new_value ()
         in
         [getstatic (class, JavaString.fromString JavaNames.allocCount,
            true, (v, JavaRep.Java Types.INT)),
          instr (Blocks.add, [v, constInt 1], [v']),
          putstatic (class, JavaString.fromString JavaNames.allocCount, 
            true, (v', JavaRep.Java Types.INT)),
          getstatic (JavaNames.globalClass, 
            JavaString.fromString JavaNames.allocCount,
            true, (v'', JavaRep.Java Types.INT)),
          instr (Blocks.add, [v'', constInt 1], [v''']),
          putstatic (JavaNames.globalClass, 
            JavaString.fromString JavaNames.allocCount, 
            true, (v''', JavaRep.Java Types.INT))]
         end
         else []
        ) @
        map (fn (arg,fld,rep) =>
          instr 
          (Blocks.putfield (fref (class, fld, rep, true), Blocks.OPTIONAL),
             [obj,arg], [])) typedargs
      ),
      Blocks.return []
    )), Attribute.empty)
}
end

end (* of local open *)

end (* of struct *)
