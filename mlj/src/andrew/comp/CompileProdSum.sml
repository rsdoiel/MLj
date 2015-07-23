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
(* Compile product and sum classes.                                     *)
(*======================================================================*)
structure CompileProdSum :> COMPILEPRODSUM =
struct

local 
  open CompileOps JavaRepOps CompileFixedOps
in

(*----------------------------------------------------------------------*)
(* Create a class definition for a product type				*)
(*----------------------------------------------------------------------*)
fun makeProdClass (i, args) =
let
  val jtys = map toJava args
  val class = JavaNames.prodClass i
  val rep = JavaRep.Prod i
  val fields = Gen.mapi (fn (i,rep) => 
    (JavaString.fromString (JavaNames.argLabel i), rep)) args
in
  Class.middling
  {
    this = class, 
    super = SOME ClassHandle.object,
    interfaces = [ClassHandle.serializable],
    flags = [Class.FINAL], 
    fields = makeProdFields [Field.PROTECTED] jtys,
    attributes = Attribute.empty,
    methods = 
      [makeInitMethod (class, ClassHandle.object, fields, [], true)]
      @ makeProdSelectors class [Method.PROTECTED, Method.FINAL] fields
  }
end

(*----------------------------------------------------------------------*)
(* Create a class definition for a sum constructor 			*)
(*----------------------------------------------------------------------*)
fun makeConClass (iopt, argreps) =
case iopt of
  NONE =>
  Class.middling
  {
    this = JavaNames.conClass NONE,
    super = SOME ClassHandle.object, 
    interfaces = [ClassHandle.serializable],
    attributes = Attribute.empty,
    flags = [],
    fields = 
    makeDiagFields () @
    [Field.simple {
      name = JavaString.fromString JavaNames.sumTag, 
      flags = [Field.PROTECTED], 
      field_type = Types.F(0, Types.INT),
      attributes = Attribute.empty
    }],
    methods = 
      [makeInitMethod (JavaNames.conClass NONE, ClassHandle.object,         
        [(JavaString.fromString JavaNames.sumTag, 
          JavaRep.Java Types.INT)], [], true)] @
       makeProdSelectors (JavaNames.conClass NONE) 
         [Method.PROTECTED, Method.FINAL] 
         [(JavaString.fromString JavaNames.sumTag, JavaRep.Java Types.INT)]
  }

| _ =>
  let
    val jtys = map toJava argreps
    val class = JavaNames.conClass iopt
    val rep = JavaRep.Con iopt
    val fields = Gen.mapi (fn (i,rep) => 
       (JavaString.fromString (JavaNames.argLabel i), rep)) argreps
  in
    Class.middling
    {
      this = class,
      super = SOME (JavaNames.conClass NONE), 
      attributes = Attribute.empty,
      interfaces = [ClassHandle.serializable],
      flags = [Class.FINAL],
      fields = makeProdFields [Field.PROTECTED] jtys,
      methods = 
        [makeInitMethod (class, JavaNames.conClass NONE, fields,
          [JavaRep.Java Types.INT], true)] 
        @ makeProdSelectors class [Method.PROTECTED, Method.FINAL] fields
    }
  end

end (* of local open *)
end (* of struct *)
