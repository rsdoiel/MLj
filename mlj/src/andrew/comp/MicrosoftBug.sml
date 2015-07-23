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

structure MicrosoftBug =
struct

val js = JavaString.fromString

fun fref rep =
if JavaRepOps.isInt rep
then CompileOps.fref (JavaNames.globalClass, js "$$i", 
  JavaRep.Java Types.INT, true)
else 
  case rep of
    JavaRep.Java Types.FLOAT => 
    CompileOps.fref (JavaNames.globalClass, js "$$f", rep, true)
  | JavaRep.Java Types.DOUBLE =>
    CompileOps.fref (JavaNames.globalClass, js "$$d", rep, true)
  | JavaRep.Java Types.LONG =>
    CompileOps.fref (JavaNames.globalClass, js "$$l", rep, true)
  | _ =>
    CompileOps.fref (JavaNames.globalClass, js "$$o", JavaRepOps.object, true)

fun kludgeInstrs (resval,rep) =
  if Controls.isOn "MicrosoftBug"
  then
  [CompileOps.instr 
  (
    Blocks.putstatic (fref rep, Blocks.MUSTDO), 
    [resval], []
  )]
  else []

fun makeField (name,flags,ty) =
  Field.simple
  {
    name = JavaString.fromString name,
    flags = flags,
    field_type = ty,
    attributes = Attribute.empty
  }

fun kludgeFields () =
  if Controls.isOn "MicrosoftBug"
  then map makeField
  [
    ("$$i", [Field.PUBLIC, Field.STATIC], Types.F(0, Types.INT)),
    ("$$l", [Field.PUBLIC, Field.STATIC], Types.F(0, Types.LONG)),
    ("$$f", [Field.PUBLIC, Field.STATIC], Types.F(0, Types.FLOAT)),
    ("$$d", [Field.PUBLIC, Field.STATIC], Types.F(0, Types.DOUBLE)),
    ("$$o", [Field.PUBLIC, Field.STATIC], 
      Types.F(0, Types.CLASS ClassHandle.object))
  ]
  else []

end
