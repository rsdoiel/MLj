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
(* Java class definitions as used in SML type checking			*)
(*======================================================================*)
structure SMLClassDef =
struct

(*----------------------------------------------------------------------*)
(* Java field: name, modifiers, type and final static value info.       *)
(*----------------------------------------------------------------------*)
type FieldInfo = 
  JavaString.t * Field.flag list * SMLTy.Type * Constants.constant option

(*----------------------------------------------------------------------*)
(* Java method: name, modifiers, argument types and (optional) result   *)
(* type.                                                                *)
(*----------------------------------------------------------------------*)
type MethodInfo = 
  JavaString.t * Method.flag list * SMLTy.Type list * SMLTy.Type option

(*----------------------------------------------------------------------*)
(* Java class: (optional) name, modifiers, (optional) superclass and    *)
(* list of interfaces.                                                  *)
(*----------------------------------------------------------------------*)
type ClassInfo = 
  SMLClass.Class * Class.flag list * SMLClass.Class option * 
  SMLClass.Class list

(*----------------------------------------------------------------------*)
(* The full class definition.						*)
(*----------------------------------------------------------------------*)
type ClassDef =
  ClassInfo * FieldInfo list * MethodInfo list

(*----------------------------------------------------------------------*)
(* The type used for environments of internal class defns		*)
(*----------------------------------------------------------------------*)
type ClassEnv = 
  ClassDef ClassName.Map.map


end
