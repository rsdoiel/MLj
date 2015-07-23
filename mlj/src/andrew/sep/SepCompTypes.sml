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
(* Types used for separate compilation.                                 *)
(*======================================================================*)
structure SepCompTypes =
struct

datatype Result = 
  Sig of Env.Sig

| Str of 
  {
    (* The SML environment to which the structure elaborates *)
    E : Env.Env,

    (* Types of classes defined by this structure *)
    CE : SMLTy.ClassEnv,

    (* Exception bindings for this structure *)
    EE : SMLTy.ExEnv,

    (* Map from imported structure IDs to MIL variables *)
    strVars : Var.Var Symbol.OrdMap.map,

    (* Maximum variable index used for structure IDs *)
    limit : Var.Supply,

    (* The MIL variable supply *)
    supply : Var.Supply,

    (* The MIL term *)
    term : MILTerm.Cmp,

    (* MIL type defs for this module *)
    tynameTys : MILTy.Type TyName.Map.map,

    (* The type of the MIL term *)
    ty : MILTy.Type

  }

| JavaStr of Env.Env

type Cache = (Result * Entity.FileRef option) Entity.Map.map

end