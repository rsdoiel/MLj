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
(* Special compilation for 1+ types.					*)
(*======================================================================*)
signature COMPILEONEPLUS =
sig

(*----------------------------------------------------------------------*)
(* Given a type ty, return the instructions and value necessary to	*)
(* generate inl <> : 1+ty                                               *)
(*----------------------------------------------------------------------*)
val none : 
  CompileOps.Env -> 
  MILTy.Type ->
  Blocks.instruction list * Blocks.value

(*----------------------------------------------------------------------*)
(* Given a type ty and a value for v, return the instructions and value	*)
(* necessary to generate inr <v> : 1+ty                                 *)
(*----------------------------------------------------------------------*)
val some :
  CompileOps.Env ->
  MILTy.Type * Blocks.value ->
  Blocks.instruction list * Blocks.value

(*----------------------------------------------------------------------*)
(* Given a type ty and a value for inr <v>, return the instructions and *)
(* value corresponding to v that are required for the projection.       *)
(*----------------------------------------------------------------------*)
val proj :
  CompileOps.Env ->
  MILTy.Type * Blocks.value ->
  Blocks.instruction list * Blocks.value

(*----------------------------------------------------------------------*)
(* Generate the instructions necessary to create all dummy none values.	*)
(*----------------------------------------------------------------------*)
val makeNones :
  unit -> 
  Blocks.instruction list

(*----------------------------------------------------------------------*)
(* List the global fields used for all dummy none values.		*)
(*----------------------------------------------------------------------*)
val makeNoneFields :
  unit ->
  Field.field_data list

val init : unit -> unit

end