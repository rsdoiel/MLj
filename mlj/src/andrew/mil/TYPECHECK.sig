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
(* Type checking of MIL terms						*)
(*======================================================================*)
signature TYPECHECK =
sig

val term : MILTerm.Cmp option ref

(*----------------------------------------------------------------------*)
(* Use check b (DE,EE) tyenv ce to type check a computation             *)
(* term ce with respect to the environments described above. Return the *)
(* computation type of the term or raise a Fail exception if there      *)
(* is a type error.                                                     *)
(*----------------------------------------------------------------------*)
val check : 
  bool -> 
  {
    kindenv : MILTy.Kind Var.Map.map, (* Map from tyvars to kinds *)
    tyenv   : MILTy.Type Var.Map.map, (* Map from vars to types *)
    funenv  : MILTy.Type Var.Map.map, (* Map from vars to global fun types *)
    globenv : MILTy.Type Var.Map.map  (* Map from vars to global ref types *)
  } ->
  MILTerm.Cmp -> 
  MILTy.CmpType

end