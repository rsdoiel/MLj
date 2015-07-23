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
(* Paths through terms.                                                 *)
(*======================================================================*)
structure MILPath =
struct

datatype Item = 
  LetFun of Var.Var                     (* rhs of function defn *)
| LetClass of Var.Var                   (* rhs of method defn *)
| Cond of bool                          (* branch of conditional *)
| CaseCon of int option                 (* branch of case *)
| CaseSCon of Constants.constant option (* branch of scon-case *)
| CaseExCon of MILTy.Type option        (* branch of exn-case *)

(* The head of the list is the deepest node in the corresponding term *)
type Path = Item list

end

