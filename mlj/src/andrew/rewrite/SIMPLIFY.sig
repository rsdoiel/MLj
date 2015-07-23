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
(* Simplification of MIL terms						*)
(*======================================================================*)
signature SIMPLIFY =
sig

(*----------------------------------------------------------------------*)
(* Input:								*)
(*   a basis B=(DE,EE) with pervasive datatype and exception envs       *)
(*   a type environment mapping free variables to their types           *)
(*   a pair (ce,supply) in which the bound variables of ce are distinct *)
(*   and fresh variables can be generated from supply.                  *)
(* Output:                                                              *)
(*   a pair (ce,supply) with the same properties as the input.          *)
(*----------------------------------------------------------------------*)
val simplify : 
{ 
  removeEncaps : bool, 
  doInlineEq : bool,
  doComplex : bool,
  doBranches : bool
} 
-> Opt.Transformer

end