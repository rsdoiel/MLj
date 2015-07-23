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
(* Gather free variable information prior to closure converting.        *)
(*======================================================================*)
signature FVINFO =
sig

type FunInfo = 
{
  kind : MILTerm.FunKind option,     (* NONE = method *)
  tyvars : (Var.Var*MILTy.Kind) list,(* Bound type variables *)
  args : MILTerm.TypedVar list,      (* Arguments *)
  cty  : MILTy.CmpType,              (* Result type *)
  fvs  : FVInfoOps.VarsInfo          (* Free variable info *)
} Var.Map.map

val gather : 
  MILTerm.Cmp -> 
  {
    fvs : FVInfoOps.VarsInfo,        (* Free variable info for top level *)
    funs : FunInfo,                  (* Info for functions and methods *)
    resulttys : MILTy.CmpType Var.Map.map (* Result types for non-rec funs *)
  }

val dumpFunInfo : FunInfo -> unit

end
