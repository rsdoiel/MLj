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
(* Environment used during free variable gathering.			*)
(*======================================================================*)
structure FVInfoEnv =
struct

datatype EnvInfo = 
  FunInfo of MILTerm.FunKind
| Global
| Other
| GlobalRef

type Env = 
{
  (* Types and locality of values *)
  tyenv : (MILTy.Type*EnvInfo) Var.Map.map,

  (* Type variable kinds *)
  kindenv :  MILTy.Kind Var.Map.map,

  (* Are we top-level? (Top level = not inside a true function) *)
  toplevel : bool
}

fun envPlusVars ({ tyenv, kindenv, toplevel } : Env) (xs, tys) =
  { 
    tyenv = 
      ListPair.foldl (fn (x,ty,tyenv) => Var.Map.insert(tyenv, x, (ty,Other)))
      tyenv (xs, tys), 
    kindenv = kindenv, 
    toplevel = toplevel 
  } : Env

fun envPlusTyVars ({ tyenv, kindenv, toplevel } : Env) tyvars =
  {
    tyenv = tyenv,
    kindenv = 
      foldl (fn ((x,k),kindenv) => Var.Map.insert(kindenv, x, k))
      kindenv tyvars,
    toplevel = toplevel
  } : Env

fun envPlusTypedVars ({ tyenv, kindenv, toplevel } : Env) (xs, kind) =
  {
    tyenv = 
      List.foldl (fn ((x,ty),tyenv) => Var.Map.insert(tyenv, x, (ty,kind)))
      tyenv xs, 
    kindenv = kindenv, 
    toplevel = toplevel
  } : Env

fun envNotTop ({ tyenv, kindenv, toplevel } : Env) = 
  {
    tyenv = tyenv, kindenv = kindenv, toplevel = false
  } : Env

fun isTop ({tyenv, kindenv, toplevel} : Env) = toplevel

val emptyEnv = 
{
  tyenv = Var.Map.empty, 
  kindenv = Var.Map.empty, 
  toplevel = true
} : Env

fun lookup ({ tyenv, toplevel, kindenv } : Env, x) = Var.lookup(tyenv, x)

end
