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

signature ELAB = sig

type Sig = 
  SMLTy.DatEnv * SMLTy.ExEnv * Env.Sig

type Str = 
  SMLTy.DatEnv * SMLTy.ExEnv * SMLTy.ClassEnv * SMLTy.Realisation * 
  Env.Env * SMLTerm.StrExp

val infTopSigExp :
  Env.Basis * SMLTy.ClassEnv -> Syntax.SigBind -> Sig * Error.Error list

val infTopStrExp :
  Env.Basis * SMLTy.ClassEnv -> Syntax.StrBind -> Str * Error.Error list

end