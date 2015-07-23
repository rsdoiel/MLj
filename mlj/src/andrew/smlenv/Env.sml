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
(* Semantic objects as defined in section 4.2 and 5.1 of the Defn.      *)
(*======================================================================*)
structure Env =
struct

(*----------------------------------------------------------------------*)
(* Type environments (TE in TyEnv = TyCon -> TyStr)                     *)
(*----------------------------------------------------------------------*)
type TyEnv = TyStr.TyStr Symbol.OrdMap.map

(*----------------------------------------------------------------------*)
(* Value environments (VE in ValEnv = VId -> TypeScheme*IdStatus)       *)
(*----------------------------------------------------------------------*)
type ValEnv = ValBind.Bind Symbol.OrdMap.map		

(*----------------------------------------------------------------------*)
(* Environments (E or (SE,TE,VE) in Env = StrEnv * TyEnv * ValEnv)      *)
(*----------------------------------------------------------------------*)
datatype Env = 
  Env of StrEnv * TyEnv * ValEnv

(*----------------------------------------------------------------------*)
(* Structure environments (SE in StrEnv = StrId -> Env)   		*)
(*----------------------------------------------------------------------*)
withtype StrEnv = Env Symbol.OrdMap.map

(*----------------------------------------------------------------------*)
(* Context (C or T,U,E in Context = TyNameSet * TyVarSet * Env)		*)
(* We make some extensions: a flag to say whether or not we are under   *)
(* a lambda, the current path (for info only), the current class.       *)
(*----------------------------------------------------------------------*)
type Context = 
  TyName.Set.set * TyVar.Set.set * Env * 
  bool * Syntax.longid * TyName.TyName option

(*----------------------------------------------------------------------*)
(* Signatures (sigma in Sig = TyNameSet * Env)                          *)
(*----------------------------------------------------------------------*)
type Sig = TyName.Set.set * Env

(*----------------------------------------------------------------------*)
(* Signature environments (G in SigEnv = SigId -> Sig)                  *)
(*----------------------------------------------------------------------*)
type SigEnv = Sig Symbol.OrdMap.map

(*----------------------------------------------------------------------*)
(* Functor signatures (Phi in FunSig = TyNameSet * (Env * Sig)          *)
(*----------------------------------------------------------------------*)
type FunSig = TyName.Set.set * (Env * Sig)

(*----------------------------------------------------------------------*)
(* Functor environments (F in FunEnv = FunId -> FunSig)			*)
(*----------------------------------------------------------------------*)
type FunEnv = FunSig Symbol.OrdMap.map

(*----------------------------------------------------------------------*)
(* Basis (B or (T,F,G,E) in Basis = TyNameSet * FunEnv * SigEnv * Env)  *)
(* We make one extension: the current path (for info only)              *)
(*----------------------------------------------------------------------*)
type Basis = TyName.Set.set * FunEnv * SigEnv * Env * Syntax.longid


end

