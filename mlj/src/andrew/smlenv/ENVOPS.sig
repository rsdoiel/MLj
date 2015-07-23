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
(* Operations on environments used in type inference			*)
(*======================================================================*)
signature ENVOPS =
sig

(*----------------------------------------------------------------------*)
(* Empty environments        						*)
(*----------------------------------------------------------------------*)
val emptyE : Env.Env
val emptyB : Env.Basis

(*----------------------------------------------------------------------*)
(* Apply a TyName `renaming' to various environments                    *)
(*----------------------------------------------------------------------*)
val renameVE : TyName.Renaming -> Env.ValEnv -> Env.ValEnv
val renameTE : TyName.Renaming -> Env.TyEnv -> Env.TyEnv
val renameE  : TyName.Renaming -> Env.Env -> Env.Env

(*----------------------------------------------------------------------*)
(* Apply a realisation to various environments                          *)
(*----------------------------------------------------------------------*)
val appRealisationVE : SMLTy.Realisation -> Env.ValEnv -> Env.ValEnv
val appRealisationTE : SMLTy.Realisation -> Env.TyEnv -> Env.TyEnv
val appRealisationE  : SMLTy.Realisation -> Env.Env -> Env.Env
val appExMapE        : SMLTy.ExName SMLTy.ExMap.map -> Env.Env -> Env.Env

(*----------------------------------------------------------------------*)
(* Type names in environments       					*)
(*----------------------------------------------------------------------*)
val tynamesVE : Env.ValEnv -> TyName.Set.set
val tynamesTE : Env.TyEnv -> TyName.Set.set
val tynamesSE : Env.StrEnv -> TyName.Set.set
val tynamesE  : Env.Env -> TyName.Set.set

(*----------------------------------------------------------------------*)
(* Selection of environment components					*)
(*----------------------------------------------------------------------*)
val VEofE : Env.Env -> Env.ValEnv
val TEofE : Env.Env -> Env.TyEnv
val SEofE : Env.Env -> Env.StrEnv
val EofB  : Env.Basis -> Env.Env
val GofB  : Env.Basis -> Env.SigEnv
val CofB  : Env.Basis -> Env.Context
val FofB  : Env.Basis -> Env.FunEnv
val EofC  : Env.Context -> Env.Env
val pathofC : Env.Context -> Syntax.longid
val lamofC  : Env.Context -> bool
val classofC : Env.Context -> TyName.TyName option
val pathofB : Env.Basis -> Syntax.longid
val UofC  : Env.Context -> TyVar.Set.set

(*----------------------------------------------------------------------*)
(* Inclusion of environment components					*)
(*----------------------------------------------------------------------*)
val VEinE : Env.ValEnv -> Env.Env
val TEinE : Env.TyEnv -> Env.Env
val SEinE : Env.StrEnv -> Env.Env
val VETEinE : Env.ValEnv*Env.TyEnv -> Env.Env
val EinB  : Env.Env -> Env.Basis

(*----------------------------------------------------------------------*)
(* Equality on environments						*)
(*----------------------------------------------------------------------*)
val eq      : Env.Env*Env.Env -> bool

(*----------------------------------------------------------------------*)
(* Extension of environment components					*)
(*----------------------------------------------------------------------*)
val EplusVE : Env.Env -> Env.ValEnv -> Env.Env
val EplusTE : Env.Env -> Env.TyEnv -> Env.Env
val EplusSE : Env.Env -> Env.StrEnv -> Env.Env
val BplusE  : Env.Basis -> Env.Env -> Env.Basis
val BplusG  : Env.Basis -> Env.SigEnv -> Env.Basis
val CplusE  : Env.Context -> Env.Env -> Env.Context
val CplusTE : Env.Context -> Env.TyEnv -> Env.Context
val CplusVE : Env.Context -> Env.ValEnv -> Env.Context
val CplusU  : Env.Context -> TyVar.Set.set -> Env.Context
val lamC    : Env.Context -> Env.Context
val CwithClass : Env.Context -> TyName.TyName -> Env.Context
val BplusStr: Env.Basis -> Syntax.symbol -> Env.Basis
val BnoPath : Env.Basis -> Env.Basis

(*----------------------------------------------------------------------*)
(* Merging of environments						*)
(*----------------------------------------------------------------------*)
val EplusE  : Env.Env -> Env.Env -> Env.Env
val VEplusVE : Env.ValEnv -> Env.ValEnv -> Env.ValEnv
val TEplusTE : Env.TyEnv -> Env.TyEnv -> Env.TyEnv
val SEplusSE : Env.StrEnv -> Env.StrEnv -> Env.StrEnv

val freeVE : Env.ValEnv -> TyVar.Set.set

(*----------------------------------------------------------------------*)
(* Pretty-print								*)
(*----------------------------------------------------------------------*)
val EtoString : Env.Env -> string
val classDefToString : SMLTy.Type * SMLTy.ClassDef -> string
val EasSig: Env.Env -> string

end
