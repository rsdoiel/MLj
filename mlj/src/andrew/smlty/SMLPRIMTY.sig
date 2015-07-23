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
(* SML primitive types							*)
(*======================================================================*)
signature SMLPRIMTY =
sig

val boolType   : SMLTy.Type
val charType   : SMLTy.Type
val exnType    : SMLTy.Type
val intType    : SMLTy.Type
val int64Type  : SMLTy.Type
val listType   : SMLTy.Type -> SMLTy.Type
val optionType : SMLTy.Type -> SMLTy.Type
val orderType  : SMLTy.Type
val realType   : SMLTy.Type
val real32Type : SMLTy.Type
val stringType : SMLTy.Type
val substringType : SMLTy.Type
val unitType   : SMLTy.Type
val wordType   : SMLTy.Type
val word8Type  : SMLTy.Type
val word64Type : SMLTy.Type
val vectorType : SMLTy.Type -> SMLTy.Type

end
