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
(* Operations on Java class definitions                               	*)
(*======================================================================*)
signature SMLCLASSDEFOPS =
sig

  exception ClassNotFound of string

  val tyToClassDef : 
    SMLTy.ClassEnv -> SMLTy.Type -> SMLTy.ClassDef

  val subClass     : 
    SMLTy.ClassEnv -> SMLTy.Type * SMLTy.Type -> bool

  val checkExplicit : Syntax.longid -> bool
  val hasMain      : SMLTy.ClassDef -> bool

  type Cache = SMLTy.ClassDef Symbol.OrdMap.map 
  val cache : Cache ref

  val classToLongid : JavaString.t -> Syntax.longid
  val longidToClass : Syntax.longid -> JavaString.t

end

