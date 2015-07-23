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
(* ML type variables with sorts						*)
(*======================================================================*)
signature TYVAR =
sig

(*----------------------------------------------------------------------*)
(* Type variables (Section 4.1, p16 Defn)				*)
(*----------------------------------------------------------------------*)
type TyVar and Supply

datatype Sort =
  Normal of TySort.Sort
| Overloaded of TyName.Set.set

structure Set : ORD_SET where type Key.ord_key = TyVar
structure Map : ORD_MAP where type Key.ord_key = TyVar

(* From the source: either ordinary or equality *)
val explicit : Syntax.symbol -> TyVar

(* For error message purposes *)
val toString : TyVar -> string

val isExplicit : TyVar -> bool

(* What's its sort? *)
val sort     : TyVar -> Sort

val eq       : TyVar * TyVar -> bool

(* Fresh type variable supply *)
val fresh    : Sort -> Supply -> TyVar*Supply
val initial  : Supply

end


