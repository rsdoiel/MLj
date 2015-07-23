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
(* MIL exceptions.                                                      *)
(* These are used *only* for ML exception types, that is, those         *)
(* created by a declaration of the form                                 *)
(*    exception id                                                      *)
(* Java exceptions are just classes.                                    *)
(*======================================================================*)
signature MILEXN =
sig

type Exn

val hash          : Exn -> word
val eq            : Exn * Exn -> bool
val compare       : Exn * Exn -> order
val toString      : Exn -> string
val info          : Exn -> Syntax.longid

val exn           : Entity.Ref * int * Syntax.longid -> Exn

end


