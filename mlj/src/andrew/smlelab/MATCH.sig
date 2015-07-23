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
(* Signature matching							*)
(*======================================================================*)
signature MATCH =
sig

(*----------------------------------------------------------------------*)
(* First stage of signature matching: check structural stuff (if 	*)
(* signature defines an entity (var, type, substructure) then structure *)
(* should define it too). Also do instantiation of tynames to typefcns. *)
(*----------------------------------------------------------------------*)
val match1 : 
  Syntax.Location ->
  Env.Env * Env.Sig -> 
  SMLTy.Realisation * SMLTy.ExName SMLTy.ExMap.map

(*----------------------------------------------------------------------*)
(* Second stage of signature matching: enrichment.			*)
(* Wrt elaboration, this is just a check (does it match?) but we also   *)
(* require a term in which polymorphic variables are specialised        *)
(* appropriately.                                                       *)
(*----------------------------------------------------------------------*)
val match2 :
  Syntax.symbol * Syntax.Location ->
  Env.Env * Env.Env ->
  SMLTerm.StrExp 

end
