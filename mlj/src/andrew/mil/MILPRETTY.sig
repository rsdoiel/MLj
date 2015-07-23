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
(* Pretty-print MIL types and terms					*)
(*======================================================================*)
signature MILPRETTY =
sig

(* Maximum indentation depth before "..." are displayed *)
val printDepth : int ref

val valToString     : MILTerm.Val -> string
val cmpToString     : MILTerm.Cmp -> string
val tabstrToString  : MILTerm.TAbstr -> string
val fundefToString  : MILTerm.FunKind * MILTerm.FunDef -> string
val classdefToString: 
  MILTy.Type * MILTerm.ClassInfo * MILTerm.FieldInfo list * 
    MILTerm.MethodInfo list -> string
val kindToString    : MILTerm.FunKind -> string

(*
val dumpCmp         : (string -> unit) -> unit
val dumpVal         : (string -> unit) -> unit
*)

val failVal         : MILTerm.Val -> string -> 'a
val failCmp         : MILTerm.Cmp -> string -> 'a

end

