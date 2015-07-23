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
(* Global free variable counts for `current' term.			*)
(* Assumption 1: bound variables in the term are distinct.              *)
(* Assumption 2: the census is used in a single threaded way; there's   *)
(* only one census so no backtracking is allowed.                       *)
(*======================================================================*)
signature CENSUS =
sig

(* Clear all entries in the census to zero *)
val clearCensus : unit -> unit

(* Initialise census with info about term specified *)
val initCensus : MILTerm.Cmp*Var.Var -> unit

(* What's the highest numbered variable in use? *)
val maxVar : unit -> Var.Var

(* Generate a fresh variable with the number of occurrences specified *)
val freshVar : int -> Var.Var

(* Increase the census for a variable by the number given *)
val addVar : Var.Var * int -> unit

(* Return the current count for the variable given *)
val getVar : Var.Var -> int

(* Increase the census by the specified amount for every (non-bound) 
   variable occurrence in a computation term *)
val addCmp : MILTerm.Cmp * int -> unit

(* Increase the census by the specified amount for every (non-bound) 
   variable occurrence in a value term *)
val addVal : MILTerm.Val * int -> unit

(* Kill a variable; it must no longer even appear bound in the term *)
val removeVar : Var.Var -> unit

(* Kill all bound variables in e, decrement count of free variables *)
val removeCmp : MILTerm.Cmp -> unit
val removeTAbstr : MILTerm.TAbstr -> unit
val removeAbstr : MILTerm.Abstr -> unit

(* Mark a variable as `inlined' *)
val inlineVar : Var.Var -> unit

(* Check that the census is consistent with the term given *)
(* Report any discrepancies to the log file *)
val checkCmp : MILTerm.Cmp -> unit

(* Duplicate a term by renaming all bound variables and increasing the census
   appropriately *)
val renameCmp    : MILTerm.Cmp -> MILTerm.Cmp
val renameTAbstr : MILTerm.TAbstr -> MILTerm.TAbstr

end