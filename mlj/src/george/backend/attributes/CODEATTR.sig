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

signature CODEATTR=
(* the functions in CodeAttr produce a code attribute *)
sig
   type t={
	 instructions:Code.instruction list,
	 (* setting max_stack_words or max_locals to NONE will currently
	    cause a matching error but this may change if I get around to
	    writing some code to compute them automatically. *)

	 max_stack_words:int option,
	 (* Maximum number of words that will be on the operand stack *)
	 max_locals:int option
	 (* Maximum number of local variables (NB that each one contains a
	    word so that doubles and longs need two) *)
	 }

   val name:JavaString.t

   type t2 (* result of pool_pass *)
   val pool_pass:(AllPools.AllPools*t)->t2
   val bytecode_pass:t2->W8.vector

   val check_instructions:bool ref
      (* set this to control whether instructions are checked.  Defaults to
         true.  Setting it false will save computation time but then incorrect
         input may result in mysterious errors, matching failures or
         even wrong output. *)
   val do_minor_optimise:bool ref
      (* set this to control whether we do minor optimisation.  Defaults to
         true.  Setting it false will probably save very little time, and
         in fact compilation may take longer. *)
end
