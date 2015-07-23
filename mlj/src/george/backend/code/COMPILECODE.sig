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

(* The CompileCode structure contains the functions which translate
   values of type "Code.instruction list" to bytecode.

   There are currently 5 passes, plus another function (clear_labels)
   which should be done at the end, before another method is compiled:

   optimise_pass changes load to load0, store to store0, and push_XXX to
   im0 or im1 or im2 where appropriate.  (optimise_pass is optional)

   check_pass actually does nothing except make various checks on the
   validity of an instruction list.  If it discovers an error it raises
   Fail with an explanatory message beginning "check_pass: ".  It can
   be done either before or after optimise_pass but it must be done before
   the other 3 passes (because it will fail the internal instructions that
   pool_pass inserts).  Warning - check_pass does not currently
   do any modelling of
   the stack or the contents of local variables, since such errors should
   not cause errors in the other passes, and can be detected using javap
   on the output.  check_pass only looks at each individual instruction.
   (check_pass is optional).

   pool_pass puts entries into the constant pool, taking
   an instruction list to another instruction list paired with an
   exception_spec list, to be added to the exceptions for the containing
   code attribute.  pool_pass also processes the begin_catch, end_catch and
   handle_catch, replacing them by labels, and outputting an exception_spec
   list corresponding to them.

   label_pass finds the value of all the labels.  It also computes
   how wide all the instructions are going to be, so it takes
   the instruction list to a list of items of type width_data*int.
   (the instructions themselves are unchanged), where width_data contains
   information about where the instruction is short or long (2 byte
   operands or 1 byte operands) and, if its a table instruction, how many
   padding bytes to put in; and the int contains the length of the
   instruction.  Also label_pass lists all labels, outputting this as type
   u; this should then be fed to clear_labels.

   bytecode_pass outputs the bytecode, taking a pair of
   the instruction list (from pool_pass) and width_data list (from
   label_pass) to a Word8Vector.vector.

   clear_labels unsets all labels.
   This means that the same label can be used in two different
   methods without problems, provided that
   no other label_pass, bytecode_pass or clear_labels
   intervenes between this method's
   label_pass and clear_labels.
   Another advantage is that the same class can be compiled twice.

   There are other constraints on order which are relevant:
   1) For a given class, pool_pass should be done for ALL methods before
      label_pass is done for any method, and the pool should be
      resolved after the last pool_pass and before the first label_pass.
   2) The exception list should not be compiled for a method until the
      label_pass has been done.

   If passes are done in the wrong order, the effect is undefined, but
   normally incorrect output will not result; instead there will be
   matching failures or uncaught exceptions.

   *)

signature COMPILECODE=
sig
   type t
   type u

   val check_pass:Code.instruction list->unit
   val optimise_pass:Code.instruction list->Code.instruction list
   val pool_pass:AllPools.AllPools*Code.instruction list->
     Code.instruction list*TryCatch.t list
   val label_pass:Code.instruction list->t*u
   val bytecode_pass:Code.instruction list*t->W8.vector
   val clear_labels:u->unit
end



