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

structure CodeAttr:>CODEATTR=
struct
   local
      type hndl=unit->int
      open Numbers
      open W8
   in
      val check_instructions=ref true
      val do_minor_optimise=ref true

      val name=JavaString.fromString "Code"

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

      datatype t2=T of {
         max_stack_words:int,
         max_locals:int,
         instructions:Code.instruction list,
         exceptiontable:CompileExceptionTable.t2
         }

      fun pool_pass(A,{max_stack_words,max_locals,instructions}:t)=
      let
         val () =
            if !check_instructions
            then
               CompileCode.check_pass instructions
            else
               {}
         val optimised_instructions=
            if !do_minor_optimise
            then
               CompileCode.optimise_pass instructions
            else
               instructions

         val (pooled_instructions,exceptiontable)=
            CompileCode.pool_pass(A,optimised_instructions)
      in
         T {max_stack_words=(case max_stack_words of
               SOME i => i
               ),
            max_locals=(case max_locals of
               SOME i => i
               ),
            instructions=pooled_instructions,
            exceptiontable=CompileExceptionTable.pool_pass(A,exceptiontable)
            }
      end

      fun bytecode_pass(
         T {max_stack_words,max_locals,instructions,exceptiontable})=
      let
         val (labelled,labels)=CompileCode.label_pass instructions
         val instructionbytecode=
            CompileCode.bytecode_pass(instructions,labelled)
         val ibl=W8.length(instructionbytecode)
         val exceptionbytecode=
            CompileExceptionTable.bytecode_pass exceptiontable
         val ()=CompileCode.clear_labels(labels)
      in W8.concat[
         W8.fromvList[
            u2 max_stack_words,
            u2 max_locals,
            u4 ibl
            ],
         instructionbytecode,
         exceptionbytecode,
         W8.vv1(w2(0w0)) (* no attributes *)
         ]
      end (* let *)
   end (* local *)
end (* struct *)



