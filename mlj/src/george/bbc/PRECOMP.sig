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

(* Precomp:PRECOMP does the "preliminary compilation" of a DagBlock into backend instructions.  That is
   to say, it produces the backend instructions which do the computations in the block (with the exception
   of any involved in the exit instruction itself - I am not yet sure how this can be dealt with best),
   but the local numbers output are unlikely to be the ones in the final code (indeed they may
   not even work; longs and doubles will not use two locals as they do in real life).  Precomp attempts
   no optimisation of locals at all; it uses a new local number each time it has a new value to store,
   with the single exception of when it uses the iinc instruction and the local in question is no
   longer needed.

   Precomp also contains the move_locals function, which takes a list of local copies to do
   and returns the instruction list for doing those copies, and the do_exits function,
   which compiles exits.  *)
signature PRECOMP=
sig
   val precomp:
     {db:Dag.DagBlock,
      first_local:int
(* Use local numbers from first_local onwards.  At the entry to the block the first argument is in
   local number first_local, and so on. *)
       }
      ->
     {instructions:Code.instruction list,
         (* instructions which perform the computations in the basic block.  At the beginning the
            stack is empty; at the end it contains only the arguments needed for the vale instruction.
            (I am planning to change the basic block code to pass arguments between blocks on the stack,
            but haven't yet sorted out how). *)
      max_stack_words:int, (* Maximum number of stack words used *)
      final_stack_words:int,
      (* size of the stack at the end of this code (IE the size of the arguments to the exit function *)
      first_local:int,
      next_local:int, (* We used local numbers from first_local to next_local-1 *)
      getargs:Dag.DagNode->int,
         (* For non-constant nodes quoted as arguments in labels in the block
            this function returns the local number used. *)
      gettype:int->Types.java_type
         (* gettype l returns the type of local variable number l *)
      }

   val move_locals:
      {from:int,to:int,java_type:Types.java_type} list *
      {from:Constants.constant,to:int} list *
      int ->
     {instructions:Code.instruction list,
      max_stack_words:int
      }
   (* move_locals returns code+stack size so that for each {from,to} the contents of
      L(to) at the end contain the constant from,
      or alternatively the value of type java_type that was in L(from) at the beginning of the code.
      from should never be the same as to.  The last argument is the stack size at the start of
      this (which is 0 unless this is the beginning of an exception handler, when it will be 1).
       *)

   val do_exit:
     {block:Dag.DagBlock,
      getlabel:int->Labels.label option
      (* getlabel i is SOME lab if to do label i we jump lab, and NONE if we fall through. *)
      }->
     {instructions:Code.instruction list,
      max_stack_words:int
      }
end
