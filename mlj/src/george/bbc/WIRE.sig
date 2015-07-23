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

(* Wire:WIRE constructs and manipulates connections between basic block
   Dags. *)
signature WIRE=
sig
   type DBvec

   val wire:Operations.block->DBvec
   (* wire takes the block at the head of the method and turns it and all blocks it refers to
      into Dag blocks, numbered consecutively from 0, and returns the vector containing them all,
      with Dag block i as element i.
      *)

   (* the XXX_toString functions are intended for debugging purposes only and
      should not be used in production code or in any other way relied on. *)
   val db_toString:DBvec*int -> string
   (* db_toString takes a dbvec and an integer argument and outputs a
      string describing Dag block number i *)

   type label_data= (* this is the data returned by find_children for each label of a block *)
     {destination:Dag.DagBlock,
      kind:Variables.label_kind,
      arguments:Dag.DagNode list, (* arguments supplied to destination, in order *)
      id:int (* Number of the label *)
      }


   val find_children:Dag.DagBlock->label_data list
   (* find_children finds the Dag blocks to which there are labels from this block, together with the
      kind of label. *)

   (* find_freqs does a crude flow analysis to estimate the number of times each block is called each
      time the method is called. *)
   val find_freqs:DBvec->
      (Dag.DagBlock->real)*(Dag.DagBlock->(real*label_data) list)
   (* find_freqs returns two functions (f1,f2) say.  f1(block) returns the guess of the number of times
      this block is called.  f2(block) returns a list of label_data for
      labels out of the block, paired with the
      number of times we expect to go through that label. *)

   type internal_compiled=
     {instructions:Code.instruction list Vector.vector,
      max_locals:int,
      max_stack_words:int,
      common_move_instructions:Code.instruction list Vector.vector,
      label_move_instructions:Code.instruction list Vector.vector
      }

   val do_internal_compile:
     {blocks:DBvec,
      block_freq_fun:Dag.DagBlock->real,
      label_freq_fun:Dag.DagBlock->(real*label_data) list
      } -> internal_compiled
(* do_internal_compile compiles all the operations, except exit instructions and those required for
   copying arguments to blocks, which can't be done until we have decided what order to output the
   instructions in.  It takes the block vector found by wire and the two frequency functions
   found by find_freqs, and returns a vector, with entries corresponding to those in
   the DagBlock vector, containing the corresponding compiled instructions,
   max_locals which is the maximum number of locals used (this allows for the fact that longs and
   doubles use two locals), max_stack_words, the maximum number of stack words required, and
   label_move_instructions.  label_move_instructions contains instructions required while going to
   a label to get locals in the right place.  Let L=Vector.sub(label_move_instructions,i).  If
   L=[], no instructions are required, and the label can go right to the start of the relevant block;
   otherwise instructions L must be done first. *)

   datatype borlabid=
      BLOCK of int
   |  LABM of int

   val resolve_code_motion:
      {blocks:DBvec,
       label_freq_fun:Dag.DagBlock->(real*label_data) list,
       label_move_instructions:Code.instruction list Vector.vector
       } -> borlabid list
   (* resolve_code_motion takes the data produced by earlier functions in this structure and produces
      the borlabid list, which contains BLOCK i for block number i, LABM for the motion code for
      label i (if any), in the order in which they are to appear in the final compiled method. *)

   val final_push:
     {blocks:DBvec,
      internal_compiled:internal_compiled,
      order:borlabid list} -> CodeAttr.t
   (* Compute the final output *)
end
