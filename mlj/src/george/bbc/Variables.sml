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

(* Variables contains various functions which are called to determine
   priorities &c in such a way that they can be tweaked later. *)
structure Variables=
struct
   (* We attempt (in Interblock) to assign relative probabilities to each
      basic block.  This is done probabilistically, by constructing a random
      walk on the blocks.  The weight_label function assigns numbers to
      each label;
      the ratio of the transition probabilities out of a basic block should
      be the same as the ratio of the numbers. *)

   (* label_type describes the label; the weight_label function here
      uses the type.  *)
   datatype label_kind=
      TEST of Tests.test
(* Indicates which test has to be satisfied for this label (ignoring
   NaNs). *)
   |  SWITCH of int*bool
(* The integer is the number of branches in the switch.  The bool is TRUE if
   this is not the label for the default option. *)
   |  GOTO
   |  EXCEPTION

   type label_type=label_kind*bool
   (* The bool is TRUE if this label is part of a cycle; IE there is
      a control path from the target of the label back to the source of the
      label. *)

   val weightlabel:(label_type->real) ref =
      ref (fn(EXCEPTION,true)=>0.1
      |  (_,true)=>10.0
      |  (TEST t,false)=>
            if Tests.eq_yes t then 1.0 else 0.5
      |  (SWITCH (n,false),false)=>
             (case n of
                 0=>1.0
             |   1=>0.5
             |   2=>0.333
             |   3=>0.25
             |   n=>0.2
             )
      |  (SWITCH (n,true),false)=>
             (case n of
                 1=>0.5
             |   2=>0.333
             |   3=>0.25
             |   n=>0.8/Real.fromInt(n)
             )
      |  (EXCEPTION,false)=>0.01
      |  (GOTO,false)=>1.0
      )

   val jump_random: real ref=ref 0.001 (* used in jump_cost.  *)

   val jump_cost:(label_kind->real) ref=
   (* This is a guess at the cost of doing a label of this kind, as opposed to falling through.
      Note that the numbers here get multiplied by the estimated number of times we do the label. *)
      ref(fn EXCEPTION => !jump_random
         |  TEST t => 1.0
         |  SWITCH(n,_) =>
               if n<=1 then 1.0 (* it will get compiled to an if *)
               else !jump_random
         |  GOTO  => 1.0
         )

   val jump_cost_added:real ref=
   (* jump_cost for gotos at the end of blocks inserted to move locals around *)
      ref 1.0
   val code_copy_cost:real ref=
      ref 2.0
   (* code_copy_cost is the extra cost corresponding to code size which gets added to
      the weighting used for label arguments as the penalty if argument and parameter
      are in different locals. *)

   val leakage:real ref=ref 0.000001
   (* See Wire.sml *)

   val push_constants_before:bool ref=ref false
   (* If possible, push constants for arithmetic before their arguments *)

   val do_1dim_as_newarray=ref true
   (* allocate a 1 dimensional array of array objects using anewarray rather than
      multianewarray (false corresponds to Java 1.0's behaviour but true with what
      the VM book seems to recommend). *)

   val do_monitors=ref true
   (* monitorenter and monitorexit now work *)

   val fix_netscape3_bug1=ref true
   (* If true, insert a nop instruction at the end of methods which would
      otherwise end in an end_catch instruction *)

   val sparseness=ref 3 (* see do_table *)

   val do_table=ref
      (* true if a lookupswitch with nentries where the difference between the highest and lowest
         entry is range should get compiled into a tableswitch *)
      (fn {nentries:int,range:int}=> range div (! sparseness)<=nentries)
      (* There is a time-space tradeoff here.  tables will in the worst case (IE not with an
         intelligent JIT) be much faster than lookupswitches; on the other hand
         if 13+4*range > 9+8*nentries the lookupswitch will require fewer bytecodes. *)

   val be_naive=ref false
      (* set to true to use the Naive basic block code *)
   val do_elision=ref true
      (* set to false to avoid eliding load/store pairs *)
   val moveup_needs=ref true
      (* set to false to avoid moving needs arrows upwards *)

   val do_peephole=ref true
      (* set to false to avoid peephole optimisation. (Mostly iincs). *)

   datatype local_merge_strategy=ALWAYS|NEVER|SAMETYPE
   val local_merge_strategy=ref ALWAYS
   (* NEVER means never merge variables for locals.
      ALWAYS means always do it if possible (using register colouring).
      SAMETYPE means only do it between locals of the same type.
      ALWAYS should (nearly) always produce better code; the other cases are only
      there for compiler development and dealing with broken JITs.
      *)

   val symantec_bug=ref false
   (* If true, stop exception handlers being before the exception they handle *)
end
