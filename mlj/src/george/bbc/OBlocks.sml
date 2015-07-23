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

(* OBlocks:>OBLOCKS contains code for constructing basic blocks, and converting
   them into items of type CodeAttr.code_data.

   Outline: OBlocks encode dataflow information.  They are supposed to be
   sufficiently low-level that optimisations tailored to the Java VM are
   apprpriate on them (in particular, stack and local variable allocation),
   yet sufficiently high-level to serve as an intermediate code should
   we need to write a compiler to some more conventional register-based
   architecture.

   Unlike most intermediate code, these blocks are remarkably pure;
   assignments are forbidden, and all jumps are effectively tail-calls.
   This is not because I like purity, but because as we are compiling to
   a stack + local variable architecture, we need to do some fairly complex
   dataflow analysis which would be complicated by assignments.  Given
   an assignment

   n:=[expression]

   this would normally be compiled on a register machine to code which
   puts the value of [expression] into the location of n, be it register or
   address.  For the Java VM this does not make sense, since it may well
   make sense to store n at this particular point on the stack.  Of course,
   I could unpick all the assignments in blocks, but this would be pointless
   for ML, since it's hard to see why it should be necessary to generate
   them in the first case (one might want to optimise accesses to refs, but
   it seems to me more appropriate to put the special code for them where
   the optimisation is done).


   Notes:

   Subroutines
   -----------

   Subroutines will not be implemented for a while, until I know how
   we want to use them.

   Exceptions
   ----------

   When an exception is thrown in a basic block, the only values which
   can be passed to that exception are arguments to the block or constants.

   This will hopefully not be too much of a problem in ML, where the
   equivalent of

   int i
   try {
      for (i [blah] ) {
         [blah]
         }
       } catch (Exception blurgh) {
         answer=i
         }

   is not possible.  But if it is desired to have the value of a local
   variable accessed by an exception changed, it will be necessary to split
   the block at the assignment.

   There will be two problems with exceptions as they will be implemented
   initially.
   (1) If two or more exceptions on the same block have the same handler,
       then the argument list specified for the exceptions must be the same.
       This is because the arguments must be put in fixed local variables
       where the handler can get at them.
   (2) If the same handler is used in two consecutive blocks, but with
       different values, it might be necessary to move values around to
       get them in the right place.  This movement will be done by
       instructions which cannot cause synchronous exceptions; for example
       load / store / stack operations / iinc (perhaps).
       If an exception were to occur during this movement, the arguments
       would be undefined.  Fortunately this can only occur with asynchronous
       exceptions; for example ThreadDeath.

   *)
structure OBlocks:>OBLOCKS where type block=Operations.block =
struct
(* Primary documentation for the meanings of these things is in the
   signature file *)

   (* Pick up sundry types from Operations *)

   open Operations

   val ctr=(ref 0)

   fun new_value ()=
   let
      val _=(ctr:= !ctr+1)
   in
      V(!ctr)
   end

   val new_constant_value=C

   exception Unordered
   structure ValueKey:>ORD_KEY where type ord_key=value =
   struct
      type ord_key=value
      fun compare (v1,v2)=
         (case (v1,v2) of
            (V n1,V n2) => Int.compare(n1,n2)
         |   _ => raise Unordered
         )
   end

   fun is_constant(C _)=true
   |   is_constant _ = false

   fun is_zero(C c)=Constants.is_zero c
   |   is_zero _ = false

   val block_counter=(ref 0)
   (* block_counter provides the ints which number the basic block *)
   fun make_block (p,q,r,s,t)=
   let
      val _= (block_counter:= !block_counter +1)
   in
      Operations.B(!block_counter,(p,q,r,s,t,NONE))
   end

   fun make_handler_block (p,q,r,s,t,h)=
   let
      val _= (block_counter:= !block_counter +1)
      val hh=
         (case #thrown_class h of
            NONE => {thrown= #thrown h,thrown_class=SOME ClassHandle.object}
         |  _ => h
         )
   in
      Operations.B(!block_counter,(p,q,r,s,t,SOME hh))
   end


   fun compile_block block=
      if !Variables.be_naive 
      then Naive.compile_block block
      else 
      let
         open Wire
         val dbvec=wire block
         val (freqb,freqbb)=find_freqs dbvec
         val iced=do_internal_compile { blocks=dbvec,block_freq_fun=freqb,label_freq_fun=freqbb }
         val order=resolve_code_motion { blocks=dbvec,label_freq_fun=freqbb,label_move_instructions=
            #label_move_instructions iced }
      in
         final_push {blocks=dbvec,internal_compiled=iced,order=order}
      end

   val todebug=ref (NONE:Wire.DBvec option)
   fun prepare_debug block=
      todebug:=SOME(Wire.wire block)

   fun toString i=
   let
      val SOME dbv= !todebug
   in
      Wire.db_toString(dbv,i)
   end
end


