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

(* Blocks:BLOCKS contains code for constructing basic blocks, and
   converting them into items of type CodeAttr.t.

   Outline: Blocks encode dataflow information.  They are supposed to be
   sufficiently low-level that optimisations tailored to the Java VM are
   apprpriate on them (in particular, stack and local variable
   allocation), yet sufficiently high-level to serve as an
   intermediate code should we need to write a compiler to some
   more conventional register-based architecture.

   Unlike most intermediate code, these blocks are remarkably pure;
   assignments are forbidden, and all jumps are effectively tail-calls.
   This is not because I like purity, but because as we are compiling to
   a stack + local variable architecture, we need to do some fairly
   complex dataflow analysis which would be complicated by
   assignments.  Given an assignment

   n:=[expression]

   this would normally be compiled on a register machine to code which
   puts the value of [expression] into the location of n, be it register
   or address.  For the Java VM this does not make sense, since it may well
   make sense to store n at this particular point on the stack.  Of course,
   I could unpick all the assignments in blocks, but this would be pointless
   for ML, since it's hard to see why it should be necessary to generate
   them in the first case (one might want to optimise accesses to refs, but
   it seems to me more appropriate to put the special code for them where
   the optimisation is done).


   Notes:

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

   is not possible. 

   Exceptions have priorities.  Where an exception is thrown which matches
   more than one exception handler, the exception handler with highest priority
   is the one that is jumped to.  The exception handlers for a basic block
   come in a list.  Earlier handlers in the list take priority over later
   handlers.


   *)
signature BLOCKS=
sig
   type block (* a block is a chunk of straight-line code *)

   (* NB.  When blocks are referred to in other blocks, they are usually
      stored as a block option ref rather than a block.  This is to make
      it possible to construct loops and other circular block arrangments;
      NONE can be put as a placeholder in the ref until the target has
      been defined. *)

   (* Each block has a descriptor giving the input types of its arguments.

      Note one important difference between block descriptors and
      method descriptors.  For non-static methods, the corresponding
      block descriptor should include as the first element of the input list
      the class type for this class. *)


   datatype block_descriptor=D of {input:Types.java_type list}

(* val compile_block:block->CodeAttr.t *)
   (* compile_block does the work.  It is only necessary to specify the
      block which begins the corresponding method, since compile_block
      pulls in all other blocks which that references.
      *)

   type value
   val new_value:unit->value
   val new_constant_value:Constants.constant->value
   (* a value is a handle to some value in the block; it may be one of the
      arguments to the block, or a constant, or something computed in the
      block.  new_constant_value should be used to construct values which
      are constants; new_value should be used to construct all other
      values.  Values produced by new_value are just handles; they mean
      nothing except in the context of a block which uses them in an
      argument or sets them as the result of a computation. 
      *)

   val is_zero:value->bool
   (* Returns true if value is a constant value satisfying
      Constants.is_zero.
      *)
   structure ValueKey:ORD_KEY sharing type ValueKey.ord_key=value
   exception Unordered
   (* The set of "ordered values", which includes at least the values
      created by new_value, is linearly ordered by ValueKey.compare.
      If either of the arguments to ValueKey.compare is not an ordered value,
      it raises Unordered.
      *)
   val is_constant:value->bool
   (* is_constant returns true if the value was created by new_constant_value
      *)

   (* A block is a sequence of instructions, together with an exit.  The
      execution of the block should be equivalent to the instructions being
      executed in sequence, followed by the exit being executed. *)

   type block_operation
   type instruction=block_operation*{input:value list,output:value list}
(* An instruction consists of an operation together with lists of input
   values and output values.  A value in a block is said to be _set_ at
   some point if it was declared with new_constant_value, or was an
   argument to the block, or has previously appeared in an input list.
   Immediately before the instruction, all the values in the input list
   should be set, and no value in the output list may be set.

   Each instruction has associated with it a partial function of
   the set of java_type list to the set of java_type list.
   The block compiler computes a partial function from values to
   java types using these functions and the types of the arguments.
   If there is a type mismatch, Fail is raised.

   We write the "type" of each operation in ML notation; eg
   int*int->int and so on.  N is the set of numeric types
   {int,long,float,double}.  M is the set of numeric types union
   {byte,char,short}.  I is the set {int,long}
   Where N occurs in a type, the type is the union
   of all types which can be got by choosing some n in N and replacing
   all the N's by that n; similarly for M.
   *)

   datatype Compulsory=MUSTDO|OPTIONAL
   (* Compulsory is used for instructions to indicate whether they might raise
      an exception, for example because the object we are reading a field from
      might actually be a null pointer *)

   (* arithmetic operations *)
   val add:block_operation (* N*N->N *)
   val sub:block_operation (* N*N->N *)
   val neg:block_operation (* N->N *)
   val mul:block_operation (* N*N->N *)
   val divide:Compulsory->block_operation (* N*N->N *)
   val rem:Compulsory->block_operation (* N*N->N *)
   
   (* logical instructions *)
   val bitwise_or:block_operation (* I->I *)
   val bitwise_and:block_operation (* I->I *)
   val bitwise_xor:block_operation (* I->I *)
   
   
   val shl:block_operation (* I*int->I *)
   val shr:block_operation (* I*int->I *)
   val ushr:block_operation (* I*int->I *)
   
   (* array accesses *)
   val aload:ArrayHandle.Handle*Compulsory->block_operation (* T[]*int->T *)
   val astore:ArrayHandle.Handle*Compulsory->block_operation (* T[]*int*S->unit, where S should be
                           assignment compatible (see VM book on
                           "aastore") *)
   
   (* Conversion *)
   val convert:Types.base_type->block_operation
   (* M->type corresponding to base_type, which should also be in M. *)
   (* convert converts from one numeric type to another.  This translates
      to 0,1 or 2 Java VM operations as required. *)
   
   val cmp:bool->block_operation (* N*N->int *)
   (* In the Java VM cmp is the only method of comparison for
      longs, floats and doubles, while if is the only method of
      comparison for ints.  However we implement both for
      all types of numbers, and try to use in all cases the
      most efficient translation.  So only use cmp if you
      really need a threeway comparison. *)
   
   val sign_extend:block_operation (* byte->byte, char->char or short->short *)
   (* sign_extend applies i2b, i2c or i2s (according to the argument type)
      to sign extend the input *)

   val new:MethodHandle.Handle->block_operation
   (* This operation does not correspond to a single bytecode, but is
      similar to the Java Language new operation.  Its purpose is to create an
      initialised object, given an initialisation method to use.
      The new object is created with the class of the method.
      
      If you want to call an init method on an object which already exists
      (EG inside an init method) use invoke_special instead.
      *)    
   
   val newarray:Types.java_type*Compulsory->block_operation (* some number of
      ints->the type corresponding to java_type *)
   (* In the Java VM, multidimensional arrays are implemented as
      arrays of arrays of . . [ to number of dimensions].  The supplied
      java_type should be an array type.  The input values should be
      between 1 and the dimension of the array in number, and should
      all be ints.  The operation allocates as many dimensions as are
      given by the values in the list.  This operation compiles to
      newarray or anewarray or multianewarray as appropriate. *)
   val arraylength:Compulsory->block_operation (* array reference->int *)
   
   val getfield:FieldHandle.Handle*Compulsory->block_operation
      (* class reference->type field *)
   val putfield:FieldHandle.Handle*Compulsory->block_operation
      (* class reference*type of field->unit *)
   val getstatic:FieldHandle.Handle*Compulsory->block_operation
      (* unit->type of field *)
   val putstatic:FieldHandle.Handle*Compulsory->block_operation
      (* type of field->unit *)
   
   val invoke_interface:MethodHandle.Handle->block_operation
   val invoke_special:MethodHandle.Handle->block_operation
   val invoke_static:MethodHandle.Handle->block_operation
   val invoke_virtual:MethodHandle.Handle->block_operation
   (* The types and numbers of values, plus the
      return type, if any, are determined by the handle.
      The list of values should
      have the object (if any) first, followed by the arguments of the
      method, in order. *)

   val checkcast:Types.java_type*Compulsory->block_operation
   (* reference->reference, of which the type is
      determined by the given reference type, or else throws an exception *)
   val instanceof:Types.java_type->block_operation
   (* reference->int *)

   val monitorenter:block_operation
   (* reference->unit *)
   val monitorexit:block_operation
   (* reference->unit *)

(* Now for exits. Blocks be only be left by an exit or by throwing
   an exception (which may be done inside a method which has been invoked,
   as a result of a checkcast, or for various other reasons). *)

   type exit
(* If we transfer control to another block via an exit or
   an exception, we specify a label. 
   A label should contain SOME(block,arguments).  Labels can be
   ref NONE so that it is possible to create loops *)

   type label=block option ref*value list

   val cond:{test:Tests.test,input:value list,yes:label,no:label}->exit
   (* Let the list be [a,b]; if a [test] b then goto yes else goto no.
      a and b should have the same type, which may be
      byte, char, short, int, long, float, double or reference.
      If the type is reference test may only be eq or ne.  For floats
      and doubles, NaN's on either side make the comparison false (this
      is as it should be for ML under the standard basis).  Restriction: Testing floats and doubles
      for inequality is not currently permitted.  (This is to be consistent we'd have to make
      x<>y be false if either x or y were NaN, and there is no quick way to ensure this in Java
      bytecode). *)

   val goto:label->exit
   val athrow:value list->exit

   (* I haven't decided how to do ret yet *)

   val lookupswitch:{lookuptable:(JavaInt.t*label) list,default:label,
      input:value list}->exit
   (* This is like the Java lookuptable operation.  However,
      it may also compile into tableswitch or a conditional if
      appropriate *)
   val return:value list->exit

(* Exceptions. *)
   datatype excep=E of ClassHandle.Handle option * label
   (* If the ClassHandle.Handle is supplied, it indicates the type of
      exception to catch; otherwise all exceptions are caught.
      When the exception is caught we jump to the block given by
      label with the supplied arguments.

      Note that the arguments of the label in an exception in
      a block must all be either arguments of the block, or constants.
       *)

   type handler_data=
   (* this is extra information attached to handler blocks *)
     {thrown:value, (* value to be attached to the exception object *)
      thrown_class:ClassHandle.Handle option
   (* class of the exception object.  This should be the same class as or a superclass of the
      thrown object.  If NONE is used, object is assumed *)
      }

(* Now how to construct blocks: *)
   val make_block:block_descriptor*value list*instruction list*exit*
      excep list->block
   val make_handler_block:block_descriptor*value list*instruction list*exit*
      excep list*handler_data->block
   (* the block_descriptor gives the type of the block + what will
      eventually be returned; the value list gives the arguments to
      the block (which should match the type); the instruction list gives
      the instructions in the block; the exit gives the way of leaving
      the block and the excep list gives the exceptions that apply to
      this block. *)

(* Compile it! *)
   val compile_block:block->CodeAttr.t
   (* compile_block does the work.  It is only necessary to specify the
      block which begins the corresponding method, since compile_block
      pulls in all other blocks which that references.
      *)

end



