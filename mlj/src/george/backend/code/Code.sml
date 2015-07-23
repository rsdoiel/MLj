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

structure Code=
struct
(* this is a definition of the format in which the backend is
   given the code for Java methods.  *)

  (* abbreviations: top item of stack (whether int, double, or whatever)
      is
      S0, continues S1, S2,. ..  Top _word_ of stack (32 bits) is W0,W1, . .
      local variables are L0,L1,... *)

   (* In comments we abbreviate some types as follows:
      int    stands for F(0,INT)
      long   stands for F(0,LONG)
      (similarly for boolean,char,byte,short,float and double)
      An array is a type F(n,_) where n>0.
      A reference type is either F(0,CLASS _) or an array 
      *) 

   datatype instruction=
(* NB.  Constructors beginning internal_ should only be created by the
   backend itself during the passes that produce the bytecode.  However
   they are left available (for the moment) so that they can be used
   for testing. *)

      L of Labels.label
   (* NB, begin_catch, end_catch and handle_catch must all be specified
      exactly once for each exception.  L must be specified exactly once
      for each label.  Otherwise mysterious errors may result. *)
   |  begin_catch of TryCatch.t (* begin catching an exception *)
   |  end_catch of TryCatch.t (* end catching an exception *)
   |  handle_catch of TryCatch.t (* handle an exception *)

   |  nop (* no-op *)

   (* Arithmetic instructions.  The type must be int or long or float or double. *)
   |  add of Types.java_type
         (* outputs an add instruction *)
   |  sub of Types.java_type
         (* outputs a subtract instruction *)
   |  neg of Types.java_type
         (* ditto negate *)
   |  mul of Types.java_type
         (* ditto multiply *)
   |  divide of Types.java_type
         (* ditto divide *)
   |  rem of Types.java_type
         (* ditto remainder *)

   |  iinc of int*JavaInt.t
         (* int1 must be an unsigned integer.  L(int1) must have type int.
            int2 must be in [-32768,32767].  This instruction
            adds int2 to L(int1). *)

   (* Bitwise instructions.  The type must be int or long. *)
   |  bitwise_or of Types.java_type
 (* inclusive OR *)
   |  bitwise_and of Types.java_type
   |  bitwise_xor of Types.java_type

   (* Shift instructions.  For these, S0 must be an int and S1 a [type].
      S1 is shifted by a shift quantity equal to the low 6 bits of S0. *)
   |  shl of Types.java_type  (* shift left logically *)
   |  shr of Types.java_type  (* shift right with sign extension *)
   |  ushr of Types.java_type (* shift right logically *)

   (* Conversion functions *)
   |  convert of Types.java_type*Types.java_type
         (* The two java_types may be (1) distinct elements of {int,long,float,double}
            or (2) the first java_type may be int and the second
            short, byte or char.  The meaning is that S0 of type
            [java_type1] is replaced by the same value of [java_type2] *)

   (* Load and store functions.  The int must be non-negative.
      These instructions are automatically qualified by wide when that
      is necessary (the index of the local variable >255) *)
   |  load of Types.java_type*int (* Type should not be boolean, char, short or byte *)
         (* push L[int] as [type]. *)
   |  store of Types.java_type*int (* Type should not be boolean, char, short or byte *)
         (* pop [type] at top of stack to L[int] *)
   |  aload of Types.java_type (* java_type may be anything, including boolean *)
         (* S1 must be an array
            S0 must be an int.
            Replaces both by S1[S0] *)
   |  astore of Types.java_type (* java_type may be anything, including boolean *)
         (* S2 must be an array ([java_type] may be boolean), S1 an int,
            S0 a [java_type].
            Removes all and sets S2[S1]=S0 *)

   (* load0 and store0 are optimised versions of load and store
      for special cases.  Eventually the optimiser will replace load and
      store by these instructions where possible; do not do this outside
      the backend! *)

   (* java_type should not be boolean, char, short or byte.
      int should be 0,1,2 or 3 *)
   |  load0 of Types.java_type*int
   |  store0 of Types.java_type*int

   (* Branch instructions *)
   |  cond of Tests.test*Types.java_type*Labels.label
         (* java_type must be int or a reference type; if the latter,
            Tests.test may only be eq or ne.  S1 and S0 must be java_type's.
            If S1 [test] S0, branch to label. *)
   |  cond0 of Tests.test*Labels.label
         (* S0 must be an int.  If S0 [test] 0, branch to label. *)
   |  cmp of Types.java_type*bool
         (* java_type must be long, float or double.  If S0<S1, push
            int(1); if
            S0=S1, push int(0); if S0>S1, push int(-1). The boolean is
            ignored
            for long.  For float and double, it matters if
            either argument
            is a NaN; if it is and boolean is true, 1 is pushed on the
            stack;
            if false, -1 is pushed on the stack.
            *)
   |  ifnull of Labels.label
         (* branch if reference S0 is null *)
   |  ifnonnull of Labels.label
         (* branch if reference S0 isn't null *)

   (* Load constant functions.  Only one of these should now be used;
      the push function.  The others (push_XXX, internal_push_XXX,
      im0, im1 and im2) are for internal use only.
      *)
   |  push of Constants.constant


   |  push_int of JavaInt.t
   |  push_long of JavaLong.t
   |  push_float of JavaFloat.t
   |  push_double of JavaDouble.t
   |  push_string of JavaString.t
   |  push_null

   |  internal_push_int of AllPools.hndl
   |  internal_push_long of AllPools.hndl
   |  internal_push_float of AllPools.hndl
   |  internal_push_double of AllPools.hndl
   |  internal_push_string of AllPools.hndl

   (* Immediate constant functions *)
   |  im0 of Types.java_type*int
         (* push integer as an [java_type].
        
            possible values:

            java_type   int
        
            int         [-1,5]
            long        [0,1]
            float       [0,2]
            double      [0,1]
            reference   0 (pushed as null)
            *)

   (* im1 pushes an int in [-128,127]. im2 pushes an int in [-32768,32767]
      *)
   |  im1 of int (* this is "bipush" *)
   |  im2 of int (* this is "sipush" *)


   (* Stack operations

      These are all untyped but no operation should break up a value of a
      type; EG swap (which swaps the top two words) is illegal if the top
      item has 64-bit type.  But it's perfectly legal for more than 1 value
      to be dealt with at once; EG pop2 can be used to remove two 32 bit
      values at once. *)

   | pop (* removes top word *)
   | pop2 (* removes top 2 words *)

   | dup (* duplicate top word (... W0 =>... W0 W0) *)
   | dup_x1 (* ... W1 W0 => ... W0 W1 W0 *)
   | dup_x2 (* ... W2 W1 W0 => ... W0 W2 W1 W0 *)

   | dup2 (* ... W1 W0 => ... W1 W0 W1 W0 *)
   | dup2_x1 (* ... W2 W1 W0 => ... W1 W0 W2 W1 W0 *)
   | dup2_x2 (* ... W3 W2 W1 W0 => ... W1 W0 W3 W2 W1 W0 *)

   | swap (* ... W1 W0 => ... W0 W1 *)


   (* Switch Instructions *)

   | goto of Labels.label

   | athrow
   (* Throw the exception with reference S0.  The stack is emptied and
      S0 is put back on it.  *)

   | jsr of Labels.label
   | ret of int
   (* jsr pushes the address of the next opcode as a value of type
      returnAddress to the stack, and then branches to the
      supplied Labels.label.  ret(addr) branches to L(addr), which must be a return
      address.  returnAddress would appear to be a reference and so can
      be put in local variables (or taken out of them) using the load/store
      instructions.

      For restrictions on the use of jsr/ret see pages 123-4 of the
      Java book.  Note that recursive subroutines and multiple return points
      seem to be banned.  I don't think it is completely defined what is
      allowed.
      *)


   |  tableswitch of {low:JavaInt.t,high:JavaInt.t,jumptable:Labels.label list,
                      not_in_interval:Labels.label}
      (*
      If the int S0 is in [low,high], jump to element [S0-low] of jumptable,
      where element 0 is the first element.  Otherwise jump to

      not_in_interval.  The jumptable should have high-low+1 elements.
      *)
   |  lookupswitch of {lookuptable: (JavaInt.t*Labels.label) list,default:Labels.label}
      (* If the int S0 appears in the list, paired with a label, jump to
         that label, otherwise jump to default (if it appears multiple times
         I suppose any of the matching labels get jumped to??).

         NB - the lookuptable should already be sorted in increasing
         order of JavaInt.t's. *)

   (* Return instructions *)

   |  return of Types.java_type
      (* java_type may be int,long,float,double,or reference.
         Returns the operand on the top of the stack (the rest of the stack
         is discarded). *)
   |  return_void
      (* Returns void, discarding the stack. *)

   (* Allocation and arrays *)
   |  new of ClassHandle.Handle (* Allocate reference to new class and push
      it *)
   |  internal_new of AllPools.hndl

   |  anewarray of Types.java_type
      (* S0 must be an int.  java_type should be an array type.  Construct a single-
         dimensional array of this type with dimension S0. *)
   |  internal_anewarray of AllPools.hndl
   |  newarray of Types.java_type
      (* new_array should now only be used internally.

         S0 must be an int and java_type should not be reference (it may,
         however, be boolean).
         Allocate an array of S0 elements of type [java_type]
         and push reference *)

   |  multianewarray of Types.java_type*int
      (* The second argument should be the number of dimensions; it should
         be in
         [1,255].  That number of values are popped off the stack (they
         should all be non-negative ints) and used as dimensions.  An array
         object with type that supplied and
         these dimensions is constructed and a reference to it pushed.
          *)
   |  internal_multianewarray of AllPools.hndl*int

   |  arraylength
      (* S0 should be a reference to an array; its length is pushed *)

   (* Accessing elements of classes *)

   |  getfield of Descriptors.field_ref
      (* Gets the field from the object with reference S0 and
         pushes it *)
   |  putfield of Descriptors.field_ref
      (* Sets the field from S0 *)
   |  getstatic of Descriptors.field_ref
      (* Gets the static field *)
   |  putstatic of Descriptors.field_ref
      (* Sets the static field *)

   |  internal_getfield of AllPools.hndl
   |  internal_putfield of AllPools.hndl
   |  internal_getstatic of AllPools.hndl
   |  internal_putstatic of AllPools.hndl

   (* Method access *)

   |  invoke_interface of Descriptors.interface_method_ref*int
      (* N=[int] should be in [1,255].  S_{N-1} should be a reference.
         S_{N-2}. . S_{0} are arguments (in that order).  All these are
         removed from the stack and a method is called based on S_{N-1};
         if it's an object, the method is searched for in that object; if
         it's an array type, the method is searched for in Object.

         The method is then called in the ordinary way, with the object ref
         in L0 and the arguments in L1,. . .

         See the
         VM book page 259 for more details. .  *)
   |  internal_invoke_interface of AllPools.hndl*int

   |  invoke_special of Descriptors.method_ref
      (* The method is resolved (this is done before the bytecode is run),
         taking it from the closest superclass.
         (see page 261 for more details).  The number of its arguments is
         determined from this; it and the object should be on the stack with
         the same format as for invoke_interface.  The method is then
         called as for invoke_interface. *)
   |  internal_invoke_special of AllPools.hndl

   |  invoke_static of Descriptors.method_ref
      (* The method (which should be static) is resolved and called.
         The arguments are taken off the
         stack, with the last argument at the top, and put in local variables
         as for invoke_interface. *)
   |  internal_invoke_static of AllPools.hndl

   |  invoke_virtual of Descriptors.method_ref
      (* The method is invoked as for invoke_special, except that the
         class comes directly from the object. .
         See page 267 for more details.
         *)
   |  internal_invoke_virtual of AllPools.hndl


   (* Class comparisons.
      The argument of checkcast and instanceof should be a class or
      array type. *)

   |  checkcast of Types.java_type
      (* If the object S0 is null or
         can be cast to the type, nothing is done
         (S0 is not removed); otherwise CheckCastException is thrown. *)
   |  instanceof of Types.java_type
      (* The reference S0 is removed.  If it is not null and is an instance
         of the given type, the integer 0 is pushed; otherwise 1 is
         pushed. *)

   |  internal_checkcast of AllPools.hndl
   |  internal_instanceof of AllPools.hndl


   (* Synchronization *)


   |  monitorenter
      (* A monitor is entered for S0 (which must be an non-null
         object reference).
         No more than one thread is allowed to be in the monitor for the same
         object at any time; however a thread may be in a monitor for
         an object several times over. *)
   |  monitorexit
      (* A monitor is left for S0 (which must be non-null).  Precisely what
         happens isn't clear if no monitor has been entered. *)

   |  stack_size of int
      (* stack_size should not be used in code passed to the backend.  However, it is
         used by the basic block code to store information about the current stack size. *)

end
