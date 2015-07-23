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

(* The Operations structure contains the datatype which describes all the
   operations available to the user in Blocks (see BLOCKS.sig) plus
   internal ones.  It also contains sundry other datatypes referred to in
   Blocks.  As this structure only contains datatype declarations, there
   is no point
   having a corresponding signature, since if there were one it would look
   exactly the same. *)
structure Operations=
struct
   (* We omit comments for items described in BLOCKS *)

   datatype value=V of int
               |  C of Constants.constant

   type constant_value=Constants.constant
   datatype Mutability=MUTABLE|IMMUTABLE
   datatype Compulsory=MUSTDO|OPTIONAL
   type AccessOrder=Mutability*Compulsory

   (* Now for the operations themselves.  Internal ones are commented by
      INTERNAL *)
   datatype operation=
(* MEMORANDUM.  MakeDag uses catch-all clauses to when classifying
   operations using the is_ functions.  Thus, if the set of operations is
   extended, I must remember to check the is_ functions to ensure they
   still work.
   *)
       add|sub|neg|mul|divide of Compulsory|rem of Compulsory
      |add_constant of constant_value (* INTERNAL *)
      |mul_constant of constant_value (* INTERNAL *)
      |divide_constant of constant_value (* INTERNAL *)
      |rem_constant of constant_value (* INTERNAL *)
      |bitwise_or|bitwise_and|bitwise_xor|shl|shr|ushr
      |aload of AccessOrder|astore of AccessOrder
      |convert of Types.base_type
      |cmp of bool
      |sign_extend
      |new of Descriptors.method_ref|newarray of Types.java_type*Compulsory
      |arraylength of Compulsory
      |getfield of Descriptors.field_ref*AccessOrder
      |putfield of Descriptors.field_ref*AccessOrder
      |getstatic of Descriptors.field_ref*AccessOrder
      |putstatic of Descriptors.field_ref*AccessOrder
      |invoke_interface of Descriptors.interface_method_ref
      |invoke_special of Descriptors.method_ref
      |invoke_static of Descriptors.method_ref
      |invoke_virtual of Descriptors.method_ref
      |checkcast of Types.java_type*Compulsory
      |instanceof of Types.java_type
      |monitorenter of Compulsory|monitorexit of Compulsory

      |vale of {n_args:int} (* INTERNAL *)
     (* The vale operation is a fictional one put at the end of a basic
        block.  The argument list consists of n_args values which must
        be put on the stack to work out conditional jumps (for example
        the two values to compare if the exit is a cond), or
        to return, followed by
        the values which are required by one or more of the labels
        which can be jumped to.  *)

   datatype block_descriptor=D of {input:Types.java_type list,
                                      output:Types.java_type list}

   type class_ref=ClassHandle.Handle

   type handler_data=
   (* this is extra information attached to handler blocks *)
     {thrown:value, (* value to be attached to the exception object *)
      thrown_class:class_ref option
   (* class of the exception object.  This should be the same class as or a superclass of the
      thrown object.  If NONE is used, object is assumed *)
      }

   type instruction=operation*{input:value list,output:value list}

   (* The following datatypes are all mutually recursive. *)

   datatype
      block=B of int*(block_descriptor*value list*instruction list*exit*
         excep list*handler_data option)
   (* The integer bundled with the basic block is a unique integer assigned
      by the make_block routine.  This integer is used for comparing
      basic blocks, so for example when we construct the flow graph of the
      whole function we can discover if we have already visited a basic
      block.

      For everything apart from make_block, which arranges it, we assume that
      if handler_data is present, the value of thrown_class inside it is not NONE. *)
   and
      excep=E of class_ref option * label
   and
      exit=
         cond of
            {test:Tests.test,
             yes:label,
             no:label,
             input:value list
             }
      |  cond0 of
            {test:Tests.test,
             yes:label,
             no:label,
             input:value list
             }
      |  goto of label
      |  athrow of value list
      |  lookupswitch of
            {lookuptable: (JavaInt.t*label) list,
             default:label,
             input:value list
             }
      |  tableswitch of
            {jumptable:label list,
             low:JavaInt.t,
             not_in_interval:label,
             input:value list
             }
      |  return of value list
   withtype
      label=block option ref*value list

   (* the XXX_toString functions are intended for debugging purposes only and
      should not be used in production code or in any other way relied on. *)

   fun mutability_toString m=
      (case m of
         MUTABLE=>"M"
      |  IMMUTABLE=>"I"
      )

   fun compulsory_toString c=
      (case c of
         MUSTDO=>"m"
      |  OPTIONAL=>"o")

   fun accessorder_toString(m,c)=
      mutability_toString m ^ compulsory_toString c

   fun operation_toString Op=
      (case Op of
         add=>"add"
      |  sub=>"sub"
      |  neg=>"neg"
      |  mul=>"mul"
      |  divide c=>"divide "^compulsory_toString c
      |  rem c=>"divide "^compulsory_toString c
      |  add_constant const=>"addc "^Constants.constant_toString const
      |  mul_constant const=>"mulc "^Constants.constant_toString const
      |  divide_constant const=>"divc "^Constants.constant_toString const
      |  rem_constant const=>"remc "^Constants.constant_toString const
      |  bitwise_or=>"bitwise_or"
      |  bitwise_and=>"bitwise_and"
      |  bitwise_xor=>"bitwise_xor"
      |  shl=>"shl"
      |  shr=>"shr"
      |  ushr=>"ushr"
      |  aload a=>"aload "^accessorder_toString a
      |  astore a=>"astore "^accessorder_toString a
      |  convert a=>"convert "^Types.base_type_toString a
      |  cmp b=>"cmp "^(if b then "g" else "l")
      |  sign_extend=>"sign_extend"
      |  new h=>"new "^
            Descriptors.method_ref_toString h
      |  newarray(t,c)=>"newarray "^Types.java_type_toString t^" "^
              compulsory_toString c
      |  arraylength c=>"arraylength "^compulsory_toString c
      |  getfield (f,a)=>"getfield "^accessorder_toString a^" "^
            Descriptors.field_ref_toString f
      |  putfield (f,a)=>"putfield "^accessorder_toString a^" "^
            Descriptors.field_ref_toString f
      |  getstatic (f,a)=>"getstatic "^accessorder_toString a^" "^
            Descriptors.field_ref_toString f
      |  putstatic (f,a)=>"putstatic "^accessorder_toString a^" "^
            Descriptors.field_ref_toString f
      |  invoke_interface m=>"invoke_interface "^
            Descriptors.interface_method_ref_toString m
      |  invoke_special m=>"invoke_special "^
            Descriptors.method_ref_toString m
      |  invoke_static m=>"invoke_static "^
            Descriptors.method_ref_toString m
      |  invoke_virtual m=>"invoke_virtual "^Descriptors.method_ref_toString m
      |  checkcast (jt,c)=>"checkcast "^compulsory_toString c^
            Types.java_type_toString jt
      |  instanceof jt=>"instanceof "^
            Types.java_type_toString jt
      |  monitorenter c=>"monitorenter "^compulsory_toString c
      |  monitorexit c=>"monitorexit "^compulsory_toString c
      |  vale {n_args=n_args,...}=>
            "vale "^Int.toString n_args
      )
end
