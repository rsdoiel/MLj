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

(* Descriptors:>DESCRIPTORS contains the datatype declarations for
   field and method descriptors, and the code for turning them into
   and out of JavaStrings.
   *)
signature DESCRIPTORS=
sig
   type field_descriptor=Types.java_type
   val field_descriptor_equal:field_descriptor*field_descriptor->bool

   datatype method_descriptor=M of Types.java_void_type*
      (Types.java_type list)
      (* The list contains descriptors of the arguments; the optional
         argument is the return type *)

   type java_string=JavaString.t

   datatype field_ref=fref of {
      class:ClassHandle.Handle,
      name :JavaString.t, (* name of the field *)
      desc :field_descriptor
                         (* type of the field *)
      }

   datatype method_ref=mref of {
      class:ClassHandle.Handle,
      name :JavaString.t, (* name of the method *)
      desc :method_descriptor
                         (* type of the method *)
      }

   datatype interface_method_ref=iref of {
      class:ClassHandle.Handle,
      name :JavaString.t, (* name of the interface method *)
      desc :method_descriptor
                         (* type of the interface method *)
      }
  
   val is_init:method_ref->bool

   exception TooManyDimensions
   (* raised if there are more than 255 dimensions for an array, which
      is outlawed by the VM book in section 4.10 *)
   val fdout:field_descriptor->JavaString.t
   val mdout:method_descriptor->JavaString.t

   (* fdin and mdin use the Assert functions if the argument cannot be 
      completely parsed as a field descriptor or method descriptor 
      respectively. *)
   val fdin:JavaString.t->field_descriptor
   val mdin:JavaString.t->method_descriptor

   (* the XXX_toString functions are intended for debugging purposes only and
      should not be used in production code or in any other way relied on. *)
   val field_descriptor_toString:field_descriptor->string
   val method_descriptor_toString:method_descriptor->string
   val field_ref_toString:field_ref->string
   val method_ref_toString:method_ref->string
   val interface_method_ref_toString:interface_method_ref->string
end   
   


