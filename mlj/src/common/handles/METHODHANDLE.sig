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

signature METHODHANDLE=
sig
   type Handle
   val class:Handle->ClassHandle.Handle
   val name:Handle->JavaString.t 
   val input_types:Handle->Types.java_type list
   val output_type:Handle->Types.java_type option (* NONE for void *)

   val unknown:{class:ClassHandle.Handle,name:JavaString.t,
      input_types:Types.java_type list,output_type:Types.java_void_type} -> 
      Handle

   (* Now for various service functions *)
   (* reads, writes, exceptional and unbounded classify various impure 
      properties the method may have.  If they don't know, they should return 
      true. 

      Definitions.  "MUTABLE" means data that may be written or read
      via field or array handles which are mutable, or other data
      which may change (EG position of a file pointer).  "external" means
      data which is accessed outside the method.  *)
   val reads:Handle->bool
   (* Returns true if the method reads external MUTABLE data, or a static field *)
   val allocs:Handle->bool
   (* Returns true if the method writes a MUTABLE field in
      an external cell that was created during its execution. *)
   val writes_first:Handle->bool
   (* Returns true if the method writes to a MUTABLE field in
      in its first argument (this only matters for init methods). *)
   val writes:Handle->bool
   (* Returns true if the method writes some external MUTABLE data, or a static field *)
   val exceptional:Handle->bool
   (* Returns true if the method raises an exception *)
   val unbounded:Handle->bool
   (* Returns true if the method doesn't return *) 

   (* For notes on synchronize see the Notes file, section 2 *)
   val synchronize:Handle->bool
     
   val is_init:Handle->bool
   (* is_init returns true if and only if the method is an <init> method. *)

   val method_handle_toString:Handle->string
   (* method_handle_toString returns a string representation of the handle.
      This function is for debugging purposes only. *)
end
