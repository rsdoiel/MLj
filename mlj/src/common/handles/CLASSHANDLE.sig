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

signature CLASSHANDLE = 
sig
   type Handle
   val name : Handle -> JavaString.t
   val unknown : JavaString.t -> Handle
   val equal : Handle * Handle -> bool
   val superclass : Handle -> Handle option
   val is_interface : Handle -> bool option
   val maybe_subclass : Handle * Handle -> bool
   (* maybe_subclass(X,Y) should return true if X is a subclass of Y *)  

   (* ClassHandle includes handles for Object, String and all the standard
     exception classes. *)
   val object:Handle
   val string:Handle
   val cloneable:Handle
   val arithmetic_exception:Handle
   val array_index_out_of_bounds_exception:Handle
   val array_store_exception:Handle
   val biginteger:Handle
   val class_cast_exception:Handle
   val class_not_found_exception:Handle
   val clone_not_supported_exception:Handle
   val Exception:Handle
   (* we have to avoid confusion with the ML reserved word "exception" *)
   val illegal_access_exception:Handle
   val illegal_argument_exception:Handle
   val illegal_monitor_state_exception:Handle
   val illegal_state_exception:Handle
   val illegal_thread_state_exception:Handle
   val index_out_of_bounds_exception:Handle
   val instantiation_exception:Handle
   val interrupted_exception:Handle
   val negative_array_size_exception:Handle
   val no_such_field_exception:Handle
   val no_such_method_exception:Handle
   val null_pointer_exception:Handle
   val number_format_exception:Handle
   val runtime_exception:Handle
   val security_exception:Handle
   val serializable:Handle
   val string_index_out_of_bounds_exception:Handle
   val throwable:Handle

   val class_handle_toString:Handle->string
   (* class_handle_toString outputs a string representation of a class handle.
      It should only be used for debugging purposes. *)
end























