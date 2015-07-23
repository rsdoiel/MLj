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

(* This is the version of CLASSHANDLE for debugging.  NB the function cref
   which provides a sneaky way of constructing a class *)
signature CLASSHANDLE =
sig

   type Handle
   val equal : Handle * Handle -> bool
   val name : Handle -> JavaString.t
   val superclass : Handle -> Handle option
   val is_interface : Handle -> bool option
   val unknown : JavaString.t -> Handle
   (* unknown is used by the Decode code, and is so called because the
      classes come from outside and we don't know anything about them. *)
 
   (* ClassHandle includes handles for Object, String and all the standard
     exception classes. *)
   val object:Handle
   val string:Handle
   val cloneable:Handle
   val arithmetic_exception:Handle
   val array_index_out_of_bounds_exception:Handle
   val array_store_exception:Handle
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
   val string_index_out_of_bounds_exception:Handle

   (* The following additional members of ClassHandle are only used for
      debugging and should not be implemented for the production version
      of ClassHandle. *)

   val cref: string->Handle
   (* classA,B,C,D are imaginary classes such that B is a direct subclass of
      A, and C,D are direct subclasses of B. *)
   val classA:Handle
   val classB:Handle
   val classC:Handle
   val classD:Handle


end
