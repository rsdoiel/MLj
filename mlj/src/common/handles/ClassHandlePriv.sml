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

(*======================================================================*)
(* Private class handle operations            				*)
(*======================================================================*)
structure ClassHandlePriv :> CLASSHANDLEPRIV = 
struct
   datatype Handle = 
     Unknown of JavaString.t
   | Known of JavaString.t * Handle
   
   structure C =
   struct
      type Handle = Handle
      
      val unknown=Unknown
      
      fun name (Unknown s) = s
        | name (Known (s,_)) = s
      
      fun equal (h1 : Handle, h2 : Handle) =
         JavaString.equal(name h1,name h2)

      val biginteger = Unknown (JavaString.fromString "java/math/BigInteger")
      val serializable = Unknown (JavaString.fromString "java/io/Serializable")

      val 
      [
         object,
         string,
         cloneable,
         arithmetic_exception,
         array_index_out_of_bounds_exception,
         array_store_exception,
         class_cast_exception,
         class_not_found_exception,
         clone_not_supported_exception,
         Exception,
         illegal_access_exception,
         illegal_argument_exception,
         illegal_monitor_state_exception,
         illegal_state_exception,
         illegal_thread_state_exception,
         index_out_of_bounds_exception,
         instantiation_exception,
         interrupted_exception,
         negative_array_size_exception,
         no_such_field_exception,
         no_such_method_exception,
         null_pointer_exception,
         number_format_exception,
         runtime_exception,
         security_exception,
         string_index_out_of_bounds_exception,
         throwable
      ] = map (fn s => Unknown (JavaString.fromString("java/lang/" ^ s)))
      [
         "Object",
         "String",
         "Cloneable",
         "ArithmeticException",
         "ArrayIndexOutOfBoundsException",
         "ArrayStoreException",
         "ClassCastException",
         "ClassNotFoundException",
         "CloneNotSupportedException",
         "Exception",
         "IllegalAccessException",
         "IllegalArgumentException",
         "IllegalMonitorStateexception",
         "IllegalStateException",
         "IllegalThreadStateException",
         "IndexOutOfBoundsException",
         "InstantiationException",
         "InterruptedException",
         "NegativeArraySizeException",
         "NoSuchFieldException",
         "NoSuchMethodException",
         "NullPointerException",
         "NumberFormatException",
         "RuntimeException",
         "SecurityException",
         "StringIndexOutOfBoundsException",
         "Throwable"
      ]
      
      fun superclass (Unknown _) = NONE
        | superclass (Known (_, super)) = SOME super
      
      fun is_interface (Unknown _) = NONE
        | is_interface (Known _) = SOME false
      
      fun maybe_subclass(X,Y)=
         (case superclass Y of
            NONE=>true
         |  SOME _ =>
            let
               fun is_subclass Z =
                  if equal(Z,Y)
                  then true
                  else
                     (case superclass Z of
                        NONE=>false
                     |  SOME new_Z=>is_subclass new_Z
                     )
            in
               is_subclass X
            end
         )
            
      fun class_handle_toString h=
      let
         fun get_name js=
            CharVector.map
               (fn #"/" => #"."
               |  x => x)
               (JavaString.toMLString js)
      in
         (case h of
            Unknown js => get_name js
         |  Known(js,_) => get_name js
         )
      end 
   end (* of substruct *)
end (* of struct *)



