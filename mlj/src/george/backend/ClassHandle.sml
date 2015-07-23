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

(* DEBUGGING version of CLASSHANDLE!!  The extra features are
   (1) the cref function, allowing the caller to construct arbitrary
       classes.
   (2) four artificial classes, classA,classB,classC,classD, such that
       A is a subclass of B and B is a subclass of C and D *)
structure ClassHandle:>CLASSHANDLE =
struct
  type Handle=JavaString.t
  fun equal(x:JavaString.t,y)=JavaString.equal(x,y)
  fun name x=x
  fun one_of_ours x=false
  fun unknown x=x

  val classA=JavaString.fromString("Debugging/A")
  val classB=JavaString.fromString("Debugging/B")
  val classC=JavaString.fromString("Debugging/C")
  val classD=JavaString.fromString("Debugging/D")

  fun superclass t=
     if equal(t,classB) then SOME classA
     else if equal(t,classC) then SOME classB
     else if equal(t,classD) then SOME classB
     else NONE

  val object=JavaString.fromString "java/lang/Object"
  val string=JavaString.fromString "java/lang/String"
  val cloneable=JavaString.fromString "java/lang/Cloneable"

  fun is_interface t=
     if equal(t,classA) then SOME false
     else if equal(t,classB) then SOME false
     else if equal(t,classC) then SOME false
     else if equal(t,classD) then SOME false
     else if equal(t,object) then SOME false
     else if equal(t,string) then SOME false
     else if equal(t,cloneable) then SOME true
     else NONE

   fun cref(x)=JavaString.fromString(x)

   fun cc x=JavaString.fromString("java/lang/" ^ x)
   (* temporary abbreviation, meaning "Construct Class" *)

   val arithmetic_exception                 =cc "ArithmeticException"
   val array_index_out_of_bounds_exception  =cc "ArrayIndexOutOfBoundsException"
   val array_store_exception                =cc "ArrayStoreException"
   val class_cast_exception                 =cc "ClassCastException"
   val class_not_found_exception            =cc "ClassNotFoundException"
   val clone_not_supported_exception        =cc "CloneNotSupportedException"
   val Exception                            =cc "Exception"
   val illegal_access_exception             =cc "IllegalAccessException"
   val illegal_argument_exception           =cc "IllegalArgumentException"
   val illegal_monitor_state_exception      =cc "IllegalMonitorStateException"
   val illegal_state_exception              =cc "IllegalStateException"
   val illegal_thread_state_exception       =cc "IllegalThreadStateException"
   val index_out_of_bounds_exception        =cc "IndexOutOfBoundsException"
   val instantiation_exception              =cc "InstantiationException"
   val interrupted_exception                =cc "InterruptedException"
   val negative_array_size_exception        =cc "NegativeArraySizeException"
   val no_such_field_exception              =cc "NoSuchFieldException"
   val no_such_method_exception             =cc "NoSuchMethodException"
   val null_pointer_exception               =cc "NullPointerException"
   val number_format_exception              =cc "NumberFormatException"
   val runtime_exception                    =cc "RuntimeException"
   val security_exception                   =cc "SecurityException"
   val string_index_out_of_bounds_exception =cc "StringIndexOutOfBoundsException"
end
