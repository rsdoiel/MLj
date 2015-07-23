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

(* AllPools contains the code for gluing together all the constant
   pools, plus functions for adding particular items to the
   constant pool and sorting out all the handles. *)
signature ALLPOOLS=
sig
   type AllPools (* type of the whole constant pool for a class *)

   (* The rule is that first an AllPools item is created (using "create),
      then things are added to it, using the "r_XXX function (with just one
      argument, to return a unit->int function), then resolved (using
      "resolve), and finally the locations of the items in the pool
      can be found by calling the results of the r_XXX functions with
      a unit argument.  Resolving the pool twice, or doing r_XXX after
      it is resolved, or doing the result of an r_XXX before it is resolved,
      has undefined effect (probably an exception will be raised)>
      *)

   val create:unit->AllPools (* create a new constant pool *)

   (* hndl is a convenient abbreviation used for the only kind of handle functions outside
      the constant pool have to deal with, namely a function unit->int which does a lookup
      on a Handle.pool_handle. *)
   type hndl=unit->int

   val r_Utf8:AllPools*JavaString.t->hndl
   val r_int:AllPools*JavaInt.t->hndl
   val r_long:AllPools*JavaLong.t->hndl
   val r_float:AllPools*JavaFloat.t->hndl
   val r_double:AllPools*JavaDouble.t->hndl
   val r_string:AllPools*JavaString.t->hndl
   val r_class:AllPools*ClassHandle.Handle->hndl
   val r_array:AllPools*Types.java_type->hndl
   val r_field:AllPools*Descriptors.field_ref->hndl
   val r_method:AllPools*Descriptors.method_ref->hndl
   val r_interface_method:
      AllPools*Descriptors.interface_method_ref->hndl

   val resolve:AllPools->W8.vector
       (* resolve produces a W8.vector encoding the whole constant pool *)

end
