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

signature FIELD=
sig
   datatype flag=PUBLIC|PRIVATE|PROTECTED|STATIC|FINAL|VOLATILE|TRANSIENT
   (* The flags for a field are given as a list of flags *)

   datatype field_data=simple of {
      name:JavaString.t, (* name of the field, in unqualified form *)
      field_type:Descriptors.field_descriptor,
      flags:flag list,
      attributes:Attribute.attributes
      }

   type t2 (* output of constant pool pass *)
   val pool_pass:AllPools.AllPools*field_data->t2
   val bytecode_pass:t2->W8.vector

   val decode_field:ReadPool.pool*BinIO.instream->field_data
end


