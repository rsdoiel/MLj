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

(* the Attribute structure contains general code for constructing
   attributes. *)
signature ATTRIBUTE=
sig
   type attributes
   
   val empty:attributes

   (* The setXXXX functions are used to set attributes.
      Each of them may only be used once on an attribute. *)
   val setExceptions:Exceptions.t*attributes->attributes
   (* Set the exceptions a method can throw.  (May only be used for
      methods) *)
   val setConstantValue:ConstantValue.t*attributes->attributes
   (* Set a constant value for a field.  (May only be used for fields
      of appropriate type.) *)
   val setSignature:Signature.t*attributes->attributes
   (* Set a GJ Signature.  (May be used for classes, methods, and fields.)
      *)
   val setDeprecated:Deprecated.t*attributes->attributes
   (* Mark a field or method as deprecated.  (Currently Deprecated.t
      is unit.) *)

   val setCode:CodeAttr.t*attributes->attributes
   (* Set the code for a method.  (May only be used for methods) *)

   (* The getXXXX functions return the settings for various attributes
      (or NONE if there is no attribute present). 
      getCode is not implemented. 
      *)
   val getExceptions:attributes->Exceptions.t option
   val getConstantValue:attributes->ConstantValue.t option
   val getSignature:attributes->Signature.t option
   val getDeprecated:attributes->Deprecated.t option

   (* Here are functions used by the rest of the backend *)
   type t2 (* Output of pool_pass *)
   val pool_pass:(AllPools.AllPools*attributes) -> t2
   val bytecode_pass:t2 -> W8.vector

   val decode_attributes:ReadPool.pool*BinIO.instream->attributes
   (* decode_attributes decodes the attributes starting at some
      point in the file. 
      *)
end






