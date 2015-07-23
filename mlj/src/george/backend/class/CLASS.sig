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

signature CLASS=
sig
   datatype flag=PUBLIC|FINAL|INTERFACE|ABSTRACT

   datatype class_data=
(*
      simple of {
         this:ClassHandle.Handle, (* name of this class *)
         flags:flag list,
         super:ClassHandle.Handle option,
            (* name of superclass.  If NONE there is no superclass
               (this must be java/lang/Object!).
               *)
         fields:Field.field_data list,
         methods:Method.method_data list
         }
*)
      middling of { (* this allows interfaces too! *)
         this:ClassHandle.Handle, (* name of this class *)
         flags:flag list,
         super:ClassHandle.Handle option,
            (* name of superclass.  If NONE there is no superclass
               (this must be java/lang/Object!).
               *)
         interfaces:ClassHandle.Handle list,
         fields:Field.field_data list,
         methods:Method.method_data list,
         attributes:Attribute.attributes
         }

   val compile:class_data->W8.vector

   val save:string*W8.vector->unit
   (* creates a binary file with name the given string and contents
      the Word8Vector.vector *)

   val do_all:string*class_data->unit
   (* concatenation of make_class, compile and save *)

   val quick_directory:string ref (* Initially "" *)
   val quick:class_data->unit
   (* like do_all but saves to !quick_directory ^ "XXX.class" where 
      XXX is the name of the
      class (a matching exception will occur if this contains non-ASCII
      characters) *)
   val zip_quick:Zip.zipOut*class_data->unit
   (* Like quick, but puts the class file into the zip file rather than
      the file system *)

   val decode_class:BinIO.instream->class_data
   (* This decodes a class (see the Decode structure) *)
end












