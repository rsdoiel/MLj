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

(* ReadPool:>READPOOL reads a constant pool (including the constant pool
   count) from the supplied instream.
   *)
signature READPOOL=
sig
   type pool
   val readpool:BinIO.instream->pool
   (* The following functions all read items from the pool with the
      supplied index, returning
      appropriate items (and calling Assert.fail if the type is wrong or
      the index is out of bounds).
      *)
   val get_class:pool*int->ClassHandle.Handle
   val get_utf8:pool*int->JavaString.t 
   val get_mdesc:pool*int->Descriptors.method_descriptor
   val get_fdesc:pool*int->Descriptors.field_descriptor
   val get_string:pool*int->JavaString.t
   val get_int:pool*int->JavaInt.t
   val get_long:pool*int->JavaLong.t
   val get_float:pool*int->JavaFloat.t
   val get_double:pool*int->JavaDouble.t

   val get_const:pool*int->Constants.constant
   (* get_const reads a LONG, INT, STRING, FLOAT or DOUBLE constant
      from the pool. *)
end
