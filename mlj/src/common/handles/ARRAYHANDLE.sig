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

signature ARRAYHANDLE=
sig
   type Handle
   val unknown:Handle
   val is_mutable : Handle -> bool
   (* The maybe_same function attempts to establish if two
      array load/stores could refer to the same area of storage.
      *)
   val maybe_same : 
      {hand1:Handle,type1:Types.java_type,
       hand2:Handle,type2:Types.java_type}->bool
   (* hand1 is the handle attached to the first array load/store; type1 is
      the type of the corresponding array object.  Similarly for hand2,type2.
      *)

   val array_handle_toString : Handle -> string
   (* returns a string representation of an array handle.  This function should
      only be used for debugging purposes *)
end



