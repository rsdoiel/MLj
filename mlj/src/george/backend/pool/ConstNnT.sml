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

(* ConstNnT constants represent CONSTANT_NameAndType items, which are used
   for methods+descriptor and similar things.
   *)
structure ConstNnT:>POOL_ITEM where type t=Handles.pair =
struct
   type t=Handles.pair

(* handle 1: unqualified name of method or whatever
   handle 2: descriptor *)

   structure Packing=Handles.TwoHandlePack
   structure Hashing=Handles.TwoHandleKey
   val tag=0w12:Word8.word
   val skip=1
end
