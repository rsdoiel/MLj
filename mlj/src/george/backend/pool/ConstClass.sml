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

(* ConstClass constants contain a pointer to a Utf8 item, which is
   supplied in the form of a handle; thus it can't be packed until the
   Utf8 pool is resolved, and the hashing is done via the Handle
   structure.  Similar considerations, though more complicated ones,
   apply for a number of other constant items.

   The class name must be a fully qualified class name *)
structure ConstClass:>POOL_ITEM where type t=Handles.pool_handle =
struct
   type t=Handles.pool_handle
(* pool_handle is Utf8 for fully qualified class name *)

   structure Packing=Handles.HandlePack
   structure Hashing=Handles.HandleKey
   val tag=0w7:Word8.word
   val skip=1
end
