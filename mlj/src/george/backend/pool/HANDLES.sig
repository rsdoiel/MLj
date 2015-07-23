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

(* Handles:HANDLES implements handles to items in the constant pool.  They can be
      hashed at any time using the Handle and TwoHandle structures.
      Once the position of the item has been set, they can be used
      in the HandlePack and TwoHandlePack structures, which
      pack a handle or a pair of handles into a Word8Vector as they are
      stored in the constant pool, or via lookup.
      *)
signature HANDLES=
sig
   type pool_handle
   val new_handle:unit->pool_handle
   val set_handle:pool_handle*int->unit 
   (* Set the handle to the given integer, which should be non-negative *)
   val lookup:pool_handle->int (* This should not be called if the handle has not been set. *)

   structure HandleKey:HASH_KEY sharing type HandleKey.hash_key=pool_handle
   structure HandlePack:PACKABLE sharing type HandlePack.t=pool_handle

   type pair
   val make_pair:pool_handle*pool_handle->pair

   structure TwoHandleKey:HASH_KEY sharing type TwoHandleKey.hash_key=pair
   structure TwoHandlePack:PACKABLE sharing type TwoHandlePack.t=pair
   (* The two handles are packed in the same order as they are in the tuple *)
end
    




