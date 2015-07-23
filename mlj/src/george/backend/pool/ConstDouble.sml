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

(* The ConstDouble structure implements POOL_ITEM for double constants.
   The hashing function can be derived from the packing provided.
  (this means that doubles will all compare equal until I can be
   bothered to output them properly). *)
structure ConstDouble:>POOL_ITEM where type t=JavaDouble.t =
struct
   type t=JavaDouble.t
   structure Packing=JavaDouble.pack
   structure Hashing=PackKey(Packing)
   val tag=0w6:Word8.word
   val skip=2
end
