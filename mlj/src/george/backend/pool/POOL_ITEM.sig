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

(* We need to declare a structure:>POOL_ITEM for each kind of thing we want
   to put in the constant pool *)
signature POOL_ITEM=
(* there must be one of these for each type that can be put in the
   constant pool *)
   sig

   type t     (* type of information about this constant *)
   structure Packing:PACKABLE
      (* This contains the function which turns values of type t into
         a byte stream *)
   structure Hashing:HASH_KEY
   sharing type t=Packing.t=Hashing.hash_key
   val tag:Word8.word
   val skip:int
   (* amount to increment constant pool index by.  This is 1 except for
      ConstDouble and ConstLong, when it is 2.  (this is pretty stupid;
      see the VM book page 98, footnote). *)
   end
