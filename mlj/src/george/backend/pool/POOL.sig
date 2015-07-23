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

(* There is one structure:>POOL for each type which can be put in the
   constant pool; the functions in this structure accumulate the constants
   of that type (so the constant pool is separated out).  The :POOL
   structures are obtained by a functor on :POOL_ITEM structures. *)

signature POOL=
   sig
   type t      (* type of constants stored in the pool *)
   type pool   (* the pool itself *)

   val create : int->pool
               (* creates a new pool.  The argument currently controls
                  the size of the hash table containing the pool *)

   val add    : pool*t->Handles.pool_handle
               (* adds an item to the pool *)
   exception already_resolved
               (* raised if there is an attempt to add to a resolved pool *)

   val resolve: pool*(int ref)->W8.vector
               (* resolve "resolves" the pool, returning
                  the string of bytes representing the pool to
                  be output to the bytecode file.  The index
                  function will then return the index of
                  items in the pool.  The int ref initially contains
                  the index of the first byte of the pool, and is
                  incremented by the length of the pool *)
   val pool_size:pool->int
               (* pool_size returns the size of the pool, measured as the
                  amount the int ref gets increased by when resolve is
                  called.  This function should work whether or not the
                  pool has been resolved. *)
   exception twice_resolved
               (* raised if there is an attempt to resolve the pool twice
                  (it is left unchanged) *)

   end

