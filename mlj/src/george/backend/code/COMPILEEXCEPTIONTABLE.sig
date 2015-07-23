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

(* CompileExceptionTable:>COMPILEEXCEPTIONTABLE translates an TryCatch.t 
   list to the bytecodes for the exception table.

   There are two stages to this.  In the first, the exception spec list is
   transformed to one in which the class references have been replaced by
   references to the constant pool.  In the second, the transformed list is
   converted to a Word8Vector.vector.
   *)
signature COMPILEEXCEPTIONTABLE=
sig
   type t2

   val pool_pass:AllPools.AllPools*TryCatch.t list->t2
   (* Add (if necessary) the TryCatch.t list (a list of exceptions
      to catch which is produced by CompileCode) to the pool,
      and return a t2 *)
   
   val bytecode_pass:t2->W8.vector
   (* After the pool has been resolved, compile the bytecodes
      of the exception table and return them. *)
end



