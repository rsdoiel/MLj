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

(* EasyLocals:EASYLOCALS has the job of allocating new local numbers.  It is
   easy because right now it just allocates them naively in order 0,1,2,....,
   allowing for where single or double words are needed.

   When we do more sophisticated register allocation, it will not be here;
   however it may not be necessary then for EasyLocals to look at where
   single or double words are required.
   *)
signature EASYLOCALS=
sig
   type localpool (* There should be one of these for each method *)

   val new_localpool: Types.java_type list->localpool*int list
   (* allocate new local pool and compute local numbers for supplied
      types which are the arguments to the method (including this if
      it's non-static). *)
   val new_local:localpool*Types.java_type->localpool*int
   (* get a new local and return the updated localpool and the locals number *)
   val max_locals:localpool->int
   (* return the maximum number of locals *)
end
