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

(* ASSERT:>Assert contains the exception and the assertion function for
   the Decode structure (it is not defined there to avoid circularity
   problems). *)
signature ASSERT=
sig
   exception ClassFile of string 
   (* Thrown if the file does not appear to be a valid class file. *)

   val assert:bool*string->unit
   (* raise ClassFile(string) if bool is false. *)

   val must_assert:bool*string->unit
   (* Similar to assert.  However, assert is used for checks which verify
      the class file but are not likely to fail in practice, and
      can be recovered from, while
      must_assert is used for checks which must always be done.
      For example, checks that the class file has the correct
      magic number and does not end prematurely are done with
      must_assert (which should never return if its bool argument is
      false).  
      *)

   val fail:string->'a
   (* Equivalent to must_assert(false,string) *)
end



