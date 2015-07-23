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

structure Exceptions:>EXCEPTIONS=
(* the functions in Exceptions produce an exceptions attribute *)
struct
   type t=ClassHandle.Handle list
   val name=JavaString.fromString("Exceptions")

   local
      type hndl=unit->int
      open Numbers
   in
      type t2=hndl list

      fun pool_pass(A,l)=
         List.map
            (fn e=>AllPools.r_class(A,e))
            l

      fun bytecode_pass exceptions=
      let
         val nexceptions=List.length exceptions
      in
         W8.fromvList (
            (u2 nexceptions)::
            ( List.map
               (fn eh=>h2 eh)
               exceptions
               )
            )
      end 

      fun decode_attr((pool,is),nbytes)=
         ReadInts.getlistpartial' 
            (fn is => 
               (case ReadInts.u2 is of
                  0 => NONE
               |  n => SOME(ReadPool.get_class(pool,n))
               )
             )
             (is,(nbytes-2) div 2)
   end (* local *)
end (* struct *)



