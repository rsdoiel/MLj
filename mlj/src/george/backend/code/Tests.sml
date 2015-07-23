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

(* Tests:>TESTS defines various sorts of comparisons, and some operations 
   on them *)
structure Tests:>TESTS=
struct
   datatype test=eq|ne|lt|ge|le|gt

   fun negate t=
      (case t of
         eq => ne
      |  ne => eq
      |  lt => ge
      |  ge => lt
      |  le => gt
      |  gt => le
         )

   fun reverse t=
      (case t of
         eq => eq
      |  ne => ne
      |  lt => gt
      |  ge => le
      |  le => ge
      |  gt => lt
      )

   fun toString t=
   (case t of
         eq => "eq"
      |  ne => "ne"
      |  lt => "lt"
      |  le => "le"
      |  gt => "gt"
      |  ge => "ge"
      )

   fun eq_yes t=
      (case t of
         eq=>true
      |  ge=>true
      |  le=>true
      |  _ =>false
      )

   fun test(t,NONE)=false
   |   test(t,SOME LESS)=
       (case t of le => true | lt => true | ne => true | _ => false)
   |   test(t,SOME EQUAL)=
       (case t of eq => true | le => true | ge => true | _ => false)
   |   test(t,SOME GREATER)=
       (case t of ge => true | gt => true | ne => true | _ => false)
   
end
