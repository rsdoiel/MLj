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
signature TESTS=
sig
   datatype test=eq|ne|lt|ge|le|gt

   val negate:test->test (* A [negate test] B iff not (A [test] B) provided that
                            neither A nor B is a NaN *)
   val reverse:test->test (* A [reverse test] B iff (B [test] A) *)

   val eq_yes:test->bool (* True if A=B => A [test] B *)
 
   val test:test*order option->bool
   (* test(_,NONE) always returns false. 
      test(t,SOME order) if the test
         A [t] B is true where
      A is "order" than/to B;
      EG test(x,SOME LESS) is true for x=lt,lt or ne, but not ge,gt or eq.
      *)
      
   (* toString should only be used for debugging purposes! *)
   val toString:test->string
end


