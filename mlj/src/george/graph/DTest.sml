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

(* File for testing the Dominator functor.  Based on figure 2 of the paper by Lengauer & Tarjan *)
datatype nodes=A|B|C|D|E|F|G|H|I|J|K|L|R

fun r n=(n * 12) mod 17
structure K:ORD_KEY=
struct
   type ord_key=nodes
   fun ival n=
   r (case n of
     A => 13
   | B => 1
   | C => 2
   | D => 3
   | E => 4 
   | F => 5
   | G => 6
   | H => 7
   | I => 8
   | J => 9
   | K => 10
   | L => 11
   | R => 12
   )
   fun compare(a,b)=Int.compare(ival a,ival b)
end

structure LDom=Dominator(K)

fun successor n=
(case n of
   A=>[D]
|  B=>[A,D,E]
|  C=>[F,G]
|  D=>[L]
|  E=>[H]
|  F=>[I]
|  G=>[I,J]
|  H=>[E,K]
|  I=>[K]
|  J=>[I]
|  K=>[I,R]
|  L=>[H]
|  R=>[A,B,C]
);







