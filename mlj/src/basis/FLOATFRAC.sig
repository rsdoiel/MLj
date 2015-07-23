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

(* Operations we might need on fractions to implement floating point <> 
   decimal conversion.  All operations should be exact *)
signature FLOATFRAC=
sig
   type frac (* Positive fraction *)
   val make_frac:
     {mantissa:Int64.int,
      exp2:int,
      exp10:int
      } ->frac 
   (* create a fraction equal to the long* 2^exp2 * 10^exp10. *)
   val mul2:frac->frac
   val div2:frac->frac
   val mul10:frac->frac
   val mulpow10:frac*Int.int->frac
   (* mulpow10(f,n) returns f*10^n. *)
   val div10:frac->frac   

   val compare1:frac->order
   (* compares the fraction with 1 *)
   val modf:frac->IntInf.int*frac
   (* Computes the integral and fractional part of the argument and
      returns them *)
   val is0:frac->bool
   (* Returns true if the fraction is zero *)
end

