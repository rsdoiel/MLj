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

(* INTOPS is implemented for t=JavaInt.t and t=JavaLong.t.  The IntConv
   functor takes an INTOPS structure and produces a function for converting
   strings to integers *)
signature INTOPS=
sig
   type t (* type of an number.  t represents integers in two different ways,
             signed or unsigned.  For unsigned integers, the range that
             can be represented is [0,N] for some N, where N>=0.  For
             signed integers, the range that can be represented is
             [-M,N], where 0<N<=M.  A value of type t may correspond
             to two different integers, depending on whether the representation
             is signed or unsigned.

             Where a signed:bool argument is provided to a function, this
             indicates whether the operation should be performed using the
             signed or unsigned representation throughout.

             In all cases, Overflow is raised if the chosen quantity cannot
             be represented as an element of type t with the chosen
             representation. *)
   val zero:{signed:bool}->t
   val mul10:{signed:bool}->t->t
(* Multiplies by 10 *)

   val mul16:{signed:bool}->t->t
(* Multiplies by 16 *)

   val do_digit:{signed:bool}->(t*int)->t
(* The int should be in the range [0,15].  If signed, it is subtracted from
   t; if unsigned it is added to t. *)

   val neg:t->t
(* t is negated.  The representation is assumed to be signed. *)
end
