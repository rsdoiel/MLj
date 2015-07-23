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

signature JAVAFLOAT=
sig
   type t
   structure pack:PACKABLE
   sharing type t=pack.t
   val fromReal:real->t
   val toInt:t->int option
   (* SOME i if t can be represented by i, NONE otherwise *)
   
   val order:t*t->order
   (* Orders the reals in some arbitrary fixed total order. *)   


   val getfloat:BinIO.instream->t
   (* getfloat reads a Java float stored in Java format *)

   (* toString should be used for debugging purposes only! *)
   val toString:t->string
end
