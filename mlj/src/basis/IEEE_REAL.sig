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

(*======================================================================*)
(* IEEE_REAL signature.                                                 *)
(* Deviations from the standard:                                        *)
(* 1) setRoundingMode raises NotImplemented                             *)
(*======================================================================*)
signature IEEE_REAL=
sig
   exception Unordered
   datatype real_order = LESS | EQUAL | GREATER | UNORDERED
   datatype nan_mode = QUIET | SIGNALLING
   datatype sign_mode = NEG | POS
   datatype float_class
     = NAN of nan_mode 
     | INF of sign_mode 
     | ZERO of sign_mode 
     | NORMAL of sign_mode 
     | SUBNORMAL of sign_mode 
   datatype rounding_mode
     = TO_NEAREST
     | TO_NEGINF
     | TO_POSINF
     | TO_ZERO
   val setRoundingMode : rounding_mode -> unit 
   val getRoundingMode : unit -> rounding_mode 
end

