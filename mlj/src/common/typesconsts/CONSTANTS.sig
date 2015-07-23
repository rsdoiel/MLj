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

(* Constants:>CONSTANTS declares and manipulates various types
   of Java constant. *)
signature CONSTANTS=
sig
   datatype constant= (* If a field has a constant value, it
                         must have the STATIC flag, and this value
                         must be consistent with its type *)
(* BYTE, CHAR, SHORT and BOOLEAN are all equivalent to INT.  However they
   are provided (1) so that the typeOf function works; (2) to assist with
   later retargetting.

   Recommendations: for BYTE use an unsigned byte, CHAR use a signed
       2-word (Unicode) number, SHORT use a signed 2-word number, BOOLEAN
       use 0 or 1. *)

       BOOLEAN of JavaInt.t
   |   BYTE of JavaInt.t
   |   CHAR of JavaInt.t
   |   SHORT of JavaInt.t
   |   INT of JavaInt.t
   |   LONG of JavaLong.t
   |   FLOAT of JavaFloat.t
   |   DOUBLE of JavaDouble.t
   |   STRING of JavaString.t
   |   NULL (* This corresponds to a null constant *)

   val equal:constant*constant*bool->bool
   (* equal compares two constants.  If the bool argument is false, the constants must
      have the same type and contain the same value.  If the bool argument is true,
      the values must still be the same, but the constructors BOOLEAN, BYTE, CHAR, SHORT
      and INT are considered equivalent. *)

   val typeOf:constant->Types.java_type
   (* typeOf returns the type of a constant.  NULL is deemed to have type Object
      (which is a bit of a fudge since unlike Object it can be cast to anything) *)

   val convert:constant * Types.base_type -> constant option
   (* convert attempts to convert the constant argument to the type argument,
      as for Java's conversion functions (i2l and so on).  If it cannot
      do this, because the result would be meaningless or because this
      case hasn't been programmed (EG anything involving floats) it
      returns NONE. 

      The types it can currently convert between are
      CHAR, BYTE, SHORT, INT and LONG. *)

   val make_int:constant -> constant
   (* make_int turns constants which are BOOLEAN, CHAR,BYTE or SHORT into
      their long equivalents *)

   val hashConst:constant -> word
   (* hashConst hashes a constant.  Right now it ignores everything but the
      type. *)

   val is_zero:constant -> bool
   (* is_zero returns true if a constant is such that comparisons with it
      can be dealt with by cond0.  IE a char/byte/short/int zero, or a 
      null reference *)

   val toInt:constant -> JavaInt.t option
   (* returns the corresponding int if the constant is
      a BOOLEAN, BYTE, CHAR, SHORT or INT. *)

   val cost:constant->int
   (* this attempts to estimate the number of bytes of bytecode in the instruction
      stream required to push this constant on the stack.  (So we don't include
      the cost for the constant pool, because the purpose of cost is to allow us
      to decide whether a constant which is used more than once should be loaded each
      time or replicated using dup instructions and the like). *)

   (* the XXX_toString functions are intended for debugging purposes only and
      should not be used in production code or in any other way relied on. *)
   val constant_toString:constant->string
end
