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

structure IntConvFlags=
struct
   datatype Base=Hex|Decimal
   datatype Kind=Unsigned | Signed of bool
   (* The JavaInt.fromString and JavaLong.fromString functions are parameterised
      by a variable of type Base and a variable of type Kind.

      The variable of type Base indicates the radix of the string, decimal
      or hexadecimal.  For hexadecimal, the digits above 9 must be lower case.

      The variable of type Kind gives signing information (the string itself
      should not contain a sign, only digits).  Let N be the number represented
      by the digits of the string, and K be the number of bits needed to
      represent the target Java type (K=32 for JavaInt and K=64 for JavaLong).
      For k=Unsigned, N should be in [0,2^K) and the K bits of the
      resulting Java integer are exactly those of N.  For K=Signed(false),
      N should be in [0,2^(K-1)) and the Java number will equal N.  For
      K=Signed(true), N should be in [0,2^(K-1)] and the Java number will
      equal -N.  If N is not in the appropriate interval the fromString
      functions return NONE.
      *)
end
