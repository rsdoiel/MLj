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

(* the functions in ConstantValue produce a constant-value attribute *)
structure ConstantValue:>CONSTANTVALUE=
struct
   type t=Constants.constant
   val name=JavaString.fromString "ConstantValue"

   local
      open AllPools
      open Numbers
      open Constants
   in
      type t2=unit->int

      fun pool_pass(A,v)=
      (case v of
         BYTE i=>r_int(A,i)
      |  CHAR i=>r_int(A,i)
      |  SHORT i=>r_int(A,i)
      |  BOOLEAN i=>r_int(A,i)
      |  INT i=>r_int(A,i)
      |  LONG l=>r_long(A,l)
      |  FLOAT f=>r_float(A,f)
      |  DOUBLE d=>r_double(A,d)
      |  STRING s=>r_string(A,s)
      |  NULL=>raise
         Fail "Sorry - NULL constant fields are not permitted"
      )

      fun bytecode_pass value=
         W8.vv1(h2 value)

      fun decode_attr((pool,is),nbytes)=
      let
         val _ = Assert.assert(nbytes=2,"Bad ConstantValue attribute")
      in
         ReadPool.get_const(pool,ReadInts.u2(is))
      end
   end (* local *)
end
