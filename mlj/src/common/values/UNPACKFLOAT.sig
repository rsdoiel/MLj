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

(* UnPackFloat:UNPACKFLOAT unpacks floats and doubles in Java representation
   into ML reals 

   NB.  At the moment ML does not allow reals to contain
   "signalling NaN"s.  Hence all NaNs are represented as 
   quiet NaNs. *)
signature UNPACKFLOAT=
sig
   val unpack:{exponent_size:int,mantissa_size:int,data:Word8Vector.vector}->
      real
   (* See comments for PackFloat.pack for details.  The same restrictions
      on exponent_size and mantissa_size apply.
      *)
end







