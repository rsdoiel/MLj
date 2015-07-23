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
(* REAL signature.                                                      *)
(* Deviations from the standard:                                        *)
(* 1) The Math substructure is not included                             *)
(* 2) Real.fmt raises NotImplemented (hopefully this will change, but   *)
(*    doing this function properly requires a lot of work).             *)
(*    Best probably to use java/text/DecimalFormat, which will I hope   *)
(*    be done properly and documented one day                           *)
(* 3) scan is not included yet (because StringCvt isn't)                *)
(*======================================================================*)
signature REAL =
sig
   type real = real
   (* This signature is copied (mostly) from the current (out-of-date)
      WEB page *)
 
   structure Math : MATH
   val radix : int 
   val precision : int 
   val maxFinite : real 
   val minPos : real 
   val minNormalPos : real 
   val posInf : real 
   val negInf : real 
   val + : (real * real) -> real 
   val - : (real * real) -> real 
   val * : (real * real) -> real 
   val / : (real * real) -> real 
   val *+ : real * real * real -> real 
   val *- : real * real * real -> real 
   val ~ : real -> real 
   val abs : real -> real 
   val min : (real * real) -> real 
   val max : (real * real) -> real 
   val sign : real -> int 
   val signBit : real -> bool 
   val sameSign : (real * real) -> bool 
   val copySign : (real * real) -> real 
   val compare : (real * real) -> order 
   val compareReal : (real * real) -> IEEEReal.real_order 
   val < : (real * real) -> bool 
   val <= : (real * real) -> bool 
   val > : (real * real) -> bool 
   val >= : (real * real) -> bool 
   val == : (real * real) -> bool
   val != : (real * real) -> bool
   val ?= : (real * real) -> bool 
   val unordered : (real * real) -> bool 
   val isFinite : real -> bool 
   val isNan : real -> bool 
   val isNormal : real -> bool 
   val class : real -> IEEEReal.float_class 
   val fmt : StringCvt.realfmt -> real -> string
   val toString : real -> string 
   val fromString : string -> real option 
   val scan : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
   val toManExp : real -> {man : real, exp : int} 
   val fromManExp : {man : real, exp : int} -> real 
   val split : real -> {whole : real, frac : real}
   val realMod : real -> real 
   val rem : (real * real) -> real 
   val nextAfter : (real * real) -> real 
   val checkFloat : real ->real 
   val floor : real -> Int.int 
   val ceil : real -> Int.int 
   val trunc : real -> Int.int 
   val round : real -> Int.int 
   val realFloor : real -> real
   val realCeil : real -> real
   val realTrunc : real -> real

   val toInt : IEEEReal.rounding_mode -> real -> int 
   val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int 
   val fromInt : int -> real 
   val fromLargeInt : LargeInt.int -> real 

   (* NB LargeReal is the same as Real in this implementation *)
   val toLarge : real -> real 
   val fromLarge : IEEEReal.rounding_mode -> real -> real
end


