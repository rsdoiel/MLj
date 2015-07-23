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

(* This signature defines the functionality that should be implemented for IntInfs
   before the IntInf structure is compiled.  This mainly means things like conversions
   to and from various other types of numbers (fortunately not reals, which are compiled
   after the IntInf structure). *)
signature MLJINT_INF_UTILS=
sig
   type int=IntInfDatatype.int

   val fromInt:Int32.int->int
   val toInt:int->Int32.int

   val fromWord:word->int
   val toWord:int->word

   val fromWord8:Word8.word->int
   val toWord8:int->Word8.word

   val fromFixedInt:Int64.int->int
   val toFixedInt:int->Int64.int

   val fromLargeWord:Word64.word->int
   val toLargeWord:int->Word64.word
end

