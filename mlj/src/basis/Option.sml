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

structure Option :> OPTION = 
struct 

    datatype option = datatype Datatypes.option
    exception Option

    fun getOpt (SOME x, _) = x
      | getOpt (NONE, x) = x

    fun isSome (SOME _) = Datatypes.true
      | isSome NONE = Datatypes.false

    fun valOf (SOME x) = x
      | valOf NONE = raise Option

    fun map f (SOME x) = SOME (f x)
      | map f NONE = NONE

    fun mapPartial f (SOME x) = f x
      | mapPartial f NONE = NONE

    fun compose (f,g) x =
      case g x of
        NONE => NONE
      | SOME y => SOME (f y)

    fun composePartial (f,g) x =
      case g x of
        NONE => NONE
      | SOME y => f y

    fun filter f x = 
      if f x then SOME x else NONE

    fun join NONE = NONE
      | join (SOME NONE) = NONE
      | join (SOME (SOME x)) = SOME x
         
end
