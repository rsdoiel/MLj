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
(* CharVector structure, whose type must be identical to String.string.	*)
(* Pure ML!                                                             *)
(*======================================================================*)
structure CharVector :> MONO_VECTOR 
  where type elem = Char.char and type vector = String.string 
= struct

type elem = Char.char 
type vector = String.string

val maxLen = Option.valOf (Int.maxInt)

local 
  open Int 
  val op= = Prim.=
in

fun tabulate (n,f) = 
let
  val sb = StringBuffer.emptyWith n
  fun tr j = if j = n then StringBuffer.toString sb
             else (StringBuffer.appendChar(sb, f j); tr (j+1))
in
  tr 0
end
  
val length = String.size
val fromList = String.implode
val extract = String.extract
val concat = String.concat
val sub = String.sub

fun foldl f e a = 
    let	val stop = length a
	fun lr j res = if j < stop then lr (j+1) (f(sub(a, j), res))
		       else res
    in lr 0 e end

fun foldr f e a =
    let	fun rl j res = if j >= 0 then rl (j-1) (f(sub(a, j), res))
		       else res
    in rl (length a - 1) e end

fun app f a = 
    let val stop = length a
	fun lr j = if j < stop then (f(sub(a, j)); lr (j+1))
		   else ()
    in lr 0 end

fun sliceend (a, i, Option.NONE) = 
        if i<0 orelse i>length a then raise General.Subscript
	else length a
  | sliceend (a, i, Option.SOME n) = 
	if i<0 orelse n<0 orelse i+n>length a then raise General.Subscript
	else i+n

fun foldli f e (slice as (a, i, _)) = 
    let fun loop stop =
	    let fun lr j res = 
		if j < stop then lr (j+1) (f(j, sub(a, j), res))
		else res
	    in lr i e end
    in loop (sliceend slice) end

fun foldri f e (slice as (a, i, _)) = 
    let fun loop start =
	    let fun rl j res = 
		    if j >= i then rl (j-1) (f(j, sub(a, j), res))
		    else res
	    in rl start e end
    in loop (sliceend slice - 1) end

fun appi f (slice as (a, i, _)) = 
    let fun loop stop = 
	    let	fun lr j = 
		    if j < stop then (f(j, sub(a, j)); lr (j+1)) 
		    else ()
	    in lr i end
    in loop (sliceend slice) end

fun map f v =
let
  val n = length v
  val sb = StringBuffer.emptyWith n
  fun tr j = if j = n then StringBuffer.toString sb
             else (StringBuffer.appendChar(sb, f(sub(v, j))); tr (j+1))
in
  tr 0
end
  
fun mapi f (slice as (a, i, _)) =
let
  val stop = sliceend slice
  val sb = StringBuffer.emptyWith (stop-i)
  fun tr j = if j = stop then StringBuffer.toString sb
             else (StringBuffer.appendChar(sb, f(j, sub(a, j))); tr (j+1))
in
  tr i
end
  
end

end
