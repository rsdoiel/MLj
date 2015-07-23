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
(* Standard basis Array structure                        		*)
(*======================================================================*)
structure Array :> ARRAY =
struct

local 
  open Int General List Option
  val op= = Prim.=
in

type 'a array = 'a array
type 'a vector = 'a vector

val maxLen = Option.valOf (Int.maxInt)

fun tabulate(n:int, f : int -> 'a) =
  let 
    val a : 'a array = Prim.newarray(n)
    fun init i = 
      if i = n then a
      else (Prim.arraystore(a, i, f i); init (i+1))
  in 
    init 0 
  end

fun array(n, v : 'a) = 
  let 
    val a : 'a array = Prim.newarray(n)
    fun init i = 
      if i = n then a
      else (Prim.arraystore(a, i, v); init (i+1))
  in 
    init 0 
  end

fun fromList (vs : 'a list) =
    let val n = List.length vs
	val a : 'a array = Prim.newarray(n)
	fun init ([], i) = a
	  | init (v::vs, i) = 
            (Prim.arraystore(a, i, v); init (vs, i+1))
    in 
      init (vs, 0) 
    end

fun length (a : 'a array) = Prim.arraylength(a)

fun sub(a : 'a array, i : int) = Prim.arrayload(a, i)

fun update(a : 'a array, i, v : 'a) = Prim.arraystore(a, i, v)

fun extract (a : 'a array, i, slicelen) =
  let val n = case slicelen of NONE => length a - i | SOME n => n
  in
    Vector.tabulate(n, fn j => sub(a, i+j))
  end

fun copy {src : 'a array, si, len, dst : 'a array, di} =
let
  val len = case len of NONE => length src - si | SOME len => len
in
  java.lang.System.arraycopy(src, si, dst, di, len)
end

fun copyVec {src : 'a vector, si, len, dst : 'a array, di} =
let
  val len = case len of NONE => Vector.length src - si | SOME len => len
in
  java.lang.System.arraycopy(Prim.fromVector src, si, dst, di, len)
end

fun foldl f e a = 
    let val stop = length a
	fun lr j res = if j < stop then lr (j+1) (f(sub(a,j), res))
		       else res
    in lr 0 e end

fun foldr f e a =
    let fun rl j res = if j >= 0 then rl (j-1) (f(sub(a,j), res))
		       else res
    in rl (length a - 1) e end

fun modify f a = 
    let val stop = length a
	fun lr j = if j < stop then (update(a, j, f(sub(a, j))); lr (j+1))
		   else ()
    in lr 0 end

fun app f a = 
    let val stop = length a
	fun lr j = if j < stop then (f(sub(a, j)); lr (j+1))
		   else ()
    in lr 0 end

fun sliceend (a, i, NONE) = 
        if i<0 orelse i>length a then raise Subscript
	else length a
  | sliceend (a, i, SOME n) = 
	if i<0 orelse n<0 orelse i+n>length a then raise Subscript
	else i+n;

fun foldli f e (slice as (a, i, _)) = 
    let fun loop stop =
	    let fun lr j res = 
		if j < stop then lr (j+1) (f(j, sub(a, j), res))
		else res
	    in lr i e end
    in loop (sliceend slice) end;

fun foldri f e (slice as (a, i, _)) = 
    let fun loop start =
	    let fun rl j res = 
		    if j >= i then rl (j-1) (f(j, sub(a, j), res))
		    else res
	    in rl start e end;
    in loop (sliceend slice - 1) end

fun modifyi f (slice as (a, i, _)) = 
    let fun loop stop =
	    let fun lr j = 
		if j < stop then (update(a, j, f(j, sub(a, j))); lr (j+1))
		else ()
	    in lr i end
    in loop (sliceend slice) end;

fun appi f (slice as (a, i, _)) = 
    let fun loop stop = 
	    let	fun lr j = 
		    if j < stop then (f(j, sub(a, j)); lr (j+1)) 
		    else ()
	    in lr i end
    in loop (sliceend slice) end;

end

end
