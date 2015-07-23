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
(* Standard basis Vector structure.					*)
(* We let Java raise appropriate exceptions for Size and Subscript      *)
(*======================================================================*)
structure Vector :> VECTOR =
struct

local 
  open General List Bool Option Int
  val op= = Prim.=
in

type 'a vector = 'a vector
val maxLen = valOf (Int.maxInt)

(* Primitive stuff first *)
fun length (v : 'a vector) = Prim.arraylength(Prim.fromVector v)

fun sub (v : 'a vector, i : int) = Prim.arrayload (Prim.fromVector v, i)

fun fromList (vs : 'a list) =
  let 
    val n = List.length vs
    val a : 'a array = Prim.newarray(n)
    fun init [] (i : int) = Prim.toVector a
      | init (v::vs) i = (Prim.arraystore(a, i, v); init vs (i+1))
  in 
    init vs 0 
  end

fun tabulate(n : int, f : int -> 'a) =
  let 
    val a : 'a array = Prim.newarray(n)
    fun init (i : int) = 
      if i = n then Prim.toVector a
      else (Prim.arraystore(a, i, f i); init (i+1))
  in 
    init 0 
  end

fun map (f : 'a -> 'b) (v : 'a vector) =
  let 
    val n = length v
    val a : 'b array = Prim.newarray(n)
    fun init i = 
      if i = n then Prim.toVector a
      else (Prim.arraystore(a, i, f (sub(v,i))); init (i+1))
  in 
    init 0 
  end

fun extract (vec : 'a vector, i:int, sliceend) =
    let 
      val n = 
        case sliceend of NONE => length vec - i | SOME n => n
      val newvec : 'a array = Prim.newarray(n)
    in
      java.lang.System.arraycopy (Prim.fromVector vec, i, newvec, 0, n);
      Prim.toVector newvec
    end

fun mapi (f : 'a -> 'b) (vec : 'a vector, i, sliceend) =
    let
      val n = case sliceend of NONE => length vec - i | SOME n => n
      val newvec : 'a array = Prim.newarray(n)
      fun copy j = 
        if Int.<(j,n)
        then (Prim.arraystore(newvec, j, f(i+j, sub(vec,i+j))); copy (j+1))
        else Prim.toVector newvec
    in 
      copy 0 
    end

fun concat (vecs : 'a vector list) =
    let 
      fun acc [] len      = len
	| acc (v::vs) len = acc vs (length v + len)
      val len = acc vecs 0
      val newvec : 'a array = Prim.newarray(len)
      fun copyall to [] = Prim.toVector newvec
	| copyall to (v1::vr) =
	  let 
            val len1 = length v1
	    fun copy j =
		    if j<len1 then
			(Prim.arraystore(newvec, to+j, sub(v1,j)); copy (j+1))
		    else
			()
	    in copy 0; copyall (to+len1) vr end
    in copyall 0 vecs end

fun foldl f e a = 
  let 
    val n = length a
    fun foldl' (i, result) = 
      if i < n 
      then foldl' (i+1, f (sub(a,i), result))
      else result
  in 
    foldl' (0, e)
  end

fun foldr f e a =
  let
    fun foldr' (i, result) = 
      if i >= 0 
      then foldr' (i-1, f(sub(a,i), result))
      else result
  in
    foldr' (length a - 1, e) 
  end

fun app f a = 
  let 
    val n = length a
    fun app' i = 
      if i < n then (f(sub(a,i)); app' (i+1))
      else ()
  in 
    app' 0 
  end

fun sliceend (a, i, NONE) = 
        if i<0 orelse i>length a then raise Subscript
	else length a
  | sliceend (a, i, SOME n) = 
	if i<0 orelse n<0 orelse i+n>length a then raise Subscript
	else i+n;

fun foldli f e (slice as (a, i, _)) = 
    let fun loop stop =
	    let fun lr j res = 
		if j < stop then lr (j+1) (f(j, sub(a,j), res))
		else res
	    in lr i e end
    in loop (sliceend slice) end

fun foldri f e (slice as (a, i, _)) = 
    let fun loop start =
	    let fun rl j res = 
		    if j >= i then rl (j-1) (f(j, sub(a,j), res))
		    else res
	    in rl start e end;
    in loop (sliceend slice - 1) end

fun appi f (slice as (a, i, _)) = 
    let fun loop stop = 
	    let	fun lr j = 
		    if j < stop then (f(j, sub(a,j)); lr (j+1)) 
		    else ()
	    in lr i end
    in loop (sliceend slice) end

end
end
