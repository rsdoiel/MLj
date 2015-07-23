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

structure Word64Array :> MONO_ARRAY 
  where type elem = Word64.word and type vector = Word64Vector.vector
= struct

open Array

type elem = Word64.word
type array = Word64.word array
type vector = Word64Vector.vector

fun extract (a, i, slicelen) =
  let val n = 
    case slicelen of Option.NONE => Int.-(length a, i) | Option.SOME n => n
  in
    Word64Vector.tabulate(n, fn j => sub(a, Int.+(i,j)))
  end

fun copyVec {src=a1, si=i1, len, dst=a2, di=i2} =
    let val n = 
      case len of Option.NONE => Int.-(Word64Vector.length a1, i1) 
                | Option.SOME k => k
    in
	if Int.<(n,0) orelse Int.<(i1,0) orelse Int.>(Int.+(i1,n), 
          Word64Vector.length a1) orelse Int.<(i2,0) orelse Int.>(Int.+(i2,n),
            length a2)
	    then 
		raise General.Subscript
	else 
	    let fun lo2hi j = if Int.<(j, n) then
		(update (a2, Int.+(i2,j), Word64Vector.sub(a1, Int.+(i1,j))); 
                  lo2hi (Int.+(j,1))) else ()
	    in lo2hi 0 end
    end


end
