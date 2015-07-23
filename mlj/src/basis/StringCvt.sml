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
(* Implementation of StringCvt structure				*)
(* AJK, 11/11/97                                                        *)
(*======================================================================*)
structure StringCvt :> STRING_CVT =
struct

open Option List MLJUtils.Int

datatype radix = 
  BIN 
| OCT 
| DEC 
| HEX

datatype realfmt = 
  SCI of int option	
| FIX of int option   
| GEN of int option 	
| EXACT
                        
type cs = int
                        
type ('a, 'b) reader = 'b -> ('a * 'b) option

fun scanString scan s = 
  let val len = MLJUtils.String.size s
      fun getc i = if i >= len then NONE 
	           else SOME (MLJUtils.String.sub(s, i), i+1)
  in 
    case scan getc 0 of
      NONE => NONE
    | SOME (res, _) => SOME res
  end

fun scanList scan cs =
    let fun getc []      = NONE 
	  | getc (c::cr) = SOME (c, cr)
    in case scan getc cs of
	NONE          => NONE
      | SOME (res, _) => SOME res
    end

fun dropl p getc = 
    let fun h src =
	case getc src of
	    NONE          => src
	  | SOME(c, rest) => if p c then h rest else src
    in h end

fun splitl p f src = 
let
  val sb = StringBuffer.empty ()
  fun gather x =
  case f x of 
    NONE => (StringBuffer.toString sb, x)
  | SOME (c, x') =>
    if p c then (StringBuffer.appendChar(sb, c); gather x')
    else (StringBuffer.toString sb, x)
in
  gather src
end
  
fun skipWS getc = dropl MLJUtils.Char.isSpace getc

fun takel p getc src = #1 (splitl p getc src)

fun padLeft c n s = 
  let 
    val gap = n - MLJUtils.String.size s
  in
    if gap <= 0 then s
    else 
    let
      val sb = StringBuffer.emptyWith n
      fun fill 0 = (StringBuffer.appendString(sb, s); StringBuffer.toString sb)
        | fill n = (StringBuffer.appendChar(sb, c); fill (n-1))
    in
      fill gap
    end
  end
      
fun padRight c n s = 
  let 
    val gap = n - MLJUtils.String.size s
  in
    if gap <= 0 then s
    else 
    let
      val sb = StringBuffer.emptyWith n
      fun fill 0 = StringBuffer.toString sb        
        | fill n = (StringBuffer.appendChar(sb, c); fill (n-1))
    in
      StringBuffer.appendString(sb, s); fill gap
    end
  end
      

end
