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
(* Pretty printing auxiliary functions					*)
(*======================================================================*)
structure Pretty :> PRETTY =
struct

val indentUnit = ref 2

(*----------------------------------------------------------------------*)
(* Convert a symbol into an ML string					*)
(*----------------------------------------------------------------------*)
fun idToString id = JavaString.toMLString (Symbol.toJavaString id)

(*----------------------------------------------------------------------*)
(* Insert "." between structure components of path			*)
(*----------------------------------------------------------------------*)
fun longidToString [] = ""
  | longidToString [id] = idToString id
  | longidToString (strid::longid) = 
    idToString strid ^ "." ^ longidToString longid

(*----------------------------------------------------------------------*)
(* Turn a binding depth (or any other index) into an alphabetic id.     *)
(*----------------------------------------------------------------------*)
fun indexToString m =
if m < 0 then indexToString (~m) ^ "'"
else
if m < 26 then Char.toString (Char.chr (Char.ord #"a" + m))
else indexToString (m div 26 - 1) ^ indexToString (m mod 26)

(*----------------------------------------------------------------------*)
(* Turn a list of items into a string.					*)
(*----------------------------------------------------------------------*)
fun vec (empty,left1,right1,left2,right2,sep) f [] = 
    empty

  | vec (empty,left1,right1,left2,right2,sep) f [x] = 
    left1 ^ f x ^ right1

  | vec (empty,left1,right1,left2,right2,sep) f (x::xs) = 
    let fun separate [] = ""
          | separate (x::xs) = sep ^ f x ^ separate xs
    in
      left2 ^ f x ^ separate xs ^ right2
    end

fun simpleVec sep = vec ("","","","","",sep)

(*----------------------------------------------------------------------*)
(* Make a newline string consisting of a newline character and 		*)
(* indentation to a depth of n.                                         *)
(*----------------------------------------------------------------------*)
fun newline n = 
  let val m = n * !indentUnit
  in
    CharVector.tabulate(m+1, fn 0 => #"\n" | _ => #" ")
  end

(*----------------------------------------------------------------------*)
(* Pretty print a list/set/map with a separate line for each element	*)
(*----------------------------------------------------------------------*)
fun bigVec depth =   
  vec (
    "{}", 
    newline depth ^ "{" ^ newline (depth+1), 
    newline depth ^ "}", 
    newline depth ^ "{" ^ newline (depth+1), 
    newline depth ^ "}", 
    "," ^ newline (depth+1))

(*----------------------------------------------------------------------*)
(* Enclose with parentheses if b is true.        			*)
(*----------------------------------------------------------------------*)
fun parens b s = if b then "(" ^ s ^ ")" else s

end
