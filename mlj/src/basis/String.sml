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
(* Standard basis: structure String.					*)
(*======================================================================*)
structure String :> STRING 
where type string = string and 
type Char.char = char and type Char.string = string =
struct

(* Underneath this is just a Java string *)
type string = string

structure Char = Char

local 
  open General Bool Option List Int
  val op= = Prim.=
in

fun extract(s : string, i:int, nopt:int option) =
  Prim.unsafeValOf(
  case nopt of
    NONE   => s.#substring(i)
  | SOME n => s.#substring(i, i+n)
  )

fun substring(s, i, n) = extract(s, i, SOME n)

open MLJUtils.String

(* For concat we use StringBuffer (as does Java) but with some special
   cases on 0,1,2 strings for efficiency *)
fun concat [] = ""
  | concat [s] = s
  | concat [s1,s2] = s1 ^ s2
  | concat (s::ss) =
    let 
      val sb = StringBuffer.fromString s
      fun app [] = StringBuffer.toString sb
        | app (s::ss) = (StringBuffer.appendString(sb, s); app ss)
    in
      app ss
    end

(* For implode we do a similar thing, using StringBuffer... *)
fun implode [] = ""
  | implode [c] = str c
  | implode cs =
    let 
      val sb = StringBuffer.empty ()
      fun app []  = StringBuffer.toString sb
        | app (c::cs) = (StringBuffer.appendChar(sb, c); app cs)
    in
      app cs
    end

(* ...and again for translate *)
fun translate f s = 
let
  val sb = StringBuffer.empty ()
  val finish = size s
  fun tr j = 
      if j=finish 
      then StringBuffer.toString sb
      else 
      (StringBuffer.appendString (sb, f (sub(s, j))); tr (j+1))
in
  tr 0
end
  
   
(* For explode we just use a tail-recursive auxiliary function *)
fun explode s =
let 
  fun exp (0, acc) = acc
    | exp (j, acc) = 
      let val j' = j-1
      in
        exp (j', sub(s, j')::acc)
      end
in
  exp (size s, [])
end

      
fun compare(s1 : string,s2 : string) = 
  let 
    val i = _pure(s1.#compareTo(s2))
  in
    if Int.<(i,0) then LESS else
    if Int.>(i,0) then GREATER
    else EQUAL
  end

(* Returns true if the string s1 is a prefix of the string s2 *)
fun isPrefix s1 s2 = _pure (s1.#regionMatches(0,s2:string,0,size s1))

fun collate cmp (s1, s2) =
    let val n1 = size s1 
	and n2 = size s2
	val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point s1[0..j-1] = s2[0..j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
              let 
                val c1 = sub(s1, j)
                val c2 = sub(s2, j)
              in
		case cmp(c1,c2) of
		    EQUAL   => h (j+1)
                  | result  => result
              end
    in h 0 end;

val op< = fn (s1:string,s2:string) => _pure(s1.#compareTo(s2)) < 0
val op> = fn (s1:string,s2:string) => _pure(s1.#compareTo(s2)) > 0
val op<= = fn (s1:string,s2:string) => _pure(s1.#compareTo(s2)) <= 0
val op>= = fn (s1:string,s2:string) => _pure(s1.#compareTo(s2)) >= 0

fun tokens p s = 
  map MLJSubstring.string 
    (MLJSubstring.tokens p (MLJSubstring.all s))
fun fields p s = 
  map MLJSubstring.string 
    (MLJSubstring.fields p (MLJSubstring.all s))


fun fromString s =
let
  val n = size s
in
  if n=0 then SOME s
  else 
  let
    val sb = StringBuffer.emptyWith n (* At least n characters *)
    fun getc i = if i=n then NONE else SOME(sub(s,i), i+1)
  in
    case Char.scan getc 0 of
      NONE => NONE
    | SOME (c, i) =>
      let
        fun loop i = 
            case Char.scan getc i of
              NONE => SOME (StringBuffer.toString sb)
            | SOME (c, i) => (StringBuffer.appendChar(sb, c); loop i)
      in
        StringBuffer.appendChar(sb, c); loop i
      end
  end
end

fun fromCString s =
let
  val n = size s
in
  if n=0 then SOME s
  else 
  let
    val sb = StringBuffer.emptyWith n (* At least n characters *)
    fun getc i = if i=n then NONE else SOME(sub(s,i), i+1)
  in
    case MLJUtils.String.charscan false getc 0 of
      NONE => NONE
    | SOME (c, i) =>
      let
        fun loop i = 
            case MLJUtils.String.charscan false getc i of
              NONE => SOME (StringBuffer.toString sb)
            | SOME (c, i) => (StringBuffer.appendChar(sb, c); loop i)
      in
        StringBuffer.appendChar(sb, c); loop i
      end
  end
end

fun toString s = translate Char.toString s

fun toCString s = translate Char.toCString s

val maxSize = valOf(Int.maxInt)

end

end
