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
(* Char structure							*)
(* Not quite in accordance with Basis Definition: maxOrd != 255         *)
(*======================================================================*)
structure Char :> CHAR where type char = char and type string = string =
struct

type char = char
type string = string

local 
  open Int General Bool Option 
  val op= = Prim.=
in

open MLJUtils.Char

val minChar = Prim.i2c 0
val maxChar = Prim.i2c 65535

fun ord c = Prim.c2i c

val maxOrd = ord maxChar

fun chr i = 
  if Int.<(i, 0) orelse Int.>(i, maxOrd) 
  then raise Chr
  else Prim.i2c i

fun succ c = chr(Int.+(ord c,1))
fun pred c = chr(Int.-(ord c,1))

fun compare (x, y) = 
  if x < y then LESS
  else if x > y then GREATER
  else EQUAL

fun contains (s : string) (c : char) = 
  case _pure (s.#indexOf (c)) of
    ~1 => false 
  | _ => true

fun notContains (s : string) (c : char) = 
  case _pure (s.#indexOf (c)) of
    ~1 => true 
  | _ => false

fun isGraph c  = isPrint c andalso Bool.not (isSpace c)
fun isPunct c  = isGraph c andalso Bool.not (isAlphaNum c)

fun toString c = 
  case c of
    #"\a" => "\\a"
  | #"\b" => "\\b"
  | #"\t" => "\\t"
  | #"\n" => "\\n"
  | #"\v" => "\\v"
  | #"\f" => "\\f"
  | #"\r" => "\\r"
  | #"\\" => "\\\\"
  | #"\"" => "\\\""
  | c => 
    if c < #" "
    then MLJUtils.String.^("\\^", MLJUtils.String.str (chr (ord c + ord #"@")))
    else 
    if c >= #"\127"
    then MLJUtils.String.^("\\",
      Prim.unsafeValOf(
        _pure (java.lang.Integer.toString(ord c))))
    else MLJUtils.String.str c

fun toCString c =
  case c of
    #"?" => "\\?"
  | #"'" => "\\'"
  | #"\a" => "\\a"
  | #"\b" => "\\b"
  | #"\t" => "\\t"
  | #"\n" => "\\n"
  | #"\v" => "\\v"
  | #"\f" => "\\f"
  | #"\r" => "\\r"
  | #"\\" => "\\\\"
  | #"\"" => "\\\""
  | c => 
    if c < #" " orelse c >= #"\127"
    then MLJUtils.String.^("\\",
      Prim.unsafeValOf(
        _pure (java.lang.Integer.toString(ord c,8))))
    else MLJUtils.String.str c
  
fun scan getc = MLJUtils.String.charscan true getc

fun fromString s = 
let
  val len = MLJUtils.String.size s
  val r =
    MLJUtils.String.charscan true (fn i =>  
      if i=len then NONE else SOME (MLJUtils.String.sub(s,i), i+1)) 0
in
  case r of
    NONE => NONE
  | SOME(c,_) => SOME c
end

fun fromCString s = 
let
  val len = MLJUtils.String.size s
  val r =
    MLJUtils.String.charscan false (fn i =>  
      if i=len then NONE else SOME (MLJUtils.String.sub(s,i), i+1)) 0
in
  case r of
    NONE => NONE
  | SOME(c,_) => SOME c
end

end (* of local open *)

end
