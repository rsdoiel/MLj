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
(* Operations on sorts							*)
(*======================================================================*)
structure SortOps :> SORTOPS =
struct

open TySort

fun toString s = 
  case s of
    Int     => "Int"
  | Word    => "Word"
  | Real    => "Real"
  | Text    => "Text"
  | WordInt => "WordInt"
  | RealInt => "RealInt"
  | Num     => "Num"
  | NumText => "NumText"
  | WordIntText => "WordIntText"
  | Eq      => "Eq"
  | Any	    => "Any"
  | Class   => "Class"

fun s1 <= s2 = s1=s2 orelse
  case (s1,s2) of
    (Int,WordInt)  => true
  | (Int,RealInt)  => true
  | (Int,Num)      => true
  | (Int,NumText)  => true
  | (Int,WordIntText) => true
  | (Word,WordInt) => true
  | (Word,Num)     => true
  | (Word,NumText) => true
  | (Word,WordIntText) => true
  | (Real,RealInt) => true
  | (Real,Num)     => true
  | (Real,NumText) => true
  | (WordInt,Num)  => true
  | (WordInt,NumText) => true
  | (WordInt,WordIntText) => true
  | (RealInt,Num)  => true
  | (RealInt,NumText) => true
  | (Num,NumText)  => true
  | (Text,NumText) => true
  | (Text,WordIntText) => true
  | (Real,Eq) 	   => false
  | (RealInt,Eq)   => false
  | (Num,Eq)	   => false
  | (NumText,Eq)   => false
  | (Any,Eq)	   => false
  | (_,Any) 	   => true
  | (_,Eq) 	   => true
  | _ 		   => false

fun glb (s1,s2) = 
  if s1<=s2 then SOME s1
  else if s2<=s1 then SOME s2
  else   
    if s1=Word orelse s1=Int orelse s1=Real orelse s1=Text orelse
       s2=Word orelse s2=Int orelse s2=Real orelse s2=Text
    then NONE
    else case (s1,s2) of
      (WordInt, RealInt) => SOME Int
    | (RealInt, WordInt) => SOME Int
    | (Eq, NumText) => SOME WordIntText
    | (NumText, Eq) => SOME WordIntText
    | (Num, Eq) => SOME WordInt
    | (Eq, Num) => SOME WordInt
    | (WordIntText, Num) => SOME WordInt
    | (Num, WordIntText) => SOME WordInt
    | _ => NONE
      
end
