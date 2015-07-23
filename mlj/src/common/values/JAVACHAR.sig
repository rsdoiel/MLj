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

(* JAVACHAR classifies JavaChars (actually words, for now) as
   letter or digits.

   NB.  We do not check that characters are in fact
   legal Unicode characters.  This means that we will falsely return
   true in some instances.  There are two reasons for this.
   1. The Unicode charcter set is continually being revised with new
      symbols.  We cannot continually update MLJ to keep step.
      In any case it is unlikely that JVMs will be that picky.
   2. I can't be bothered to copy the relevant arrays in.

   However we should get all ASCII characters right (these being
   those with codes from 0-127).
   *)
signature JAVACHAR=
sig
   type c=word 
   (* This may change.  Only JavaString.sml should rely on it.  However
      it may be assumed everywhere that c is an eqtype. *) 

   val toAscii:c->char option
   val fromAscii:char->c

   (* Java Chars are normally represented in the compiler as
      JavaInts. *)
   val fromJavaInt:JavaInt.t -> c option
   val toJavaInt:c->JavaInt.t

   (* The _ character is deemed to be a "letter" and a "letter-or-digit".
   The $ character is not. *)
   val isJavaLetterOrDigit:c->bool
   val isJavaLetter:c->bool
   val isAsciiLetterOrDigit:c->bool
   val isAsciiLetter:c->bool

   val toMLescape:c->string
   (* toMLescape turns c into the corresponding string using ML(J) escapes,
      minus the quotes.  So characters with codes 32-126 go to themselves,
      characters 7-13 go to "\\a", "\\b", "\\t" and so on, other characters
      go to "\\u[hex digit][hex digit][hex digit][hex digit]".

      This is a matter of taste of course, and may change.  
      EG is the null character better represented as "\u0000" or "\000" or
      "\@"?
      *) 
end
