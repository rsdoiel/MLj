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

(* JavaString:JAVASTRING is the structure for representing Unicode
   strings.
   *)

signature JAVASTRING=
sig
   type t
   val equal:t*t->bool
   structure pack:PACKABLE where type t=t 
   (* this is how Unicode Strings are output.
      *)

   structure hash:HASH_KEY where type hash_key=t

   val fromString:string->t
   (* Convert an string *)
   val fromStringSlice:string*int*int->t
   (* Convert a string slice (given as (string,start,length)) *)
   val fromAsciiString:string->t
   (* Like fromString but all the characters of the string must have
      codes in [1,127].  So all ASCII characters except NULL are OK. *)
   val fromAsciiStringSlice:string*int*int->t
   (* Ditto for slices *)

   val fromUnicode:JavaChar.c list->t
   (* Turns a list of words into a JavaString *)

   val hashAsciiStringSlice:string*int*int->word
   (* Hash an ASCII slice.  hashAsciiStringSlice =
      hash.hashVal o fromAsciiStringSlice
      *)

   val equalAsciiStringSlice:string*int*int*t->bool
   (* Compare an Ascii string slice with a Javastring.
      equalAsciiStringSlice(s,start,slen,t)=
      equal(fromAsciiStringSlice(s,start,slen),t)
      *)

   val compare:t*t -> order
   (* Comparison.  The ordering used is unspecified, but it is linear
      and always distinguishes distinct strings. *)
   val slash:t -> (t*t) option
   (* If t contains no "/" character, return NONE.  Otherwise,
      return SOME(x,y) where x and y are the portions of the Java string
      before and after respectively the last "/" character.
      *)

   val is_identifier:t->bool
   (* True if t[0] satisfies JavaChar.isJavaLetter and t[n>0] satisfies
      JavaChar.isJavaLetterOrDigit.
      *)

  val is_classname:t->bool
  (* True if t is a sequence of identifiers separated by periods and
     in addition contains only ASCII (<128) characters. *)

   val containsAscii:t*char -> bool
   (* True if JavaString contains the ASCII character char. *)

   val concat:t list->t

   val toString:t->string option
   (* SOME i if t can be represented by i, NONE otherwise *)

   val toMLString:t->string
   (* toMLString converts t into a string with ML escapes *)

   val dot2slash:t->t
   (* dot2slash replaces periods by forward slashes *)

   val getstring:BinIO.instream->t
   (* getstring reads a Unicode string from the given instream, including
      an initial length of length 2. Thus this reverses pack.pack, except 
      that it reads from an instream rather than a Word8Vector.vector. 
  
      We do not catch errors in the format of the UTF8-encoded string.
      *)

   (* The following functions can be used to read JavaStrings.  They
      are fairly minimal since they are (currently) only used for
      parsing field and method descriptors. *)
   type pos (* pos represents a position in the JavaString *)
   val read_begin:t->pos 

   (* begin reading, with pos at the beginning *)
   val read_char:pos->(JavaChar.c*pos) option

   (* Get character at position pos and return it and next position *)
   val read_to_ascii_char:pos*char->(t*pos) option
   (* If there is a character in the string from pos onwards which
      equals the supplied ASCII char, return the JavaString from pos up to
      but not including that character, and the pos after that character.
      This is intended for parsing classnames represented in Java files
      as Lxxxx; where xxxx is the class name; if the position is just
      after the L, read_to_char(,,#";") will return the JavaString
      corresponding to the class name. *) 
   val read_atend:pos->bool
   (* returns true if pos is at the end of the JavaString *)

   val isit_ascii:pos*char->bool
   (* True if there is a character at this position equal to the supplied
      ASCII character *)      
end














