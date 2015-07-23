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

(* JavaString contains functions for converting strings to Java Unicode, and
   for outputting Unicode in bytecode form.

   We UTF-encode the JavaStrings immediately, to save space.

   We ensure that JavaString.ts are always valid UTF8 constants.
   (This means that strings read in from class files have to be checked)
   *)
structure JavaString:>JAVASTRING=
struct
   (* How to speed this structure up - use Unsafe.Word8Vector.sub,
      replace integers used for indexing by words.
      *)
   structure W=Word (* this should be a WORD structure with at least
                       16 bits.  Word16 would be better, if it exists *)
   type t=Word8Vector.vector

   fun equal(x,y)=(x=y)

   structure pack=
   struct
      type t=t

      val equal=equal

      fun pack t=
         Word8Vector.concat[
            Numbers.u2(Word8Vector.length t),
            t
            ]
   end

   exception Bad_Unicode_Bug

   fun do_one w= (* Equals Word8Vector corresponding to Utf8 encoding
                     of word w, as described in the VM book, page 100.
                     *)
      if W.<(w,0wx0080) andalso w<>0w0 then
         (* one-byte (and hopefully most common) case *)
         Word8Vector.fromList [Numbers.w1(w)]
      else if W.<(w,0wx0800) then
         (* two byte case *)
         Word8Vector.fromList [
            Word8.orb
               (0wxc0,Numbers.w1(W.>>(w,0wx6))),
            Word8.orb
               (0wx80,Word8.andb(Numbers.w1(w),0wx3f))
            ]
      else if W.<=(w,0wxffff) then
         (* three byte case *)
         Word8Vector.fromList [
            Word8.orb
               (0wxe0,Numbers.w1(W.>>(w,0wxc))),
            Word8.orb
               (0wx80,Word8.andb(Numbers.w1(W.>>(w,0wx6)),0wx3f)),
            Word8.orb
               (0wx80,Word8.andb(Numbers.w1(w),0wx3f))
            ]
      else raise Bad_Unicode_Bug (* This word is not 16-bit *)

   val concat=Word8Vector.concat

   fun fromString s=
   let
      val slen=String.size s
      val packedchars=List.tabulate
         (slen,fn i=>do_one(Word.fromInt(Char.ord(String.sub(s,i)))))
   in
      concat packedchars
   end

   fun fromStringSlice(s,start,slen)=
   let
      val packedchars=List.tabulate
         (slen,fn i=>do_one(Word.fromInt(Char.ord(String.sub(s,start+i)))))
   in
      concat packedchars
   end

   fun fromAsciiString s=
   let
      val slen=String.size s
   in
      Word8Vector.tabulate(slen,fn i=>Word8.fromInt(Char.ord(String.sub(s,i))))
   end

   fun fromAsciiStringSlice(s,start,slen)=
      Word8Vector.tabulate(slen,
         fn i=>Word8.fromInt(Char.ord(String.sub(s,start+i))))

   fun fromUnicode wlist=
   let
      val packedchars=List.map do_one wlist
   in
      concat packedchars
   end
   
   structure hash=
   struct
      type hash_key=t
      val sameKey= op=
      val hashVal=Hash.hashWord8Vector
   end

   val hashAsciiStringSlice=Hash.hashStringSlice
   
   fun equalAsciiStringSlice(string,start,len1,vec)=
   let
      val len2=Word8Vector.length vec
      fun equal i=
      (* This function could be speeded up with Unsafe operations and
         words (rather than ints) for indexes, but I don't think it'll
         be called that often; the lengths will usually determine the
         answer. *)
      if i=0
      then
         true
      else
      let
         val i=i-1
         val w1=Word8.fromInt(Char.ord(String.sub(string,start+i)))
         val w2=Word8Vector.sub(vec,i)
      in
         (w1=w2) andalso equal i
      end
   in
      (len1=len2) andalso equal len2
   end

   fun compare(vec1,vec2)=
   let
      val len1=Word8Vector.length vec1
      val len2=Word8Vector.length vec2
      fun cp i=
      if i=0
      then
         EQUAL
      else
      let
         val i=i-1
         val w1=Word8Vector.sub(vec1,i)
         val w2=Word8Vector.sub(vec2,i)
      in
         (case Word8.compare(w1,w2) of
            EQUAL => cp i
         |  other => other
         )
      end
   in
      (case Int.compare(len1,len2) of
         EQUAL => cp len1
      |  other => other
      )
   end
      
   fun is_valid vec=
   (* Returns true if the vec is a valid UTF8-encoded string.
      Otherwise false. 
      *)
   let
      fun sub i=Word8Vector.sub(vec,i)
      fun subopt i=SOME(sub i) handle Subscript => NONE
      fun rest j= 
      (* Checks top 2 bits of byte j are 10 and return byte j *)
      let
         val b=sub j
      in
         if Word8.andb(b,0wxc0)<>0wx80
         then 
            raise Subscript
         else
            b
      end

      (* We raise Subscript to indicate failure, otherwise return unit *)
      fun check i= 
      (* check from i onwards, raising Subscript if the vector is invalid,
         otherwise returning NONE. *)
      (case subopt i of
         NONE => ()
      |  SOME first =>
         if first<0wx80
         then
            if first=0w0 then raise Subscript else check(i+1)
         else if Word8.andb(first,0wxe0)=0wxc0
         then
         let
            val second=rest(i+1)
            val ()=
               if
                 (first<0wxc2) andalso (first<>0wxc0 orelse second<>0wx80)
               then
                  raise Subscript
               else
                  ()
         in
            check(i+2)
         end
         else if Word8.andb(first,0wxf0)=0wxe0
         then
         let
            val second=rest(i+1)
            val _ = rest(i+2)
            val () =
               if first=0wxf0 andalso second<=0wxa0
               then
                  raise Subscript
               else
                  ()
         in
            check(i+3)
         end
         else
            raise Subscript
      )
   in
      (check 0;true) handle Subscript => false
   end       

   fun getstring is=
   (* We check bytes for validity, because otherwise (for example)
      comparison functions and hashing might be inconsistent if
      class files contain broken strings.  However, this may be time-
      consuming; if it is we may have to turn the checking off and 
      accept the inconsistencies.  Perhaps an alternative approach
      would be to delay checking until we do the hashing or something.
      *)
   let
      val length=ReadInts.u2 is
      val bytes=ReadInts.inputN(is,length)
      val ()=Assert.assert(is_valid bytes,"Invalid UTF8 constant")
   in
      bytes
   end

   type pos=t*int
   fun read_begin t=(t,0)
   fun read_char(vec,i)=
   (* We recognise end-of-string by assuming Word8Vector.sub will raise
      Subscript.  This means that strings read by getstring which have
      incomplete escapes will generally be silently truncated.
      *)
   let
      fun sub j=W.fromLargeWord(Word8.toLargeWord(Word8Vector.sub(vec,j)))
      val first=sub i
      fun rest j= (* Get remaining bits of a later word, removing the top 2 *)
         W.andb(sub j,0wx3f)
   in
      SOME(
         if first<0wx80
         then
            (first,(vec,i+1))
         else if W.andb(first,0wxe0)=0wxc0
         then
         let
            val second=rest(i+1)
         in
           (W.orb(
               W.<<(W.andb(first,0wx1f),0w6),
               second
               ),
            (vec,i+2)
            )
         end
         else
         let
            val second=rest(i+1)
            val third=rest(i+2)
         in
           (W.orb(
               W.orb(
                  W.<<(W.andb(first,0wxf),0w12),
                  W.<<(second,0w6)
                  ),
               third
               ),
            (vec,i+3)
            )
         end
         )
   end handle Subscript => NONE


   fun isit_ascii (pos as (vec,i),ch)=
      (Word8Vector.sub(vec,i)=Word8.fromInt(Char.ord ch))
         handle Subscript => false
      
   fun read_atend (pos as (vec,i))=(i=Word8Vector.length vec)
   
   fun is_identifier t=
   (case read_char(read_begin t) of
      NONE => false
   |  SOME(c,pos) =>
         JavaChar.isJavaLetter c andalso
         let
            fun dorest pos=
            (case read_char pos of
               NONE => true
            |  SOME(c,pos) =>
                  JavaChar.isJavaLetterOrDigit c
                     andalso
                  dorest pos
            )
         in
            dorest pos
         end
   )

   val period=JavaChar.fromAscii #"."

   fun is_classname t=
   let
      fun ic_from pos=
      (case read_char pos of
         SOME(c,pos) =>
            JavaChar.isAsciiLetter c andalso is_from pos
      |  NONE => false
      )
      and is_from pos=
      (case read_char pos of
         SOME(c,pos) =>
            (JavaChar.isAsciiLetterOrDigit c andalso is_from pos)
               orelse
            (c=period andalso ic_from pos)
      |  NONE => true
      )
   in
      ic_from(read_begin t)
   end

   (* containsAscii, slash, dot2slash and read_to_ascii_char all rely on the
      observation that escape characters in UTF8-encoded strings
      always have their top bit set, so cannot be confused with
      ASCII characters.  All could also be speeded up by using
      Unsafe operations, unless the compiler does the range analysis. *)

   fun c2w8 c=Word8.fromInt(Char.ord c)
   val slashw=c2w8 #"/"
   val dotw=c2w8 #"."

   fun containsAscii(vec,ch)=
   let
      val chw=c2w8 ch
      fun ca i=
      if i=0
      then
         false
      else
      let
         val i=i-1
      in
         Word8Vector.sub(vec,i)=chw orelse ca i
      end
   in
      ca(Word8Vector.length vec)
   end

   fun dot2slash vec=
      Word8Vector.tabulate(Word8Vector.length vec,
         fn i=>
            let
               val w=Word8Vector.sub(vec,i)
            in
               if w=dotw then slashw else w
            end
         )

   fun rightmost(vec,w)=
   (* returns rightmost occurrence of w in vec or ~1 if there isn't one. *)
   let
      fun r i=
      if i=0
      then
         ~1
      else
      let
         val i=i-1
      in
         if Word8Vector.sub(vec,i)=w
         then
            i
         else
            r i
      end
   in
      r(Word8Vector.length vec)
   end

   fun slash vec=
   let
      val pos=rightmost(vec,slashw)
   in
      if pos<0
      then
         NONE
      else
         SOME(Word8Vector.extract(vec,0,SOME pos),
            Word8Vector.extract(vec,pos+1,NONE))
   end

   fun read_to_ascii_char((vec,i),ch)=
   let
      val chw=Word8.fromInt(Char.ord ch)
      fun search j=
         if Word8Vector.sub(vec,j)=chw then j else search(j+1)
      (* raises Subscript if there isn't such a char *)
      val index=search i
   in
      (SOME(Word8Vector.extract(vec,i,SOME (index-i)),(vec,index+1)))
         handle Subscript => NONE
   end

   fun toMLString t=
   let
      fun mklist(sf,pos)=
      (case read_char pos of
         SOME(c,pos) =>
            mklist(JavaChar.toMLescape c::sf,pos)
      |  NONE => sf
      )
   in
      String.concat(List.rev(mklist([],read_begin t)))
   end

   exception toStringFails

   fun toString t=
   let
      fun mklist(sf,pos)=
      (case read_char pos of
         NONE => sf
      |  SOME(c,pos) =>
            (case JavaChar.toAscii c of
               NONE => raise toStringFails
            |  SOME ch => mklist(ch::sf,pos)
            )
      )
   in
      (SOME(String.implode(List.rev(mklist([],read_begin t))))) 
         handle toStringFails => NONE
   end
end

