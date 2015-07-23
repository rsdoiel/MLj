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

(* Symbols are hashed JavaStrings which support fast equality testing
   and hashing.  We keep a record of all the symbols made so far so
   that we don't have to make them again. 

   WARNING.  Symbols will not work in a multi-threaded environment
   unless it is revised and locks put in at critical sections. 
   *)
structure GeneralSymbol:>SYMBOL=
struct
(* This structure is inspired by the Atom structure provided by
   SML/NJ.  However, it differs externally in that it uses
   JavaStrings, and internally in that symbols are just integers.
   The JavaStrings themselves are stored in a dynamic array.  To
   turn a JavaString into a symbol, we maintain a hash table (also
   a dynamic array) indexed by the hash code of the contents of the
   table.
   *)

   type symbol=int

   (* Initial sizes of the string array and the hash table.
      These are both made pretty big since rehashing is expensive. 
      *)
   val initial_stringarr_size=8192
   val initial_hash_size=8192 (* This should be a power of 2 *)

   local
      (* Implement the array mapping symbols to Java Strings. *)
      val dummy=JavaString.fromString "DUMMY"
      val stringarr=ref(Array.array(initial_stringarr_size,dummy))
      val next_slot=ref 0

   in
      fun growStrings()=
      let
         val old= !stringarr
         val size=2*Array.length old
         val new=Array.array(size,dummy)
         val ()=Array.copy {di=0,dst=new,si=0,src=old,len=NONE}
      in
         stringarr:=new
      end

      fun insert(js:JavaString.t):int=
      let
         val slot= !next_slot
      in
        (Array.update(!stringarr,slot,js);
         next_slot:=slot+1;
         slot
         )
      end
      (* insert raises Subscript if the array is too small, and otherwise
         leaves everything unchanged.  The caller should then call
         growStrings and try again.  We do it this way because
         it is probably also a good time to grow the hash table.
         *)

      fun toJavaString s=Array.sub(!stringarr,s)
   end

   (* Implement the hash table.  We roll our own - rather than
      using the NJ one - because we want symbolAsciiSlice to avoid
      copying the slice until we are sure that it is new.
      *)
   local
      (* Arguably we should cache the hash value, but we don't. *)
      val table=ref(Array.array(initial_hash_size,[]:int list))

      fun getMask()=Word.fromInt(Array.length(!table)) - 0w1
      val mask=ref(getMask())

      fun growTable()=
      let
         val old= !table
         val size=2*Array.length old
         val new=Array.array(size,[]:int list)
         val ()= table:=new
         val newmask=getMask()
         val ()=mask:=newmask
         val highbit=(Word.fromInt size) div 0w2
         val highbiti=Word.toInt highbit

         fun part(f,x)= 
         (* Like List.partition but without currying or list reversal *)
         let
            fun partit(asf,bsf,[])=(asf,bsf)
            |   partit(asf,bsf,h::t)=
               if f h then partit(h::asf,bsf,t) else partit(asf,h::bsf,t)
         in
            partit([],[],x)
         end

         fun hashhigh s=
         (* True if this symbol has high bit of its hash code set. *)
         let
            val hash=JavaString.hash.hashVal(toJavaString s)
         in
            Word.andb(hash,highbit)<>0w0
         end

         val ()=
            Array.appi
               (fn(index,l) =>
                  let
                     val (high,low)=part(hashhigh,l)
                     fun u(ind,li)=
                        (case li of [] => () | _ => Array.update(new,ind,li))
                     (* we expect at least half the entries of the new array
                        to be empty so this is worthwhile *)
                  in
                     (u(index,low);u(index+highbiti,high))
                  end
                  )
               (old,0,NONE)
      in
         ()
      end
   in
      (* symbol and symbolAsciiSlice copy code! *)
      fun symbol js=
      (* If there isn't room we get a Subscript exception; we catch it
         and recurse *)
      let
         val fullhash=JavaString.hash.hashVal js
         val hash=Word.toInt(Word.andb(!mask,fullhash))
         val tab= !table
         val l=Array.sub(tab,hash)
         fun scanList []=
         (* The element is not there.  Add it. *)
         let
            val index=insert js
            val ()=Array.update(tab,hash,index::l)
            (* We put the new element at the head of the list.
               Since elements are often used together, this may well
               be a good idea. *)
         in
            index
         end
         |  scanList (h::t)=
            if JavaString.equal(toJavaString h,js)
            then
               h
            else
               scanList t
      in
         scanList l
      end handle Subscript =>
        (growStrings();growTable();symbol js)

      (* symbol and symbolAsciiSlice copy code! *)
      fun symbolAsciiStringSlice(slice as (string,start,len))=
      (* If there isn't room we get a Subscript exception; we catch it
         and recurse *)
      let
         val fullhash=JavaString.hashAsciiStringSlice slice
         val hash=Word.toInt(Word.andb(!mask,fullhash))
         val tab= !table
         val l=Array.sub(tab,hash)
         fun scanList []=
         (* The element is not there.  Add it. *)
         let
            val index=insert(JavaString.fromAsciiStringSlice slice)
            val ()=Array.update(tab,hash,index::l)
            (* We put the new element at the head of the list.
               Since elements are often used together, this may well
               be a good idea. *)
         in
            index
         end
         |  scanList (h::t)=
            if JavaString.equalAsciiStringSlice
               (string,start,len,toJavaString h)
            then
               h
            else
               scanList t
      in
         scanList l
      end handle Subscript =>
        (growStrings();growTable();symbolAsciiStringSlice slice)

      fun bucketSizes()=
      let 
         val tab= !table
      in
         List.tabulate(Array.length tab,fn i=>length(Array.sub(tab,i)))
      end
   end

   fun symbolAsciiString s=symbolAsciiStringSlice(s,0,String.size s)


   structure HashKey:>HASH_KEY where type hash_key=symbol =
   struct
      type hash_key=int
      val hashVal=Word.fromInt
      val sameKey=op=
   end

   structure OrdKey:>ORD_KEY where type ord_key=symbol=
   struct
      type ord_key=int
      val compare=Int.compare
   end

   structure OrdMap:>ORD_MAP where type Key.ord_key=symbol = IntBinaryMap
   structure OrdSet:>ORD_SET where type Key.ord_key=symbol = IntBinarySet

   val equal=op=
   fun number i=i
end


