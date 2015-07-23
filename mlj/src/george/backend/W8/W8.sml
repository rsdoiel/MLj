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

(* W8 implements vectors of bytes in such a way that appending and
   concatenation can be done in O(1) (apart from whatever happens
   with the garbage collector). *)
structure W8:>W8=
struct
   datatype vector=
      leaf of Word8Vector.vector
   |  one of Word8.word
   |  node of vector list
     (* We constrain all nodes not to have exactly 1 child. However,
        empty nodes, and empty Word8Vectors,
        may arise in some circumstances. *)

   fun fromvList [v]=leaf v
   |   fromvList l=node(List.map leaf l)

   fun concat [v]=v
   |   concat l=node l

   fun length vec=let
      fun cumulative_length(leaf(v),sofar)=Word8Vector.length(v)+sofar
      |   cumulative_length(one(w),sofar)=sofar+1
      |   cumulative_length(node(vl),sofar)=
         List.foldr cumulative_length sofar vl
   in cumulative_length(vec,0)
   end

   (* this relies on Word8Vector.sub raising Subscript
      for a 0-length vector. I apologise for the yucky style!
      I generally don't approve of programming using exceptions but the
      "clean" alternative would be to write a version of foldl which
      had (say) type (('a*'b)->'b*bool)->'b->'a list->'b which only
      carried on while the #2(the first argument) was true, and I can't
      be bothered.

      However it doesn't really matter if exceptions are inefficiently
      implemented since 0-length nodes and Word8Vectors shouldn't occur
      that often. *)
   fun first(leaf v)=Word8Vector.sub(v,0)
   |   first(one w)=w
   |   first(node [])=raise Subscript
   |   first(node (hd::rest))=
      (first(hd)
       handle Subscript=>first(node rest))

   fun v1(w)=one(w)
   fun v1l(w,[])=v1(w)
   |   v1l(w,l)=node(one(w)::(List.map leaf l))

   fun fromList(l)=leaf(Word8Vector.fromList(l))

   val vv1=leaf

(* fringe takes a vector to a list of Word8Vector.vectors *)
   fun fringe vec=let
      fun cumulative_fringe(leaf v,sofar)=v::sofar
      |   cumulative_fringe(one w,sofar)=(Word8Vector.fromList [w])::sofar
      |   cumulative_fringe(node vl,sofar)=
         List.foldr cumulative_fringe sofar vl
   in cumulative_fringe(vec,[])
   end

   fun toWord8 v=Word8Vector.concat(fringe v)

   fun flatten(v)=leaf(toWord8 v)

   fun output(ostream,vec)=let
      fun cumulative_out(leaf v,{})=BinIO.output(ostream,v)
      |   cumulative_out(one w,{})=BinIO.output1(ostream,w)
      |   cumulative_out(node vl,{})=List.foldl cumulative_out {} vl
   in
      cumulative_out(vec,{})
   end

   fun combine(veclist)=
      concat(vv1(Numbers.u2(List.length(veclist)))::veclist)

   fun prettyprint(v:vector)=let
      fun ppp((v:vector,sofar:string list),prefix:string)=let
         fun pp(leaf v,sofar)=
               prefix::Int.toString(Word8Vector.length(v))::"\n"::sofar
         |  pp(one w,sofar)=
               prefix::"Word "::Word8.toString(w)::"\n"::sofar
         |  pp(node vl,sofar)=
               prefix::"Node"::"\n"::
              (List.foldr
                 (fn (v,s)=>ppp((v,s),String.concat[prefix,"."]))
                 sofar
                 vl
                 )
      in pp(v,sofar)
      end
   in String.concat(ppp((v,[]),""))
   end
end
