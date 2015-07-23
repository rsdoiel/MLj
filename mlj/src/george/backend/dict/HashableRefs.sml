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

(* HashableRefs:HASHABLEREFS is a functor which given a type implements
   hashable references for it. *)
functor HashableRefs(type t):> HASHABLEREFS where type t=t =
struct
    type t=t
    datatype href=H of Word.word*t ref

    (* at the moment we just do this by using a counter!  We could save
       memory but not cpu time by using a vector of refs which was enlarged
       when necessary. *)
    val counter=ref(0w0);
    fun new(h)=let
       val _= counter:= !counter + 0w1
    in
       H(!counter,ref h)
    end


    fun access(H(_,r))=r
    structure S=
    struct
       type hash_key=href
       fun hashVal(H(w,_))=w
       fun  sameKey(H(w1,_),H(w2,_))=(w1=w2)
    end
end
