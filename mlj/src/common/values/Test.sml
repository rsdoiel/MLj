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

(* Test is a functor written for test purposes which takes a NUMOPS
   structure to something which prints objects in it *)
functor Test(A:NUMOPS)=
struct
   
   fun toString(x:A.num)=
   (case A.div(x,x) of
      NONE => "0"
   |  SOME one =>
      (* this rather convoluted hack gets us a value for one *)
   let
      val zero=A.sub(one,one)
      val two=A.add(one,one)
      val three=A.add(two,one)
      val four=A.add(three,one)
      val five=A.add(four,one)
      val six=A.add(five,one)
      val seven=A.add(six,one)
      val eight=A.add(seven,one)
      val nine=A.add(eight,one)
      val ten=A.add(nine,one)
      fun get_digs(sofar,x)=(* get list of digits for x, which is 
         positive *)
      let
         val dig=valOf(A.rem(x,ten))
         fun findeq((di,ch)::rest)=
            if A.Compare(di,dig)=SOME EQUAL then ch else
            findeq(rest)
         val digit=
            findeq[(zero,#"0"),(one,#"1"),(two,#"2"),(three,#"3"),
                   (four,#"4"),(five,#"5"),(six,#"6"),(seven,#"7"),
                   (eight,#"8"),(nine,#"9")]
         val quot=valOf(A.div(x,ten))
      in
         if A.Compare(quot,zero)=SOME EQUAL then digit::sofar
         else get_digs(digit::sofar,quot)
      end
      val is_negative=A.Compare(x,zero)=SOME LESS
      val absx=if is_negative then A.neg(x) else x
      val digits=get_digs([],absx)
      val chars=if is_negative then #"~"::digits else digits
   in
      String.implode chars
   end   
   )
end            
             
            
              
           