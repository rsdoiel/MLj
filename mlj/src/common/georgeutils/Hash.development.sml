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

(* This contains various functions used in the development of the
   Hash function (including functions for testing it) *)
structure Hash=
struct
   fun GeneraltestHash m (hfun:string->Word32.word) (words:string list)=
   let
      val arr=Array.array(m,0)
      val ()=
         List.app
            (fn word =>
               let
                  val index=
                     Word32.toInt(Word32.mod(hfun word,Word32.fromInt m))
               in
                  Array.update(arr,index,Array.sub(arr,index)+1)
               end
               )
            words
      val n=List.length words
      val sum=
         Array.foldl
            (fn(i,sf) => i*(i+1)+sf)
            0
            arr
      val nr=Real.fromInt n
      val mr=Real.fromInt m
      val sumr=Real.fromInt sum
   in
      sumr/((nr/mr)*(nr+2.0*mr-1.0))
   end

   val testHash=GeneraltestHash 1024
      

   fun GeneralhashString (a,b,c,d) s=
   let
      fun sub i=
         Word32.fromInt(Char.ord(String.sub(s,i)) handle Subscript => 0)
      fun hash4(i,sf)=
         (i-4,a*sf+(sub i+(sub(i+2)*c)+(sub(i+1)*b+sub(i+3))*d))
      val len=String.size s
      fun gethash(i,sf)=
      if i<0 
      then 
         sf
      else
         gethash(hash4(i,sf))
      val len4=len-Int.rem(len,4)
   in
      gethash(len4,0w0)
   end


   val hashString=GeneralhashString(0w17,0w29,0w255,0w131)


   fun rfile fname=
   let
      val is=TextIO.openIn fname
      fun dr sf=
         if TextIO.endOfStream is
         then
            sf
         else
         let
            val line=TextIO.inputLine is
            val word=substring(line,0,String.size line-1)
         in
            dr(word::sf)
         end
      val res=dr []
      val ()=TextIO.closeIn is
   in
      res
   end

   fun vs(n,d)=
   let
      fun i2s i=StringCvt.padLeft #"0" d (Int.toString i)
   in
      List.tabulate(n,fn i=>"v"^i2s i)
   end

   val letters="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
   val letstrings=
      List.tabulate(52,
         fn i=>CharVector.fromList [CharVector.sub(letters,i)]
         )

   fun crossprod(f,l1,l2)=
      List.foldl
         (fn (l2el,sf)=>
            List.foldl
               (fn (l1el,sf) => f(l1el,l2el)::sf)
               sf
               l1
               
            )
         []
         l2

   fun ac 0=[""]
   |   ac n=crossprod(String.^,letstrings,ac (n-1))
end
