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

(* PrintLoc:>PRINTLOC pretty-prints syntax positions and locations
   to strings 
   *)
structure PrintLoc:>PRINTLOC=
struct
   val SOME im=Int.maxInt (* notional position of EOF *)
   
   datatype position=
      LC of {line:int,col:int}
   |  EOF (* end of file *)
   
   fun i2pos(s,i)=
      if i=im 
      then
         EOF
      else
         LC(SourceMap.decode(s,i))

   fun pos2string(LC{line,col})=
      (String.concat[
         Int.toString line,
         ".",
         Int.toString col
         ])
   |   pos2string EOF= "EOF"
   
   fun simplepospos2string(x,y)=
      String.concat[
         pos2string x,
         "-",
         pos2string y
         ]
   
   fun pospos2string(x:position,y:position)=
     (case (x,y) of
         (LC{line=line1,col=col1},LC{line=line2,col=col2})=>
            if line1=line2 
            then
               String.concat[
                  Int.toString line1,
                  ".",
                  Int.toString col1,
                  "-",
                  Int.toString col2
                  ]
            else
               simplepospos2string(x,y)
      |  _ => simplepospos2string(x,y)
      )

   fun position2string(s,p)=pos2string(i2pos(s,p))

   fun location2string(s,{left,right})=
      if left>=right
      then
         pos2string(i2pos(s,left))
      else
         pospos2string(i2pos(s,left),i2pos(s,right))
end
