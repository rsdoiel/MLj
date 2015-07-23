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

(* SourceMap implements the map from character positions in a file to 
   pairs (line no,column no).
   
   Unlike the corresponding SML/NJ structure it is fairly free of features
   (but hopefully cheaper).
   *)
structure SourceMap:>SOURCE_MAP=
struct
   structure DArray=DynIntArray

   val line_offset=0 
   val column_offset=1
   (* Internally we think of lines and columns as being numbered from 0 
      (because that's how ML does arrays), but add these numbers to get the 
      output of decode. 

      HACK.  The toplevel function (in Parse.sml) adds a fictitious
      newline to the start of each file.  This is because at the moment,
      ml-lex starts yypos at 2 (but may later be changed to start it at
      some sensible number).  So we have an imaginary line 0.
      *)

   type sourcemap=DArray.array*int ref 
   (* The int is the number of the current line.  arr[i] is the number of the
      newline character at the end of line i.  Here lines and characters are
      numbered from 0.
      *)

   fun new()=(DArray.array 100,ref 0)

   fun newline((arr,refl as ref l),pos)=
   let
      val ()=DArray.update(arr,l,pos)
      val ()= refl:= l+1
   in
      ()
   end
   
   fun decode((arr,refl as ref l),pos)=
   let
      (* find the least j in [0,l-1] such that arr[j]>=pos, or return l if all
         elements of arr[0:l-1] are <pos.  Then the internal line number is j
         and the internal column number is pos-arr[j-1]-1.
         *)
      fun bchop(lower,upper)=
      (* Define arr[l]>=pos.    
         Then this function is called when arr[lower]<pos<=arr[upper],0<=lower<upper and we
         want to find the least j with arr[j]>=pos and arr[j-1]. *)
      let
         val middle=Int.quot(lower+upper,2)
         val try=DArray.sub(arr,middle)
      in
         if try<pos
         then
            if middle=lower
            then
               (upper,try)
            else
               bchop(middle,upper)
         else
            bchop(lower,middle)
      end

      val (line_no,last_nl)=
         if l=0 orelse DArray.sub(arr,0)>=pos
         then
            (0,~1)
         else
            bchop(0,l)
   in
     {line=line_no+line_offset,
      col=(pos-last_nl)+(column_offset-1)
      }
   end

   val eof_pos=
     (case Int.maxInt of
        SOME x => x
     )

   fun eof(arr,refl as ref l)=
   let
      val ()=DArray.update(arr,l,eof_pos-1)
      val ()=refl:= (l+1)
   in
      eof_pos
   end
end
