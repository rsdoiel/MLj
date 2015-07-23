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

(* DosTime:>DOSTIME constructs the current date and time in DOS format
   (which is used in Zip files) *)
structure DosTime:>DOSTIME=
struct
   type dostime=Word32.word (* This will be output in little endian order *)
   
   fun Dos mltime=
   let
      val i2w=Word32.fromInt
      fun shift(w,i)=Word32.<<(w,Word.fromInt i)
      fun orlist l=List.foldl Word32.orb 0w0 l
      fun month_number m=
      let
         open Date
      in
         (case m of
            Jan => 1
         |  Feb => 2
         |  Mar => 3
         |  Apr => 4
         |  May => 5
         |  Jun => 6
         |  Jul => 7
         |  Aug => 8
         |  Sep => 9
         |  Oct => 10
         |  Nov => 11
         |  Dec => 12
         )
      end

      val date=Date.fromTimeLocal mltime
      val year=Date.year date
      val month=Date.month date
      val day=Date.day date
      val hour=Date.hour date
      val minute=Date.minute date
      val second=Date.second date      

      val (y,mo,d,h,mi,s)=
      (* year, month, day, hour, minute and second as Word32s and offset 
         appropriately *)
      if year<1980 orelse year>=2108 then (0w0,0w1,0w1,0w0,0w0,0w0)
      (* Midnight beginning 1980 *)
      else 
         (i2w(year-1980),i2w(month_number month),i2w(day),
          i2w(hour),i2w(minute),i2w(second div 2))
      (* Dos dates ignore the bottom bit of the second *)
   in
      orlist 
        [shift(y,25),shift(mo,21),shift(d,16),
         shift(h,11),shift(mi,5),s]
   end

   fun now()=Dos(Time.now())
end   
