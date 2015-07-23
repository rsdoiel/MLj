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

(* Open:>OPEN abstracts reading from binary files or Word8Vectors
   (other possibilities may be available later) *)
structure Open:>OPEN=
struct
   fun open_file s=BinIO.openIn s
   fun open_vector v=
   let
      (* We use mkInstream and insert v as the "already read"
         vector.  We need a reader which reads nothing for the
         rest. *)
      val null_reader=BinPrimIO.RD 
        {name="Null reader",
         chunkSize=1,
         readVec=SOME(fn _=>Word8Vector.fromList[]),
         readArr=NONE,
         readVecNB=NONE,
         readArrNB=NONE,
         block=NONE,
         canInput=NONE,
         avail=(fn _ => SOME 0),
         getPos=NONE,
         setPos=NONE,
         endPos=NONE,
         verifyPos=NONE,
         close=(fn _ => {}),
         ioDesc=NONE
         }
   in BinIO.mkInstream(BinIO.StreamIO.mkInstream(null_reader,SOME v))
   end

   fun close is=BinIO.closeIn is
end

