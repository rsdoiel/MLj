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

(* BasicBinI:BASICBINI does basic binary input with direct
   access. *)
structure BasicBinI:>BASICBINI where type vector=Word8Vector.vector =
struct
   type vector=Word8Vector.vector
   type instream=
     {endPos:(unit->int),
      inputN:(int->vector),
      setPosIn:(int->unit),
      closeIn:unit->unit,
      name:string,
      chunkSize:int
      }

   exception NotDirectAccess

   exception EOF

   type in_pos=int
   fun openIn name=
   let
      val is=BinIO.openIn name 
      val fis=BinIO.getInstream is
      val (reader,_)=BinIO.StreamIO.getReader fis
   in
      (case reader of
         BinPrimIO.RD{
            endPos=SOME endPos,
            readVec=SOME inputN,
            setPos=SOME setPosIn,
            close=closeIn,
            chunkSize=chunkSize,
            ...
            }
         =>
           {endPos=endPos,
            inputN=inputN,
            setPosIn=setPosIn,
            closeIn=closeIn,
            name=name,
            chunkSize=chunkSize
            }
      |  _ =>
         raise IO.Io{
            name=name,
            function="BasicBinI.openIn",
            cause=NotDirectAccess
            }
      )
   end

   fun inputN(is:instream,i)= 
   let
      val ()=
         if i<0 (* best avoided since it can cause a SysErr. *) 
         then
            raise IO.Io{
               name= #name is,
               function= "BasicBinI.inputN",
               cause= EOF
               }
         else
            ()
             
      val result= #inputN is i
      val ()=
         if Word8Vector.length(result)<>i
         then
            raise IO.Io{
               name= #name is,
               function="BasicBinI.inputN",
               cause=EOF
               }
         else
            ()
   in
      result
   end

   fun endPos(is:instream)= #endPos is ()
   fun closeIn(is:instream)= #closeIn is ()
   fun setPosIn(is:instream,i)= #setPosIn is i
   fun chunkSize(is:instream)= #chunkSize is
   fun name(is:instream)= #name is
end






