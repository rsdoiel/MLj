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

(* Decode:>DECODE partially disassembles a class file so that type information
   can be extracted. 

   The only attribute decoded is the Exceptions attribute, which lists
   the checked exceptions a method may throw.  Other attributes; in particular
   the Code attribute and the ConstantValue attribute, are ignored.
   Constant pool entries only required by attributes we ignore are also 
   ignored).  However everything else should be decoded.  Also we attempt to
   check the format of what we decode.  However where we can't understand
   what might reasonably be a future or obsolete feature (such as
   an incomprehensible flag bit), and can keep 
   going, we ignore it.  For example if the ACC_SUPER flag is unset we
   currently ignore this.
*)
structure Decode:>DECODE=
struct
  
   exception ClassFile=Assert.ClassFile

   fun decode is=
   let
      val class_data=(Class.decode_class is 
         handle
            ReadInts.EOF => Assert.fail "Premature end of file"
         |  e => raise e
         )

      val _=Assert.assert(BinIO.endOfStream is,
"Extra mysterious characters at the end of the class file")
   in
      class_data
   end

   fun decode_file file_name=
   let
      val is=Open.open_file file_name
      val c=decode is
      val _=Open.close is
   in
      c
   end

   fun decode_zip(zipfile,string)=
   let
      val vec=Zip.zipInput(zipfile,string)
      val stream=Open.open_vector vec
      val c=decode stream
      val _=Open.close stream
   in
      c
   end
end

