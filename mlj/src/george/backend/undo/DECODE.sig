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
signature DECODE=
sig
   val decode:BinIO.instream->Class.class_data
   (* the Open structure contains functions for creating an instream
      from a file name and from a Word8Vector.vector *)
   val decode_file:string->Class.class_data
   (* decode_file does the whole business of opening a file, decoding it, and closing it again. *)
   val decode_zip:Zip.zipIn*string->Class.class_data
   (* decode_zip is like decode_zip but reads the file from a zip archive
      (see the Zip structure). *)
   exception ClassFile of string 
   (* Thrown if the file does not appear to be a valid class file. *)
end






