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

(* Zip:ZIP contains functions for reading and writing Zip files.
   At the moment compressed formats are NOT supported, nor is there any way
   of doing more sophisticated Zip archive functions such as updating files.
   Also, we don't support the Zip option which allows the file size and CRC
   to come after the file, because it's messy to implement (though it could
   be if we come across a Zip file written on a sufficiently naff machine
   to need it). 
 
   The format is described in appnote.iz, in this directory.  This file was 
   provided by Info ZIP, a public domain rival to PKWARE and can be found at.
   ftp://ftp.cdrom.com/pub/infozip/doc/
   Info ZIP's home page is at
      http://www.cdrom.com/pub/infozip/
   but in case it moves again I have saved a copy in infozip.html
   PKWARE, who produced the original PKZIP, are in
      http://www.pkware.com/

   IO errors are signalled by raising Io.IO, as with other ML I/O
   routines.  We also use Io.IO for zip formatting errors (see note at the
   end of this file).

   Currently files to be archived can only be read and written in one go,
   to and from a Word8Vector.vector. This is reasonable given that we are only 
   planning to use this code for Java class files we have already compiled, 
   and also means that Zip files can be written using ordinary write 
   operations on a BinIO stream.  In the zip file format, 
   each file is preceded by a header 
   containing its size.  If we later wanted to allow zipped files to
   be written as they were created it would be possible but you'd have to
   use BinIO.setPosOut or some similar method to patch the length byte
   in, or else add 16 bits per file by putting them in an extra
   data descriptor.

   We use InfoZip's zip (version 2.1) and unzip (version 5.12) as
   reference implementations.
*)
signature ZIP=
sig
   type zipIn
   type zipOut

   datatype compression_mode=STORED|UNKNOWN of Word32.word
   (* STORED means not compressed.  UNKNOWN w means an unknown mode with code w. *)
   val zipArchiveOpenIn:string->zipIn
   (* openZipIn opens the file with name string as a zip file to read from. *)
   val zipInput:zipIn*string->Word8Vector.vector
   (* read the whole file with name the given string, and return its contents. *)
   val zipArchiveCloseIn:zipIn->unit
   (* it's probably good to do this so that the machine can get rid of
      unused buffers, but unlike zipArchiveCloseOut, it is not necessary. *)
   val zipExists:zipIn*string->bool
   (* zipExists(zipfile,name) returns true if there is a file called name in zipfile *)
   val zipDir:zipIn->string list
   (* zipDir returns the list of all zip files in the archive *)

   val zipArchiveOpenOut:string->zipOut
   (* openZipOut opens the file with name string as a zip file to write to. 
      zipArchiveCloseOut MUST be called afterwards for the zip file to be
      valid. *)
   val zipOutput:zipOut*string*Word8Vector.vector*compression_mode->unit
   (* add a new file with name string and contents the given vector to the
      open zipfile.  WARNING: there is no check to verify that a file with this
      name has not been written before.
      *)
   val zipArchiveCloseOut:zipOut->unit
   (* this should be called when all the files have been written *)

   (* ERRORS.  We do not attempt to handle IO errors; that is the caller's responsibility.
      Errors can be divided into those caused by IO errors (EG access, disk full, or
      IO.RandomAccessNotSupported) and formatting errors in Zip archives we are trying
      to read.  For the latter, we throw an IO.Io exception with fields as follows: 

      1) name
         for zipArchiveOpenIn, zipArchiveCloseIn
            the name of the archive file.
         for zipInput
            the null string
         
         The reason why zipInput returns "" is that it is otherwise not
         clear whether to return the name of the whole archive or of the individual
         file or both.
      2) function
         The name of the function prefixed by the name of this structure; 
         eg for zipArchiveOpenIn "Zip.zipArchiveOpenIn".
      3) exn.  One of the following exceptions (this list is subject to change).
      *)
   exception FormatError of string (* file is not a valid Zip archive, for the given reason. *)
   exception NotFound (* for zipInput, there is no file in the archive with that name *)
   exception UnknownCompressionMode of Word32.word 
   (* for zipInput, this file is stored in a compression mode we can't read.  The word 
      argument is the number of the compression method in the file (see appnote.iz for the
      meanings of various values). *)
   exception DataDescriptorNotImplemented
   (* for zipArchiveOpenIn.  raised if we come across a file where the
      length etcetera is stored after the file rather than before. *)
end





