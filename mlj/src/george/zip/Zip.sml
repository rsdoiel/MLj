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
structure Zip:>ZIP=
struct
   (* Magic words *)
   val local_header_magic=0wx04034b50:Word32.word
   val data_desc_magic=0wx08074b50:Word32.word
   val central_header_magic=0wx02014b50:Word32.word
   val end_of_central_magic=0wx06054b50:Word32.word

   datatype compression_mode=STORED|UNKNOWN of Word32.word
   (* STORED means not compressed.  UNKNOWN w means an unknown mode with code w. *)
   val zip_version=0w20:Word32.word 
   (* Zip files contain for each file a quantity supposed to be the 
      minimum version number required to read the file.  This
      is what we use (it has no effect on anything else).
      20 is probably excessive; however JDK1.0 uses it for classes.zip. 
      On input we ignore it. *)

   val hash_hint=2000
   (* Initial size we give to the hash table for reading.  This is 
      enough for the 1.1.1 classes.zip (about 1600 classes). *)
   exception FormatError of string 
   (* file is not a valid Zip archive, for the stated reason. *)
   exception NotFound 
   (* for zipInput, there is no file in the archive with that name *)
   exception UnknownCompressionMode of Word32.word 
   (* for zipInput, this file is stored in a compression mode we can't read.  
      The word argument is the number of the compression method in the file 
      (see appnote.iz for the meanings of various values). *)
   exception DataDescriptorNotImplemented
   (* for zipArchiveOpenIn.  raised if we come across a file where the
      length etcetera is stored after the file rather than before. *)

   (* *************************
  
      Functions for WRITING zip files
    
      *************************
   *)

   type out_file_data=
   (* Directory information we keep for each file in an archive we are writing. *)
     {mode:compression_mode,
      date:DosTime.dostime, 
(* This is in fact the time when the Zip file was opened. *)
      crc:Word32.word,
      compressed_size:Word32.word,
      size:Word32.word,
      filename:Word8Vector.vector, 
      offset:Word32.word
      }

   datatype zipOut=ZF_OUT of
     {stream:BinIO.outstream,
      date:DosTime.dostime, 
(* Date when we opened the Zipfile (which is used as the modification
   time for all files in the archive). *)
      directory:out_file_data list ref, 
(* directory is in reverse order (since we add later files on at the
   front) *)
      current_offset:Word32.word ref
      }

   fun zipArchiveOpenOut s=ZF_OUT
     {stream=BinIO.openOut s,
      date=DosTime.now(),
      directory=ref [], 
      current_offset=ref 0w0
      }

   local
      fun s2wv(s:string)=
      (* Word8Vector containing the text of s *)
         Word8Vector.tabulate(
            String.size s,
            fn i=> Word8.fromInt(Char.ord(String.sub(s,i)))
            )
      val i2w=Word32.fromInt
      val wlength=i2w o Word8Vector.length

      val LW4=LittleEnd.toLittle_W4
      val LW2=LittleEnd.toLittle_W2

      fun writevec(ZF_OUT {stream,current_offset,...},vec)=
      (* Write the vector to the Zip file, increasing the offset.
         All output goes through this function *)
         
      let
         val _= BinIO.output(stream,vec)
         val _= current_offset:= !current_offset+wlength vec
      in
         {}
      end

      fun mkwvec(zfile)=(fn vec=>writevec(zfile,vec))
      (* mkwvec makes a function for writing vectors from a zipfile,
         which is passed to other functions. *)

      fun Lmode(STORED)=LW2(0w0)

      (* Functions for writing files  *)
      fun write_local_file_header(wvec,
         {mode,date,crc,compressed_size,size,filename,offset})=
      (* This precedes the file data *)
      let
          val _= wvec(LW4 local_header_magic)
          val _= wvec(LW2 zip_version)
          val _= wvec(LW2 0w0) (* general purpose bit flag *)
          val _= wvec(Lmode mode)
          val _= wvec(LW4 date) (* date and time *)
          val _= wvec(LW4 crc)
          val _= wvec(LW4 compressed_size)
          val _= wvec(LW4 size)
          val _= wvec(LW2 (wlength filename))
          val _= wvec(LW2 0w0) 
(* length of extra field (which we don't use) *)
          val _= wvec filename
      in
          {}
      end

      fun write_file_header(wvec,
         {mode,date,crc,compressed_size,size,filename,offset})=
      (* This is the entry for the file in the central directory *)
      let
         val _= wvec(LW4 central_header_magic)
         val _= wvec(LW2 zip_version) 
         val _= wvec(LW2 zip_version)
         (* the first zip_version is the version that created the file;
            the second the minimum version needed to extract it *)
         val _= wvec(LW2 0w0) (* general purpose bit flag again *)
         val _= wvec(Lmode mode)
         val _= wvec(LW4 date) (* date and time *)
         val _= wvec(LW4 crc)
         val _= wvec(LW4 compressed_size)
         val _= wvec(LW4 size)
         val _= wvec(LW2 (wlength filename))
         val _= wvec(LW2 0w0) (* extra field length *)
         val _= wvec(LW2 0w0) (* file comment length *)
         val _= wvec(LW2 0w0) (* disk number start *)
         val _= wvec(LW2 0w1) 
         (* internal file attributes.  This value indicates a binary file *)
         val _= wvec(LW4 0w0) 
         (* external file attributes.  This value indicates the file came 
            from standard input *)
         val _= wvec(LW4 offset)
         val _= wvec filename
      in
         {}
      end       
   in          
      fun zipOutput(zfile as ZF_OUT{stream,date,directory,current_offset},
         fname,data,mode)=
      let
         val filename=s2wv fname
         val offset= !current_offset
         val size=wlength data
         val crc=CRC.crc data
         val (compressed_data,compressed_size)=
            (case mode of
               STORED => (data,size)
            ) 
         val fdata=
           {mode=mode,
            date=date,
            crc=crc,
            compressed_size=compressed_size,
            size=size,
            filename=filename,
            offset=offset
            }
         val wvec=mkwvec zfile
         val _=write_local_file_header(wvec,fdata)
         val _=wvec data
         val _= directory:= fdata:: !directory
      in
         {}
      end

      fun zipArchiveCloseOut(zfile as ZF_OUT{stream,date,directory,current_offset})=
      (* this should be called when all the files have been written *)
      let
         val dir= !directory
         val wvec=mkwvec zfile
         val central_start= !current_offset
         (* Write the directory *)
         val _= 
            List.app
               (fn fdata => write_file_header(wvec,fdata))
               (List.rev dir)
         val central_size= !current_offset - central_start
 
         val _=wvec(LW4 end_of_central_magic)
         val _=wvec(LW2(0w0)) (* This disk number *)
         val _=wvec(LW2(0w0)) (* Disk on which central dir started *)
         val nentries_int=List.length dir
         val _= if nentries_int>65535 then raise Fail 
"Too many classes to put in one ZIP file" else {} 
         val _=wvec(LW2(i2w nentries_int))
         val _=wvec(LW2(i2w nentries_int))
         val _=wvec(LW4(central_size))
         val _=wvec(LW4(central_start))
   
         fun make_zip_comment s=
         let
            val comment=s2wv s
         in
            Word8Vector.concat 
               [LW2(wlength comment),comment]
         end
         val _=wvec(make_zip_comment "")
      in
         BinIO.closeOut stream
      end
   end               

   (* *************************
  
      Functions for READING zip files
    
      *************************

      We get the directory information from the central directory,
      reading the whole thing in one go.
   *)

   structure StringHashKey:HASH_KEY=
   struct
      type hash_key=string
      val hashVal=HashString.hashString
      val sameKey=(op=)
   end

   structure StringHash=HashTableFn(StringHashKey)

   type in_file_data=
   (* Directory information we keep for each file in the archive we are reading. *)
     {mode:Word32.word,
      general_purpose_bit_flag:Word32.word, 
         (* This is only used for certain types of compression, but
            we read it anyway *)
      crc:Word32.word,
      (* the file name is stored as a key in the hash table *)
      compressed_size:int,
      position:int 
      (* this is the position of the start of the local file header.
         Note.  We reread the local file header because the extra field
         sometimes has different sizes in the local file header and
         the central file header; EG for Info-Zip. *)
      }

   datatype zipIn=ZF_IN of
     {stream:BasicBinI.instream,
      directory:in_file_data StringHash.hash_table,
(* directory is in reverse order (since we add later files on at the
   front) *)
      name:string (* name of the Zip file *)
      }


   fun zipInputExcep(is,exn)=
      IO.Io
         {name=BasicBinI.name is,
          function="Zip.zipInput",
          cause=exn
          }

   fun rZIE(name,cause)=
      raise
         IO.Io
           {name=name,
            function="Zip.zipArchiveOpenIn",
            cause=FormatError cause
            }

   fun rZIExn(name,exn)=
      raise
         IO.Io
           {name=name,
            function="Zip.zipArchiveOpenIn",
            cause=exn
            }

   fun read(is,n,l)=
   let
      val ()=BasicBinI.setPosIn(is,n)
   in
      BasicBinI.inputN(is,l)
   end

   fun getECD is=
   let
   (* Get the end of central dir record *)
      val epos=BasicBinI.endPos is
      val elen=22 (* Length of everything in central dir (apart from comment)
                     *)
      val start1=epos-elen
  
      val ecd=read(is,start1,elen)
      val word=LittleEnd.fromLittleSlice(ecd,0,4)
   in
      if word=end_of_central_magic
      then
         ecd
      else
      let
         (* Oh dear.  We have to skip past a Zip comment.  We assume it
            is not too long *)
         val to_read=Int.min(epos,8192)
         (* We could make this dependent on chunkSize but then
            I think that the zip files we can process should be
            machine-independent. *)
         val start2=epos-to_read
 
         val block=read(is,start2,to_read)
         (* Find the last occurrence of end_of_central_magic
            in block before block[to_read-elen].  Resist the
            temptation to optimise this *)
         fun find_it i=
            if i=0
            then
               rZIE(BasicBinI.name is,
                 "Couldn't locate directory; zip archive comment too long??")
            else
            let
               val index=i-1
               val word=LittleEnd.fromLittleSlice(block,index,4)
            in
               if word=end_of_central_magic
               then
                  Word8Vector.extract(block,index,SOME elen)
               else
                  find_it index
            end
      in
         find_it(to_read-elen)
      end
   end

   fun readECD(v:Word8Vector.vector)=
   (* Read end of central directory record, returning the
      index of the central directory, its size, and the number
      of entries in it *)
     {index=Word32.toInt(LittleEnd.fromLittleSlice(v,16,4)),
      size=Word32.toInt(LittleEnd.fromLittleSlice(v,12,4)),
      nentries=Word32.toInt(LittleEnd.fromLittleSlice(v,10,2))
      }

   fun readDirHeader(is,v,index)=
   (* Read the central directory header in v starting at index,
      returning it, the file name, and the index of the following byte. *)
   let
      fun fls(i,n)=LittleEnd.fromLittleSlice(v,index+i,n)
      val magic=fls(0,4)
      val ()=
         if magic<>central_header_magic
         then
            rZIE(BasicBinI.name is,
              "Bad magic word for central directory entry")
         else
            ()

      val mode=fls(10,2)
      val general_purpose_bit_flag=fls(8,2)
      val crc=fls(16,4)
      val compressed_size=Word32.toInt(fls(20,4))
      val lh_index=fls(42,4)
      val fname_length=fls(28,2)
      val ef_length=fls(30,2)
      val fc_length=fls(32,2)
      val position=Word32.toInt lh_index

      val fname_start=index+46
      val fname=
         CharVector.tabulate(Word32.toInt fname_length,
            fn i=>Char.chr(Word8.toInt(Word8Vector.sub(v,fname_start+i))))

      val next_index=fname_start+Word32.toInt(fname_length+ef_length+fc_length)
   in
     ({
         mode=mode,
         general_purpose_bit_flag=general_purpose_bit_flag,
         crc=crc,
         compressed_size=compressed_size,
         position=position
         },
      fname,
      next_index
      )
   end

   fun readDir(is,{index,size,nentries})=
   (* Read the directory, returning the complete Zip Archive *)
   let
      val dir=read(is,index,size)

      val directory=StringHash.mkTable(hash_hint,zipInputExcep(is,NotFound))
      
      fun read_entries(nleft,index)=
         if nleft=0 
         then 
            ()
         else
         let
            val (entry,name,next_index)=readDirHeader(is,dir,index)
            val ()=StringHash.insert directory (name,entry)
         in
            read_entries(nleft-1,next_index)
         end
      
      val ()=read_entries(nentries,0)
   in
      ZF_IN 
        {stream=is,
         directory=directory,
         name=BasicBinI.name is
         }
   end handle Subscript =>
      rZIE(BasicBinI.name is,"Mysteriously truncated central directory")

   fun zipArchiveOpenIn name=
   let
      val is=BasicBinI.openIn name

      (* Is it a zip file? *)
      val start=BasicBinI.inputN(is,4)
      val start_word=LittleEnd.fromLittle start
      val () =
         if start_word=local_header_magic orelse 
            start_word=end_of_central_magic
         then
            ()
         else
            rZIE(name,"File isn't a ZIP file")
   in
      readDir(is,readECD(getECD is))
   end handle Overflow => 
      rZIE(name,"Zip contains huge (>=2^31 bytes), or funny index")
             


   fun zipArchiveCloseIn(ZF_IN{stream,...})=BasicBinI.closeIn stream
      (* we just close the file.  We could also zap the hash table, but for now
         we'll leave that to the garbage collector, on the assumption that
         the user drops any references to the stream. *)

   fun zipExists(ZF_IN{directory,...},filename)=
   (case StringHash.find directory filename of
      SOME _ => true
   |  NONE => false
   )

   fun zipDir(ZF_IN{directory,...})=
      List.map #1 (StringHash.listItemsi directory)

   fun zipInput(ZF_IN{stream,directory,...},filename)=
   let
      fun decode_mode(0w0)=STORED
      |   decode_mode w =UNKNOWN w
      val {mode,crc,compressed_size,position,...}=
         StringHash.lookup directory filename
   
      val decomp = 
      (case decode_mode mode of
         UNKNOWN w => 
            raise zipInputExcep(stream,UnknownCompressionMode w)
      |  STORED => (fn vector => vector)
      )

      (* Get 4 bytes for file name and extra field.  This means
         doing two low-level reads, one to get the extra field and one to
         get the file contents.  However, from what I know of
         Unix systems, these are likely to be buffered anyway.
         *)
      val flenelen= read(stream,position+26,4)
      val flen=LittleEnd.fromLittleSlice(flenelen,0,2)
      val elen=LittleEnd.fromLittleSlice(flenelen,2,2)

      val contents_index=position+Word32.toInt(flen+elen+0w30)
      
      val compressed_vector = read(stream,contents_index,compressed_size)

      val uncompressed_vector = decomp compressed_vector

      val () =
         if CRC.crc(uncompressed_vector) <> crc 
         then
            raise zipInputExcep(stream,FormatError "CRC mismatch")
         else
            {}

   in
      uncompressed_vector
   end
end



