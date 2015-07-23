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

(*======================================================================*)
(* TextIO structure.                                                    *)
(* This deviates from the standard basis in the following ways:         *)
(*   1) We do not implement the StreamIO substructure                   *)
(*      (substructures aren't implemented yet)                          *)
(*   2) We do not implement the getPosIn, setPosIn, mkInstream,         *)
(*      getInstream, setInstream, getPosOut, setPosOut, mkOutstream,    *)
(*      getOutstream, setOutstream, and scanStream functions            *)
(*      (they need StreamIO)                                            *)
(*   3) We do not implement outputSubStr                                *)
(*      (Andrew hasn't implemented substrings yet)                      *)
(*   4) openAppend raises an Io exception indicating that it isn't      *)
(*      implemented.                                                    *)
(*      (I can't work out how to do it in Java)                         *)
(*======================================================================*)
structure TextIO:>TEXT_IO=
struct
   open General
   open List
   open Bool
   open Int
   open Option
   val op= = Prim.=

   type vector=string
   type elem=char
   structure J = java.io
   open java.io

   type instream = BufferedInputStream
   type outstream = BufferedOutputStream

   (* name_reader and name_writer are the names to be associated with
      the reader and the writer for this stream, when we get around to
      implementing it.  Currently they are used in the IO.Io exception *)
   val name_reader= "java.io.BufferedInputStream"
   val name_writer= "java.io.BufferedOutputStream"
  

   (* The BufferedInputStream/BufferedOutputStream classes write to and
      read from byte arrays.  Therefore we need b2s, s2b, and ss2b:
      *)
   fun b2s (bytes: Java.byte array,length : int ) : string =
      _new java.lang.String (bytes,0,0,length)

   fun s2b ( s:string ) : Java.byte array =
   let
      val len = String.size s
      val bytes : Java.byte array = Prim.newarray (len)
      val _ = s.#getBytes(0,len,bytes,0)
   in
      bytes
   end     

   fun ss2b ( s:Substring.substring ) =
   let
      val (base_string,index,len)=Substring.base s
      val bytes:Java.byte array = Prim.newarray (len)
      val _ = base_string.#getBytes (index,index+len,bytes,0)
   in
      bytes
   end

   (* Unfortunately the Java IOException cannot also be the ML IO exception,
      because the latter has a form fixed by the standard.  So we put a 
      catch clause in every function which might raise the Java IOException
      which calls the toIOexn.
      *)
   exception JavaIOException=java.io.IOException

   datatype Dir=READING|WRITING
   fun name dir=
      (case dir of
         READING=>name_reader
      |  WRITING=>name_writer
      )

   fun toIOexn(exn:exn,fname:string,dir:Dir)=
   (* ex is the exception raised; fname is the name of the STREAM_IO function
      which caused it to be raised; is_reading is true if the function was
      reading (rather than writing) *)
   
      raise IO.Io 
        {name=name dir,
         function=fname,
         cause=exn
         } 

   exception END_OF_FILE
   (* Now for exceptions we make ourselves *)
   fun eof(fname:string,dir:Dir)=
      raise IO.Io
         {name=name dir,
          function=fname,
          cause=END_OF_FILE
          }

   exception NOT_IMPLEMENTED
   fun non_imp(fname:string,dir:Dir)=
      raise General.NotImplemented
         (String.concat["TextIO.",fname,": not implemented"])
  
   fun input(is:instream)=
   let
      val b1 = is.#read()
   in
      if b1<0 
      then ""
      else 
      let    
         val avail = is.#available ()
         val blen = MLJUtils.Int.+(avail, 1)
         val bytes : Java.byte array = Prim.newarray (blen)
         val _= Array.update(bytes,0,Prim.i2b b1)
         val got= is.#read (bytes,1,avail)
         val got'= if got<0 then 0 else got
         (* got will be <0 if we have already reached end-of-stream *)
         val totlen = MLJUtils.Int.+(1,got') 
         (* It is conceivable that inbetween the call to avail and the
            read the number of bytes available shrank somehow so totlen
            may not equal blen *)
      in
         b2s(bytes,totlen)
      end
   end handle ex as JavaIOException => toIOexn(ex,"input",READING)


   fun input1(is:instream)=
   let
      val b1 = is.#read()
   in
      if b1<0 then NONE
      else SOME(Prim.i2c b1)
   end handle ex as JavaIOException => toIOexn(ex,"input1",READING)

   fun inputN(is:instream,n:int)=
   let
      val resbytes: Java.byte array = Prim.newarray (n)
   in
      let
         val got= is.#read(resbytes,0,n)
         val got'= if got<0 then 0 else got
         (* got will be <0 if we have already reached end-of-stream *)
      in
         b2s(resbytes,got')
      end handle ex as JavaIOException => toIOexn(ex,"inputN",READING)
   end


   fun inputAll(is:instream)=
   let
      (* We do inputN on blocks of length 256 until we get a null string *)
      fun getnext()=inputN(is,256)
      fun getall sofar=
      let
         val next=getnext()
      in
         if next="" then List.rev sofar else getall(next::sofar)
      end
   in
      String.concat(getall [])
   end

   fun canInput(is:instream,n)=
      is.#available() >= n
   
   fun lookahead(is:instream)=
   (* We use synchronize to deal with the case that two threads are
      simultaneously looking ahead on the same stream. *)
   ((_synchronized is)  
     (let val _ = is.#mark (1)
         val res = input1 is 
         val _ = is.#reset ()
     in 
       res 
     end)) handle ex as JavaIOException => toIOexn(ex,"lookahead",READING)

   fun closeIn(is:instream)=
      (is.#close()) 
      handle ex as JavaIOException => toIOexn(ex,"closeIn",READING)
            
   fun endOfStream(is:instream)=
   (case lookahead is of
      NONE => true
   |  SOME _ => false
   )

   fun output(os:outstream,s)=
   let
      val bytes= s2b(s)
   in
      (os.#write (bytes,0, Array.length(bytes)); ())
      handle ex as JavaIOException => toIOexn(ex,"output",WRITING)
   end

   fun output1(os:outstream,ch:char)=
   let
      val b= Prim.c2i (ch)
      val _= os.#write (b)
   in
      {}
   end handle ex as JavaIOException => toIOexn(ex,"output1",WRITING)

   fun outputSubstr(os:outstream,s:Substring.substring)=
   let
      val bytes=ss2b s
      val _ = os.#write (bytes,0,Array.length(bytes))
   in
      {}
   end handle ex as JavaIOException => toIOexn(ex,"outputSubstr",WRITING)

   fun flushOut(os:outstream)=
      (os.#flush())
      handle ex as JavaIOException => toIOexn(ex,"flushOut",WRITING)
   
   fun closeOut(os:outstream)=
      (os.#close())
      handle ex as JavaIOException => toIOexn(ex,"closeOut",WRITING)

   (* structure StreamIO : STREAM_IO *)
   (* val getPosIn : instream -> StreamIO.in_pos *)
   (* val setPosIn : (instream * StreamIO.in_pos) -> unit *)
   (* val mkInstream : StreamIO.instream -> instream *)
   (* val getInstream : instream -> StreamIO.instream *)
   (* val setInstream : (instream * StreamIO.instream) -> unit *)
   (* val getPosOut : outstream -> StreamIO.out_pos *)
   (* val setPosOut : (outstream * StreamIO.out_pos) -> unit *)
   (* val mkOutstream : StreamIO.outstream -> outstream *)
   (* val getOutstream : outstream -> StreamIO.outstream *)
   (* val setOutstream : (outstream * StreamIO.outstream) -> unit *)
   (* --------End of IMPERATIVE_IO------------------ *)
   
   (* structure StreamIO : TEXT_STREAM_IO *)

   
   fun inputLine(is)=
   (* We do this crudely using input1.  Perhaps eventually the
      buffering of input1 will get inlined and it won't be so
      bad. *)
   if endOfStream is 
   then ""
   else let
      val line_buffer=StringBuffer.empty()
      fun read_rest()=
         (case input1 is of
            NONE => StringBuffer.appendChar(line_buffer,#"\n")
         |  SOME ch => 
              (StringBuffer.appendChar(line_buffer,ch);
               if ch= #"\n" then {} else 
                  read_rest()
               )
         )
      val _ = read_rest()
   in
      StringBuffer.toString line_buffer
   end

   fun openIn (s:string)=
   let
      val fis = _new FileInputStream (s)
      val res = _new instream (fis)
   in
      res
   end handle ex as JavaIOException => toIOexn(ex,"openIn",READING)
      
   fun openOut (s:string) =
   let
      val fos = _new FileOutputStream(s)
      val res= _new outstream (fos)
   in
      res
   end handle ex as JavaIOException => toIOexn(ex,"openOut",WRITING)
     
   fun openAppend s=non_imp("openAppend",WRITING)

   fun openString (s:string)=
   let
      val sis = _new StringBufferInputStream (s)
      val res= _new instream(sis)
   in
      res
   end handle ex as JavaIOException => toIOexn(ex,"openOut",READING)

   val stdIn= _pure (_new instream(java.lang.System.in))
   val stdOut= _pure (_new outstream (java.lang.System.out))
   val stdErr= _pure (_new outstream (java.lang.System.err))

   fun print s=(output(stdOut,s);flushOut stdOut) (* copied from the basis *) 

   structure StreamIO=
   struct
      (* We only need this for scanStream *)
      type instream=instream
      type outstream=outstream
   end     
    
   fun scanStream transformer (is:instream)=
   let
      fun char_reader(is:instream)=
      (case input1 is of
         NONE => NONE
      |  SOME ch => SOME(ch,is)
      )
      
      val new_reader=transformer char_reader
   in
      (case new_reader is of
         NONE => NONE
      |  SOME(res,_) => SOME res
      )
   end
end      

