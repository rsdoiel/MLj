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

signature STREAM_IO =
sig
     type elem
     type vector
(*
     type reader
     type writer
     type instream
     type outstream
     type in_pos
     type out_pos
     type pos
     val input : instream -> (vector * instream) 
     val input1 : instream -> (elem * instream) option 
     val inputN : (instream * int) -> (vector * instream) 
     val inputAll : instream -> vector 
     val canInput : (instream * int) -> int option 
     val closeIn : instream -> unit 
     val endOfStream : instream -> bool 
     val mkInstream : (reader * vector) -> instream 
     val getReader : instream -> (reader * vector) 
     val getPosIn : instream -> in_pos 
     val setPosIn : in_pos -> instream 
     val filePosIn : in_pos -> pos 
     val output : (outstream * vector) -> unit 
     val output1 : (outstream * elem) -> unit 
     val flushOut : outstream -> unit 
     val closeOut : outstream -> unit 
     val setBufferMode : (outstream * IO.buffer_mode) -> unit 
     val getBufferMode : outstream -> IO.buffer_mode 
     val mkOutstream : (writer * IO.buffer_mode) -> outstream 
     val getWriter : outstream -> (writer * IO.buffer_mode) 
     val getPosOut : outstream -> out_pos 
     val setPosOut : out_pos -> outstream 
     val filePosOut : out_pos -> pos 
*)
end