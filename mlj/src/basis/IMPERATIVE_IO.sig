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

signature IMPERATIVE_IO =
sig
     structure StreamIO : STREAM_IO
     type vector = StreamIO.vector 
     type elem = StreamIO.elem 
     type instream
     type outstream
     val input : instream -> vector 
     val input1 : instream -> elem option 
     val inputN : (instream * int) -> vector 
     val inputAll : instream -> vector 
     val canInput : (instream * int) -> int option 
     val lookahead : instream -> elem option 
     val closeIn : instream -> unit 
     val endOfStream : instream -> bool 
     val output : (outstream * vector) -> unit 
     val output1 : (outstream * elem) -> unit 
     val flushOut : outstream -> unit 
     val closeOut : outstream -> unit 
(*
     val getPosIn : instream -> StreamIO.in_pos 
     val setPosIn : (instream * StreamIO.in_pos) -> unit 
     val mkInstream : StreamIO.instream -> instream 
     val getInstream : instream -> StreamIO.instream 
     val setInstream : (instream * StreamIO.instream) -> unit 
     val getPosOut : outstream -> StreamIO.out_pos 
     val setPosOut : (outstream * StreamIO.out_pos) -> unit 
     val mkOutstream : StreamIO.outstream -> outstream 
     val getOutstream : outstream -> StreamIO.outstream 
     val setOutstream : (outstream * StreamIO.outstream) -> unit 
*)
end