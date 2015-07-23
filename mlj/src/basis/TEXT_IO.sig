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
(* TEXT_IO signature.                                                   *)
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
signature TEXT_IO=
sig
   (* Apart from the sharing constraints, this is more or less copied from
      the standard basis *)
   (* We would include IMPERATIVE_IO here but can't do inclusions. *)
   (* --IMPERATIVE_IO signature, with some bits commented out, starts here-- *)
      type vector=string
      type elem=char
      type instream
      type outstream
      val input : instream -> vector
      val input1 : instream -> elem option
      val inputN : (instream * int) -> vector
      val inputAll : instream -> vector
      val canInput : (instream * int) -> bool
      val lookahead : instream -> elem option
      val closeIn : instream -> unit
      val endOfStream : instream -> bool
      val output : (outstream * vector) -> unit
      val output1 : (outstream * elem) -> unit
      val flushOut : outstream -> unit
      val closeOut : outstream -> unit
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
   
   structure StreamIO : TEXT_STREAM_IO

   val inputLine : instream -> string
   val outputSubstr : (outstream * Substring.substring) -> unit
   val openIn : string -> instream
   val openOut : string -> outstream
   val openAppend : string -> outstream
   val openString : string -> instream
   val stdIn : instream
   val stdOut : outstream
   val stdErr : outstream
   val print : string -> unit
   val scanStream : ((Char.char,StreamIO.instream) StringCvt.reader ->
      ('a,StreamIO.instream) StringCvt.reader) -> instream -> 'a option
end      
   













