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
signature BASICBINI=
sig
   type instream
   type in_pos=int 
   (* Character position from the start of the file. 
      The first character is number 0 *)
   type vector

   exception EOF
   (* Raised for end-of-file during inputN. *)

   exception NotDirectAccess
   (* Raised when a file doesn't support direct access
      (as required by openIn). *)

   val openIn:string->instream
   val inputN:instream*int->vector
   val closeIn:instream->unit

   val endPos:instream->in_pos
   val setPosIn:instream*in_pos->unit
   (* Set just before character n *)
   val chunkSize:instream->int
   (* Preferred size for reading *)
   val name:instream->string
   (* Name of the file *)
end
