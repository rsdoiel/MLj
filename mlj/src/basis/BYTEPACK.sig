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

(* BytePack:>BYTEPACK packs byte vectors into strings (to be
   Unicode-encoded) and back.
   It is intended to be in both the basis (used internally) and
   MLJ itself.
   *)
signature BYTEPACK=
sig
   type ('a,'b) vec = 
     {start:'b,
      reader:'b -> ('a * 'b) option
      }
   (* Uniform representation of sequences of elements with a pre-determined
      length and a function to step through the sequence. *)

   type 'b packt

   val pack:(Word8.word,'b) vec -> (Word.word,'b packt) vec
   (* converts byte vectors into strings (a character is represented
      by the corresponding word).  *)

   type 'b unpackt
   val unpack:(Word.word,'b) vec -> (Word8.word,'b unpackt) vec
   (* the reverse. *)

   val makeList:('a,'b) vec -> 'a list
   (* Handy utility function.  NB - the list is returned REVERSED. *)
end
