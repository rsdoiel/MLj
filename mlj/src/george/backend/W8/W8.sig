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

(* W8 implements vectors of bytes in such a way that appending and
   concatenation can be done in O(1) (apart from whatever happens
   with the garbage collector). *)
signature W8=
sig
   type vector (* type of the vector *)

   val fromvList:Word8Vector.vector list->vector
   val concat:vector list->vector
   (* fromList and concat take O(list length) and O(1) time.
      The following functions do not: *)
   val length:vector->int
   val first:vector->Word8.word
   (* first finds the first element of the vector *)

   (* Here are some additional functions *)
   val v1:Word8.word->vector
      (* produces a vector containing just one element *)
   val fromList:Word8.word list->vector
      (* produces a vector containing the words in the list *)

   val v1l:Word8.word*(Word8Vector.vector list)->vector
      (* produces a vector containing the concatenation of the word and
         the elements of the list *)

   val vv1:Word8Vector.vector->vector
      (* produces a vector containing just one Word8Vector vector. *)

   val combine:vector list->vector
      (* this combines several vectors using a common Java method:
         the number of vectors (as a 2-byte unsigned integer) is concatenated
         with the vectors themselves. *)


   val flatten:vector->vector
      (* simplifies Word8Vector so that length, sub and toWord8s take unit
         time.  Done twice on the same vector takes only O(1) time longer
         than doing it once, so sub and toWord8s do it automatically
         to their arguments, but this does not persist so if several such
         operations have to be done it is a good idea to flatten the vector
         first. *)
   val toWord8:vector->Word8Vector.vector
      (* returns the equivalent Word8Vector.vector *)

   val output:BinIO.outstream*vector->unit
       (* outputs the vector on the given binary stream *)
   val prettyprint:vector->string
       (* produces a string which prettyprints the contents of the
          vector *)
end


