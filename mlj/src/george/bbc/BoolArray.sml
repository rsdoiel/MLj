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

(* BoolArray is a placeholder for when SML/NJ contains the real ML 96
   BoolArray structure.  Until then, this will contain a subset of
   the functions in MONO_VECTOR.  We use Word8Array arrays, with
   0w0 representing false and 0w1 representing true. *)
structure BoolArray=
struct
   type array=Word8Array.array
   type elem=bool
   val maxLen=Word8Array.maxLen

   fun b2w b=if b then 0w1:Word8.word else 0w0:Word8.word
   fun w2b(w:Word8.word)=(w<>0w0)

   fun array (i,b)=
      Word8Array.array(i,b2w b)

   fun fromList l=
      Word8Array.fromList(List.map b2w l)

   fun tabulate(i,f)=
      Word8Array.tabulate(i,b2w o f)

   fun length (arr)=Word8Array.length(arr)

   val sub=w2b o Word8Array.sub

   fun update(arr,i,b)=
      Word8Array.update(arr,i,b2w b)
end
