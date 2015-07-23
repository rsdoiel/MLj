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

(* Symbols are hashed JavaStrings which support fast equality testing
   and hashing.  We keep a record of all the symbols made so far so
   that we don't have to make them again. 

   WARNING.  Symbols will not work in a multi-threaded environment
   unless it is revised and locks put in at critical sections. 
   *)
signature SYMBOL=
sig
   type symbol
   val symbol:JavaString.t -> symbol
   val symbolAsciiStringSlice:string*int*int -> symbol
   (* symbolAsciiStringSlice takes a slice of a string, which
      must contain only characters with codes 1-127 (inclusive)
      and turns it into the corresponding symbol *)

   val symbolAsciiString:string -> symbol
   (* symbolString turns an ASCII string into a symbol.  It is not
      as fast as it might be. *)

   val toJavaString:symbol->JavaString.t

   structure HashKey:HASH_KEY where type hash_key=symbol
   structure OrdKey:ORD_KEY where type ord_key=symbol

   structure OrdMap:ORD_MAP where type Key.ord_key=symbol
   structure OrdSet:ORD_SET where type Key.ord_key=symbol

   val equal:symbol*symbol->bool

   val bucketSizes:unit->int list
   (* Returns bucket sizes of symbol hash table *)

   (* The following functions reveal too much of the
      workings of this structure and should ONLY be used
      by the TokenTable functor in syntax. *)
   val number:symbol->int
   (* Number the symbols introduced from 0 (the first one) onwards. *)
end


