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

   Symbol is exactly like GeneralSymbol, only it reserves
   MLj-specific names using the Reserved structure. 
   *)
structure Symbol:>SYMBOL=
struct
   open GeneralSymbol
   val ()=Reserved.reserve() 
end


