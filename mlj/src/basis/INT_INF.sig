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
(* INT_INF signature                                                    *)
(* Implements infinite precision arithmetic.                            *)
(*======================================================================*)
signature INT_INF=
sig
   include INTEGER

   val divMod : (int * int) -> (int * int) 
   val quotRem : (int * int) -> (int * int) 
   val pow : (int * Int.int) -> int 
   val log2 : int -> Int.int 
   val orb : (int * int) -> int 
   val xorb : (int * int) -> int 
   val andb : (int * int) -> int 
   val notb : int -> int 
   val << : (int * Word.word) -> int 
   val ~>> : (int * Word.word) -> int 
end

