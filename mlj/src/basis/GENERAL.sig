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

signature GENERAL =
sig
     eqtype unit
     type exn

     exception Bind 
     exception Chr
     exception Div
     exception Domain
     exception Fail of string 
     exception Match
     exception Overflow
     exception Size
     exception Span
     exception Subscript
     exception NotImplemented of string
     datatype order = datatype Datatypes.order
     val exnName : exn -> string 
     val exnMessage : exn -> string 
     val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c 
     val before : ('a * unit) -> 'a 
     val ignore : 'a -> unit     
     val ! : 'a ref -> 'a 
     val := : ('a ref * 'a) -> unit 
end
