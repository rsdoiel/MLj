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

structure General : GENERAL =
struct
  
     (* These must appear first, and in this order, to generate the
        correct hard-wired exnames *)
     exception Bind
     exception Match

     type unit = {}
     type exn = exn
     datatype order = datatype Datatypes.order

     exception Subscript = java.lang.IndexOutOfBoundsException
     exception Size = java.lang.NegativeArraySizeException
     exception Overflow
     exception Domain
     exception Div = java.lang.ArithmeticException
     exception Chr
     exception Fail of string 
     exception NotImplemented of string
     exception Span

     fun x before y = x
     fun ignore x = ()
     fun (f o g) x = f (g x)

     val ! = Prim.!
     val op:= = Prim.:=

     fun exnName (e : exn) = 
       case e of
         Subscript => "General.Subscript"
       | Size => "General.Size"
       | Div => "General.Div"
       | _ => 
         if Prim.isMLExn e
         then Prim.exnName e
         else Prim.unsafeValOf(e.#toString())

(*......................................................................*)
(* We've got a similar problem to above!				*)
(*......................................................................*)
     fun exnMessage (e : exn) =
       case e of
         Subscript => "General.Subscript"
       | Size => "General.Size"
       | Div => "General.Div"
       | _ => 
         if Prim.isMLExn e
         then Prim.exnMessage e
         else Prim.unsafeValOf(e.#toString())

end
