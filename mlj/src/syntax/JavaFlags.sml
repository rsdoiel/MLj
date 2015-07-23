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

(* JavaFlags contains the datatype for flags available in
   MLJ extensions.  These are munged together so that ml.grm
   can collect them up before knowing what is being modified.
   
   Arguably they should be munged together anyway, and 
   the distinction in the backend between class, field, and
   method flags should be dropped.  This would make it less
   tiresome to use the backend directly (since you wouldn't
   have to say Field.PRIVATE and so on), and would be simpler
   to program in some ways since all have the same bit value
   in Java bytecode.  *)
structure JavaFlags=
struct
   datatype javaflag=
      ABSTRACT
   |  FINAL
   |  PRIVATE
   |  PROTECTED
   |  PUBLIC
   |  STATIC
   |  SYNCHRONIZED
   |  TRANSIENT
   |  VOLATILE
   |  INTERFACE (* INTERFACE does not correspond to any MLJ keyword but
                   is added automatically for methods declared with
                   _interfacetype *)
end



