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
(* Datatypes for SML compilation units: `entities'.   			*)
(*======================================================================*)
structure Entity =
struct

(*----------------------------------------------------------------------*)
(* The type of an entity: functor signatures are reserved for future.	*)
(*----------------------------------------------------------------------*)
datatype Type = Sig | Str | Fun | FunSig

(*----------------------------------------------------------------------*)
(* A complete entity reference: type and source identitier.		*)
(*----------------------------------------------------------------------*)
type Ref = Type * Syntax.symbol

(*----------------------------------------------------------------------*)
(* Finite sets and maps for entity references				*)
(*----------------------------------------------------------------------*)
structure Ord = 
  struct 
    fun typeToInt Sig = 0
      | typeToInt Str = 1
      | typeToInt Fun = 2
      | typeToInt FunSig = 3 

    type ord_key = Ref
    fun compare((t1,s1), (t2,s2)) = 
      case Int.compare(typeToInt t1, typeToInt t2) of
        EQUAL => Symbol.OrdKey.compare(s1, s2)
      | other => other
  end

structure Map = MapFn(Ord)
structure Set = SetFn(Ord)

(*----------------------------------------------------------------------*)
(* Type used for file identities: includes the full path name of the 	*)
(* file (so no confusion when search paths are altered) and its 	*)
(* timestamp.                                                           *)
(*----------------------------------------------------------------------*)
type FileRef = string * Time.time

end

