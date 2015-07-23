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
(* Check the syntactic restrictions for a declaration expression, and	*)
(* resolve                                                              *)
(*   (a) infix expressions, patterns and fun decs;                      *)
(*   (b) implicitly scoped type variables.                              *)
(*======================================================================*)
signature SYNTAXCHECK = 
sig

val check : 
  {
    entity    : Entity.Ref,           (* The source of this topdec *)
    AST       : Syntax.Dec,           (* The topdec itself *)
    sourcemap : SourceMap.sourcemap   (* Its associated location map *)
  } 
  ->
  {
    AST       : Syntax.Dec,           (* The resolved topdec *)
    errors    : Error.Error list      (* A list of errors and warnings *)
  }  

end