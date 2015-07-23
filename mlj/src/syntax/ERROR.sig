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
(* General error handling stuff						*)
(*======================================================================*)
signature ERROR = 
sig

type Error

val error     : Syntax.Location * string -> Error
val warning   : Syntax.Location * string -> Error
val print     : (string->unit) * SourceMap.sourcemap * Error list -> unit
(* print should only be used from Parse.sml (which packages a
   friendlier print function for each file containing the source map). *)
val isSerious : Error -> bool
val append    : Error * string -> Error

end
