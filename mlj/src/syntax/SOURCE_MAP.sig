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

(* SourceMap implements the map from character positions in a file to 
   pairs (line no,column no).
   
   Unlike the corresponding SML/NJ structure it is fairly free of features
   (but hopefully cheaper).
   *)
signature SOURCE_MAP=
sig
   type sourcemap
   val new:unit->sourcemap
   val newline:sourcemap*int->unit
   (* newline(s,i) indicates a new line character at character i 
      (characters are numbered from 0) of the file.  i should be non-negative
      and greater than all i's on previous calls with this s. 

      There is a hack added to work around a bug in ml-lex, which assumes
      the file has an additional newline added at the very start of the
      file.
      *)
   val decode:sourcemap*int->
     {line:int,col:int}
   (* decode returns the line and column number for this character position.
      Line and columns are numbered from 1. 
      The convention is that the EOF character occurs on line (last line + 1),
      column 1. *)
   val eof:sourcemap->int
   (* Finish off for file, returning the EOF position *)
end
