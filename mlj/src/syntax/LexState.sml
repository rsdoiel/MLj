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

(* The LexState structure contains state referred to by the lexer.  We use 
   this as an easier (and hopefully faster) alternative to passing args to 
   the lex function.  Note that this makes the lexer non-reentrant - if we 
   want to lex several files at once it will be necessary to copy this state 
   and restore it.
   *)
structure LexState:>LEXSTATE=
struct
   val comment_level=ref 0
   val string_start=ref ~1
   val string_contents:word list ref=ref []
   val comment_start=ref ~1

   datatype StringType=CHAR|STRING|JAVASYMBOL|NOT  
   val string_type=ref NOT
   val sourcemap=ref(SourceMap.new())
   val errors:Error.Error list ref=ref []
   (* This is also used by the parser! *)
   fun reset()=
     (comment_level:=0;
      comment_start:= ~1;
      string_start:= ~1;
      string_contents:=[];
      string_type:=NOT;
      sourcemap:=SourceMap.new();
      errors:=[]
      )

   fun err E=
     (errors:= E::(!errors))

   val frozen=ref false
end
