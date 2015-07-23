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
signature LEXSTATE=
sig
   val comment_level:int ref (* Current depth of comment nesting *)
   val comment_start:int ref (* Start of the current outermost
       comment,if any *)
   (* the string_XXX are only significant while a string/character
      constant is being lexed.*)
   val string_start:int ref
     (* Start of the current string/character constant, or 
        negative if there isn't one *)
   val string_contents:word list ref
     (* Contents of the current string/character/JavaSymbol constant *)

   datatype StringType=CHAR|STRING|JAVASYMBOL|NOT  
   val string_type:StringType ref
   (* StringType is used during lexing to store what kind of
      string constant we are currently lexing (string, character or Java
      Symbol); it is set to NOT otherwise.
      *)
   val sourcemap:SourceMap.sourcemap ref
   val errors:Error.Error list ref (* Errors so far *)

   val reset:unit->unit (* Use this at the start of a new file. *)
   val err:Error.Error->unit (* append error to the errors list *)
   
   val frozen:bool ref (* If true, we forget MLJ basis-only reserved words *)
end
