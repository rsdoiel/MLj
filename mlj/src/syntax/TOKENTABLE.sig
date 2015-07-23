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

(* TokenTable is the structure that looks up reserved words in ML, and 
   creates symbols for others.  It is functorised on MLJ_TOKENS. 

   For speed, this structure uses the SML/NJ unchecked array operations.
   *)
signature TOKENTABLE=
sig
   type token (* = (MLJ_TOKENS.svalue,Syntax.Position) MLJ_TOKENS.token *)

   type input= (* Format of arguments to functions in this structure *)
     {characters:string, (* Where the tokens come from *)
      initial:int, (* Index of first character in the token *)
      length:int, (* Length of the token *)
      position:Syntax.Position
      }

   val lookup_tyvar:input->token
   (* Look up a type variable.  The quote is included in the string but 
      excluded from the token. *)

   val lookup_alpha:input->token
   (* Look up an alphabetic identifier.  We recognise reserved words.
      *)

   val lookup_symbolic:input->token
   (* Look up a symbolic identifier.  We recognise reserved words.
      *)

   val lookup_alpha_unreserved:input->token
   val lookup_symbolic_unreserved:input->token
   (* Like lookup_alpha and lookup_symbolic, but don't recognise reserved words *)

   datatype onetwo=ONE of token | TWO of token

   val lookup_underline:input->onetwo 
   (* If this is an MLJ reserved word (preceded by an underline), 
      returns the token for it.  Otherwise it returns the tokens 
      for the identifer, which is
      assumed preceded by a WILD token.
      *)
end

