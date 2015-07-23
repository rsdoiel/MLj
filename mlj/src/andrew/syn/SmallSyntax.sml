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
(* Abstract syntax tree datatypes for compressed version of source	*)
(* containing only information relevant to dependency analysis.         *)
(* We still need location information for error messages.               *)
(* Note:                                                                *)
(*   There is no distinction between exps, decs, and strdecs.           *)
(*======================================================================*)
structure SmallSyntax =
struct

(*----------------------------------------------------------------------*)
(* Structure expressions						*)
(*    struct <dec> end                                                  *)
(*    <longstrid>                                                       *)
(*    <strexp> : <sigexp>    |    <strexp> :> <sigexp>                  *)
(*    <funid> ( <strexp> )                                              *)
(*    let <dec> in <strexp> end                                         *)
(*----------------------------------------------------------------------*)
datatype StrExp =
  Struct of Dec
| Strid of Syntax.Location * Syntax.longid
| StrConstraint of StrExp * SigExp
| FunApp of Syntax.Location * Syntax.symbol * StrExp
| StrLet of Dec * StrExp

(*----------------------------------------------------------------------*)
(* Signature expressions						*)
(*    <strid>                                                           *)
(*----------------------------------------------------------------------*)
and SigExp =
  Sigid of Syntax.Location * Syntax.symbol
| SigSpec of Spec
| Where of SigExp * Mention list

(*----------------------------------------------------------------------*)
(* Specification items							*)
(*    structure <strid> : <sigexp> and ... and <strid> : <sigexp>       *)
(*    include <sigexp>                                                  *)
(* or a reference to a <longstrid>.                                     *)
(*----------------------------------------------------------------------*)
and SpecItem =
  StructureDesc of (Syntax.symbol * SigExp) list
| Include of SigExp
| SpecMention of Mention

(*----------------------------------------------------------------------*)
(* Declaration items (also used for expressions and strdecs)		*)
(*    local <dec> in <dec> end                                          *)
(*    open <longstrid> ... <longstrid>                                  *)
(* or a reference to a <longstrid>                                      *)
(*----------------------------------------------------------------------*)
and DecItem =
  Local of Dec * Dec
| Open of Syntax.Location * Syntax.longid list
| Structure of (Syntax.symbol * StrExp) list
| Signature of (Syntax.symbol * SigExp) list
| Mention of Mention
| Class of JavaString.t


withtype Spec = SpecItem list
and Dec = DecItem list
and Mention = Syntax.Location * Syntax.longid (* without final identifier *)


end
