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

signature ELABOPS =
sig

  val abs : Syntax.symbol list -> Syntax.Exp -> Syntax.Exp

  val caseTerm : SMLTerm.Exp * SMLTy.Type * SMLTerm.Match -> SMLTerm.Exp
  val condTerm : SMLTerm.Exp * (SMLTerm.Exp * Syntax.Location)
                   * (SMLTerm.Exp * Syntax.Location) * SMLTy.Type
                   -> SMLTerm.Exp

  val expTuple : Syntax.Location -> Syntax.Exp list -> Syntax.Exp
  val makeWhileTerm : Syntax.Location * SMLTerm.Exp * SMLTerm.Exp
                        -> SMLTerm.Exp 
  val monovar : Syntax.symbol -> SMLTerm.Exp
  val patList : Syntax.Location -> Syntax.Pat list -> Syntax.Pat
  val patTuple : Syntax.Location -> Syntax.Pat list -> Syntax.Pat
  val patmerge : Syntax.Location
                   -> 'a Symbol.OrdMap.map * 'a Symbol.OrdMap.map -> 
                      'a Symbol.OrdMap.map 
  val tupleTerm : SMLTerm.Exp list -> SMLTerm.Exp

  val makeOpen : Syntax.Location * Env.Env * SMLTerm.longid -> SMLTerm.Dec 

  val vidToLongid : Syntax.OpLongVid -> Syntax.longid

end

