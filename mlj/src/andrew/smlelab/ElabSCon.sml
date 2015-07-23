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

structure ElabSCon =
struct

local open SMLTy
in

(*----------------------------------------------------------------------*)
(* Determine the type of a special constant				*)
(*----------------------------------------------------------------------*)
fun typeSCon (SCon.NumCon(_, IntConvFlags.Signed _, _)) = 
    tyVarType (freshTyVar 
      (TyVar.Overloaded (TyName.Set.addList(TyName.Set.empty,
        [TyName.int16TyName, TyName.int8TyName, 
         TyName.intTyName, TyName.int64TyName]))))

  | typeSCon (SCon.NumCon(_, IntConvFlags.Unsigned, _)) = 
    tyVarType (freshTyVar 
      (TyVar.Overloaded (TyName.Set.addList(TyName.Set.empty,
        [TyName.wordTyName, TyName.word8TyName, TyName.word64TyName]))))

  | typeSCon (SCon.StrCon _)  = 
    SMLPrimTy.stringType

  | typeSCon (SCon.RealCon _) = 
    tyVarType (freshTyVar 
      (TyVar.Overloaded (TyName.Set.addList(TyName.Set.empty,
        [TyName.realTyName (* , TyName.real32TyName *)]))))

  | typeSCon (SCon.CharCon _) = 
    SMLPrimTy.charType

end

end