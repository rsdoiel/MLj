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

structure CommonVal :> COMMONVAL =
struct

local open MILTerm in

fun tag (Var _) = 0
  | tag (SCon _) = 2
  | tag (Inj _) = 3
  | tag (Coerce _) = 8
  | tag (ExCon _) = 9
  | tag (Tuple _) = 10
  | tag (Proj _) = 11
  | tag (TApp _) = 14
  | tag (TAbs _) = 15
  | tag (Fold _) = 16
  | tag (Unfold _) = 17
  | tag (Null _) = 18

structure Map = MapFn(
  struct
    type ord_key = MILTerm.Val
    fun compare (v1, v2) =
    case Int.compare(tag v1, tag v2) of
      EQUAL =>      
      (case (v1, v2) of
        (Var i1, Var i2) => Int.compare(i1,i2)

      | (SCon(ty1,c1), SCon(ty2,c2)) =>
        (case MILTy.Map.Key.compare (ty1, ty2) of
          EQUAL => JavaOps.compareConsts (c1,c2)
        | other => other)

      | ((Coerce(v1, ty1), Coerce(v2, ty2)) 
      | (Fold(v1,ty1), Fold(v2,ty2))) =>
        (case compare (v1, v2) of
          EQUAL => MILTy.Map.Key.compare(ty1, ty2)
        | other => other)
 
      | (Inj(ty1, i1, vs1), Inj(ty2, i2, vs2)) =>
        (case Int.compare (i1, i2) of
          EQUAL => 
          (case Compare.list compare (vs1, vs2) of
            EQUAL => MILTy.Map.Key.compare(ty1, ty2)
          | other => other)
        | other => other)
 
      | (ExCon(ty1,vs1), ExCon(ty2,vs2)) =>
        (case MILTy.Map.Key.compare (ty1,ty2) of
          EQUAL => Compare.list compare (vs1,vs2)
        | other => other)

      | (Tuple vs1, Tuple vs2) =>
        Compare.list compare (vs1,vs2)

      | (Proj(i1, v1), Proj(i2, v2)) =>
        (case compare (v1, v2) of
          EQUAL => Int.compare(i1,i2)
        | other => other)
      
      | (TApp(v1, tys1), TApp(v2, tys2)) =>
        (case compare (v1,v2) of
          EQUAL => Compare.list MILTy.Map.Key.compare (tys1, tys2)
        | other => other)

      | (TAbs(tyvars1, v1), TAbs(tyvars2, v2)) =>
        (case compare (v1,v2) of
          EQUAL => Compare.list Int.compare (map #1 tyvars1, map #1 tyvars2)
        | other => other)

      | (Unfold v1, Unfold v2) =>
        compare (v1,v2)

      | (Null ty1, Null ty2) =>
        MILTy.Map.Key.compare (ty1, ty2)

      | _ => 
        MILPretty.failVal v1 "SimplifyEnv.ValMap.compare: illegal value")

    | other => other
  end)

(*----------------------------------------------------------------------*)
(* Important: anything with a null inside cannot be eliminated.		*)
(*----------------------------------------------------------------------*)
fun isMappable (ExCon(_,vs) | Tuple vs | Inj(_,_,vs)) = 
    List.all isMappable vs

  | isMappable (Proj(_,v) | Coerce(v,_) | Fold(v,_) | 
      Unfold v | TApp(v,_) | TAbs(_,v)) = 
    isMappable v

  | isMappable (Var _ | SCon _) = true

  | isMappable (Null _ | Closure _) = false

end

end

