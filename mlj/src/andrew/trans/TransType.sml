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
(* Translation of SML types and environments to MIL.			*)
(*======================================================================*)
structure TransType :> TRANSTYPE =
struct

(*----------------------------------------------------------------------*)
(* Translation of nullary type names					*)
(*----------------------------------------------------------------------*)
val basetys =
[
  (TyName.charTyName,   MILTy.java Types.CHAR),
  (TyName.intTyName,    MILTy.java Types.INT),
  (TyName.int64TyName,  MILTy.java Types.LONG),
  (TyName.intinfTyName, MILTy.java (Types.CLASS ClassHandle.biginteger)),
  (TyName.realTyName,   MILTy.java Types.DOUBLE),
  (TyName.real32TyName, MILTy.java Types.FLOAT),
  (TyName.stringTyName, MILTy.java (Types.CLASS ClassHandle.string)),
  (TyName.word8TyName,  MILTy.java Types.BYTE),
  (TyName.wordTyName,   MILTy.java Types.INT),
  (TyName.word64TyName, MILTy.java Types.LONG),
  (TyName.exnTyName,    MILTys.topExn),
  (TyName.javaByte,     MILTy.java Types.BYTE),
  (TyName.javaShort,    MILTy.java Types.SHORT)
]

(*----------------------------------------------------------------------*)
(* Translate a type name application					*)
(*----------------------------------------------------------------------*)
fun transCon TNE (tyname, tys) =
  let
    val ty = getOpt(TyName.Map.find(TNE, tyname), MILTy.tyname tyname)
  in
    MILTy.app (ty, tys)
  end

(*----------------------------------------------------------------------*)
(* Translate an SML type into a MIL value type				*)
(* TVE maps SML type variables to MIL type variables.                   *)
(*----------------------------------------------------------------------*)
fun transType 
  (TVE : MILTy.Type TyVar.Map.map) 
  (TNE : MILTy.Type TyName.Map.map) 
  ty =
  SMLTy.trans 
  (
(*......................................................................*)
(* If the type variable is not in the environment (i.e. was not bound   *)
(* by a let or letrec) then it must be `internal' and therefore can be  *)
(* instantiated to anything allowed by its sort.                        *)
(*......................................................................*)
    fn tyvar =>
      case TyVar.Map.find(TVE, tyvar) of
        NONE => 
        (case TyVar.sort tyvar of
          TyVar.Normal s => 
          MILTys.unit

        | TyVar.Overloaded tynames => 
          transType TVE TNE (SMLTy.baseType (TyName.default tynames))
        )

      | SOME ty => 
        ty,

(*......................................................................*)
(* ML base type names are translated explicitly to MIL base types.	*)
(* Special case for intinf -- it's a datatype for JDK1.0.2.             *)
(*......................................................................*)
    fn (tyname, []) => 
      (case TyName.fromExternalClass tyname of
        SOME longid => 
        MILTy.java (Types.CLASS (ClassHandle.unknown 
          (SMLClassDefOps.longidToClass longid)))
      | NONE =>
        if not (!Version.hasBigIntegers)
        andalso TyName.eq(tyname, TyName.intinfTyName) 
        then transCon TNE (tyname, [])
        else
          (case List.find (fn (t,_) =>TyName.eq(tyname,t)) basetys of
            SOME (_, prety) => prety
          | NONE => transCon TNE (tyname, []))
      )

     | (tyname, tys) => 
       if TyName.eq(tyname, TyName.vectorTyName)
       then MILTy.vector (hd tys)
       else transCon TNE (tyname, tys),

(*......................................................................*)
(* Record types are translated to products, thus losing the distinction	*)
(* between record types with the same component types but different     *)
(* field labels (ordered alphabetically).                               *)
(*......................................................................*)
    fn pairs => MILTy.prod (map #2 pairs),

(*......................................................................*)
(* Function types are translated to one-argument-one-result function 	*)
(* types in MIL, with conservative effect info.                         *)
(*......................................................................*)
    fn (ty1,ty2) => MILTy.arrow([ty1], MILTy.cmp(Effect.any, [ty2])),

(*......................................................................*)
(* Reference types translate straight.					*)
(*......................................................................*)
    fn ty => MILTy.refty ty,

(*......................................................................*)
(* Array types translate straight.                                      *)
(*......................................................................*)
    fn ty => MILTy.array ty
    
  )

  ty

(*----------------------------------------------------------------------*)
(* Translate a closed SML type scheme					*)
(*----------------------------------------------------------------------*)
fun transOverloaded TNE ([tyvar],ty) = 
  (case TyVar.sort tyvar of
    TyVar.Overloaded tynames =>
    SOME (MILTy.prod (map (fn tyname => 
          transType TyVar.Map.empty TNE
          (SMLTy.appSubst [(tyvar, SMLTy.consType([], tyname))] ty))
          (TyName.Set.listItems tynames)))
  | _ => NONE)

  | transOverloaded TNE _ = NONE

fun transScheme TNE (SMLSch.TypeScheme(sch as (tyvars, ty))) =
case transOverloaded TNE sch of
  SOME ty => ty
| NONE =>
  let
    val (TVE, kinds) = TransOps.freshDebTyVars 0 tyvars
  in
    MILTy.debforall (kinds, transType TVE TNE ty)
  end

(*----------------------------------------------------------------------*)
(* Translate a signature realisation into a map from tynames to types   *)
(*----------------------------------------------------------------------*)
fun transRealisation TNE psi =
  TyName.Map.map (fn (tyvars,ty) =>
    let
      val (TVE, kinds) = TransOps.freshDebTyVars 0 tyvars
      val ty' = transType TVE TNE ty
    in
      if null kinds
      then ty'
      else MILTy.abs(kinds, ty')
    end) 
  psi

  (* Translate a constructor environment as a sum type *)
  fun transCE TVE TNE CE =
  case Symbol.OrdMap.listItems CE of
    [SOME ty] => 
    transType TVE TNE ty

  | [NONE] =>
    MILTy.prod []

  | CElist =>
    MILTy.sum (map (fn NONE => [] | SOME ty => [transType TVE TNE ty]) CElist)

(*----------------------------------------------------------------------*)
(* Translate a set of datatype definitions into a tyname-to-type map.	*)
(* Assumption: datatypes are *regular*.                                 *)
(*----------------------------------------------------------------------*)
fun transDE TNE DE =
let

  (* Translate a single recursive or non-recursive datatype decl *)
  (* Add to the type name environment supplied *)
  fun transDef ((isrec, defs as ((tyvars,tyname,CE)::_)), (TNE,resultTNE)) =
  if not isrec
  then
  let
    val (TVE,kinds) = TransOps.freshDebTyVars 0 tyvars
    val ty = MILTy.abs(kinds, transCE TVE TNE CE)
  in
    (TyName.Map.insert(TNE, tyname, ty), 
     TyName.Map.insert(resultTNE, tyname, ty))
  end
 
  else
  let
    val numdefs = length defs
    val arity = length tyvars

    (* Create type variables for the parameters to the type constructor *)
    val (_,kinds) = TransOps.freshDebTyVars numdefs tyvars

    (* Map recursive occurrences onto MIL type variables *)
    val (_,TNE') = 
      foldl (fn ((tyvars,tyname,_), (i,TNE)) =>
      let
        val K = 
          case TyName.sort tyname of
            { eq = true, ... } => MILTy.Eq
          | _ => MILTy.Any
      in
        (i+1, TyName.Map.insert(TNE, tyname, MILTy.abs(kinds, MILTy.deb i)))
      end) (arity, TNE) defs
      
    (* Construct the recursive definitions *)
    val mildefs = 
      map (fn (tyvars, tyname, CE) => 
      let
        val (TVE,_) = TransOps.freshDebTyVars numdefs tyvars
      in
        transCE TVE TNE' CE
      end) defs

  in
    (* Enumerate the projections from the fixed point of the definitions *)
    Gen.foldli (fn (i, (tyvars, tyname, CE), (TNE,resultTNE)) => 
      let
        val ty = MILTy.abs(kinds, MILTy.mu(i,mildefs))
      in
        (TyName.Map.insert(TNE, tyname, ty), 
         TyName.Map.insert(resultTNE, tyname, ty)) 
      end) (TNE,resultTNE) defs
  end
in
  #2 (foldl transDef (TNE,TyName.Map.empty) DE)
end

(*----------------------------------------------------------------------*)
(* Translate the environment for a top-level structure into its   	*)
(* corresponding MIL tuple type.                                        *)
(*----------------------------------------------------------------------*)
fun transE TNE E =
let
  val SE = EnvOps.SEofE E
  val VE = EnvOps.VEofE E
  fun transValBind (ValBind.VarSch sch) = SOME (transScheme TNE sch)
    | transValBind _ = NONE
in
  MILTy.prod
  (Symbol.OrdMap.listItems (Symbol.OrdMap.map (transE TNE) SE) @
  Symbol.OrdMap.listItems (Symbol.OrdMap.mapPartial transValBind VE))
end

end (* of struct *)
