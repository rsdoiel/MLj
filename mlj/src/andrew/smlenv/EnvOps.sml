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
(* Operations on environments used in type inference			*)
(*======================================================================*)
structure EnvOps :> ENVOPS =
struct

local open SMLTy Env ValBind in

structure Map = Symbol.OrdMap

val symbolToString = JavaString.toMLString o Symbol.toJavaString

fun freeVE VE = 
  Map.foldr (fn (b, tyvars) => 
    TyVar.Set.union (ValBindOps.tyvars b, tyvars)) TyVar.Set.empty VE

fun merge (m1, m2) = 
  Map.foldri (fn (x,v,m) => Map.insert (m,x,v)) m1 m2

fun renameVE r = Map.map (ValBindOps.rename r)

fun renameTE r = Map.map (TyStr.rename r)

fun renameE r (Env (SE,TE,VE)) =
 Env (Map.map (renameE r) SE, renameTE r TE, renameVE r VE)

fun appRealisationVE psi = Map.map (ValBindOps.appRealisation psi)

fun appRealisationTE psi = Map.map (TyStr.appRealisation psi)

fun appRealisationE psi (Env (SE,TE,VE)) =
 Env (Map.map (appRealisationE psi) SE, appRealisationTE psi TE, 
   appRealisationVE psi VE)

fun appExMapVE exmap = Map.map (ValBindOps.appExMap exmap)

fun appExMapE exmap (Env (SE,TE,VE)) =
 Env (Map.map (appExMapE exmap) SE, TE, appExMapVE exmap VE)

(*----------------------------------------------------------------------*)
(* Type names in environments						*)
(*----------------------------------------------------------------------*)
fun tynamesVE VE = 
  Map.foldr (fn (vb,T) => TyName.Set.union(ValBindOps.tynames vb, T)) 
  TyName.Set.empty VE
fun tynamesTE TE =
  Map.foldr (fn (tystr,T) => TyName.Set.union(TyStr.tynames tystr, T))
  TyName.Set.empty TE
fun tynamesSE SE =
  Map.foldr (fn (E,T) => TyName.Set.union(tynamesE E, T))
  TyName.Set.empty SE
and tynamesE (Env(SE,TE,VE)) = 
  TyName.Set.union(tynamesSE SE, TyName.Set.union(tynamesTE TE, tynamesVE VE))

(*----------------------------------------------------------------------*)
(* Selection of environment components					*)
(*----------------------------------------------------------------------*)
fun VEofE (Env(SE,TE,VE)) = VE
fun TEofE (Env(SE,TE,VE)) = TE
fun SEofE (Env(SE,TE,VE)) = SE
fun GofB (T,F,G,E,path) = G
fun EofB (T,F,G,E,path) = E
fun TofB (T,F,G,E,path) = T
fun FofB (T,F,G,E,path) = F
fun pathofB (T,F,G,E,path) = path
fun pathofC (T,U,E,lam,path,class) = path
fun lamofC (T,U,E,lam,path,class) = lam
fun classofC (T,U,E,lam,path,class) = class
fun CofB B = (TofB B, TyVar.Set.empty, EofB B, false, pathofB B, NONE)
fun EofC (T,U,E,lam,path,class) = E
fun UofC (T,U,E,lam,path,class) = U

(*----------------------------------------------------------------------*)
(* Extension of environment components					*)
(*----------------------------------------------------------------------*)
fun EplusSE (Env(SE,TE,VE)) SE' = Env(merge(SE,SE'),TE,VE)
fun EplusTE (Env(SE,TE,VE)) TE' = Env(SE,merge(TE,TE'),VE)
fun EplusVE (Env(SE,TE,VE)) VE' = Env(SE,TE,merge(VE,VE'))
fun VEplusVE VE1 VE2 = merge (VE1,VE2)
fun TEplusTE TE1 TE2 = merge (TE1,TE2)
fun SEplusSE SE1 SE2 = merge (SE1,SE2)
fun EplusE (Env(SE1,TE1,VE1)) (Env(SE2,TE2,VE2)) =
  Env(merge(SE1,SE2), merge(TE1,TE2), merge(VE1,VE2))
fun BplusE (T,F,G,E,path) E' = (T,F,G,EplusE E E',path)
fun BplusG (T,F,G,E,path) G' = (T,F,merge(G,G'),E,path)
fun CplusE (T,U,E,lam,path,class) E' = (T,U,EplusE E E',lam,path,class)
fun CplusVE (T,U,E,lam,path,class) VE = (T,U,EplusVE E VE,lam,path,class)
fun CplusTE (T,U,E,lam,path,class) TE = (T,U,EplusTE E TE,lam,path,class)
fun CplusU (T,U,E,lam,path,class) U' = 
  (T,TyVar.Set.union(U,U'),E,lam,path,class)
fun lamC (T,U,E,lam,path,class) = (T,U,E,true,path,class)
fun CwithClass (T,U,E,lam,path,_) class = (T,U,E,lam,path,SOME class)
fun BplusStr (T,F,G,E,path) strid = (T,F,G,E,path @ [strid])
fun BnoPath (T,F,G,E,path) = (T,F,G,E,[])

(*----------------------------------------------------------------------*)
(* Inclusion of environment components					*)
(*----------------------------------------------------------------------*)
fun SEinE SE = Env(SE, Map.empty, Map.empty)
fun TEinE TE = Env(Map.empty, TE, Map.empty)
fun VEinE VE = Env(Map.empty, Map.empty, VE)
fun VETEinE (VE,TE) = Env(Map.empty, TE, VE)
fun EinB E = (TyName.Set.empty, Map.empty, Map.empty, E, [])

(*----------------------------------------------------------------------*)
(* Empty environments							*)
(*----------------------------------------------------------------------*)
val emptyE = Env(Map.empty, Map.empty, Map.empty)
val emptyB = (TyName.Set.empty, Map.empty, Map.empty, emptyE, [])

(*----------------------------------------------------------------------*)
(* Pretty-printing							*)
(*----------------------------------------------------------------------*)
fun VEtoString depth VE =
  Pretty.bigVec depth 
  (fn (id, vbind) => symbolToString id ^ " : " ^ ValBindOps.toString vbind)
  (Map.listItemsi VE)

and TEtoString depth TE =
  Pretty.bigVec depth
  (fn (id, tystr) => symbolToString id ^ " = " ^ TyStr.toString tystr)
  (Map.listItemsi TE)

and SEtoString depth SE =
  Pretty.bigVec depth
  (fn (id, E) => symbolToString id ^ " = " ^ EtoString (depth+1) E)
  (Map.listItemsi SE)

and EtoString depth (Env(SE,TE,VE)) =
  Pretty.newline depth ^ "(" ^ Pretty.newline (depth+1) ^
  "SE = " ^ SEtoString (depth+1) SE ^ "," ^ Pretty.newline (depth+1) ^
  "TE = " ^ TEtoString (depth+1) TE ^ "," ^ Pretty.newline (depth+1) ^
  "VE = " ^ VEtoString (depth+1) VE ^ ")" ^ Pretty.newline (depth)

val EtoString = EtoString 0

fun VEasVals depth VE =
  String.concat (map
  (fn (id, vbind) => "val " ^ 
    symbolToString id ^ " : " ^ ValBindOps.toString vbind
    ^ Pretty.newline depth) (Map.listItemsi VE))

and TEasTypes depth TE =
  String.concat (map 
  (fn (id, tystr) => 
  case TyStr.fromDatatype tystr of
    SOME (_, (tyvars, tyname, contys)) =>
    "datatype " ^ SMLTy.tyvarsToString tyvars ^ symbolToString id ^ " = " ^ 
    Pretty.newline depth ^ "  " ^ Pretty.simpleVec "| "
    (fn (id, tyopt) => symbolToString id ^ 
      (case tyopt of NONE => "" | SOME ty =>
      " of " ^ SMLTy.toString ty) ^ Pretty.newline depth) 
      (Map.listItemsi contys)

  | NONE =>
    case TyStr.fromAbstract tystr of
      SOME (tyvars, tyname) =>
      (if #eq (TyName.sort tyname)
      then "eqtype " else "type ") ^ 
      SMLTy.tyvarsToString tyvars ^ symbolToString id ^ Pretty.newline depth

    | NONE =>
      case TyStr.fromConcrete tystr of
        SOME (tyvars, ty) =>
        "type " ^ SMLTy.tyvarsToString tyvars ^ symbolToString id 
        ^ " = " ^ SMLTy.toString ty
        ^ Pretty.newline depth

      | NONE =>
        case TyStr.fromClassType tystr of
          SOME (tyname, classdef) =>
          classDefToString depth (SMLTy.consType([], tyname), classdef)

        | NONE => "")
  (Map.listItemsi TE))

and SEasSig depth SE =
  String.concat (map 
  (fn (id, E) => "structure " ^ symbolToString id ^ " : " ^ EasSig (depth+1) E
    ^ Pretty.newline depth) (Map.listItemsi SE))

and classDefToString depth (classty, 
  ((flags,super,ints), fields, methods) : SMLTy.ClassDef) =
let
  val (intflag,flags) = List.partition (fn x => x=Class.INTERFACE) flags
in
  (String.concat (map (fn f => JavaOps.classModToString f ^ " ") flags)) ^
  (if null intflag then "_classtype " else "_interfacetype ") ^ 
  SMLTy.toString classty ^
  (case super of NONE => "" | 
                 SOME classty => " _extends " ^ SMLTy.toString classty) ^
  (if null ints then "" 
   else (if null intflag then " _implements " else " _extends ") ^ 
     Pretty.simpleVec "," SMLTy.toString ints) ^
  Pretty.newline depth ^ "{" ^ 
  String.concat (map 
    (fn (id,flags,ty,_) => Pretty.newline (depth+1) ^
    (if null flags then "" 
     else Pretty.simpleVec " " JavaOps.fieldModToString flags ^ " ") ^
    "_field \"" ^ valOf (JavaString.toString id) ^ "\" : " ^ 
    SMLTy.toString ty) fields) ^ 
  String.concat (map 
    (fn (id,flags,tys,tyopt) => Pretty.newline (depth+1) ^ 
    (String.concat (map (fn Method.NATIVE => ""
                          | m => JavaOps.methodModToString m ^ " ") flags)) ^
    (if JavaString.toString id = SOME "<init>" 
     then "_constructor " else "_method \"" ^ 
    valOf (JavaString.toString id) ^ "\" ") ^
    "(" ^ Pretty.simpleVec "," SMLTy.toString tys ^ ")" ^
    (case tyopt of NONE => "" | SOME ty => " : " ^ SMLTy.toString ty))
    methods) ^
  Pretty.newline depth ^ "}" ^ Pretty.newline depth
end

and EasSig depth (Env(SE,TE,VE)) =
  Pretty.newline depth ^ "sig" ^ Pretty.newline (depth+1) ^
  SEasSig (depth+1) SE ^ 
  TEasTypes (depth+1) TE ^
  VEasVals (depth+1) VE ^ Pretty.newline (depth) ^ "end\n"

val EasSig = EasSig 0
val classDefToString = classDefToString 0

(*----------------------------------------------------------------------*)
(* Equality of environments						*)
(*----------------------------------------------------------------------*)
fun eqVE (VE1, VE2) = 
  Eq.list 
  (fn ((id1,vb1), (id2,vb2)) => Symbol.equal(id1,id2) 
    andalso ValBindOps.eq(vb1,vb2))
  (Map.listItemsi VE1, Map.listItemsi VE2)

and eqTE (TE1, TE2) =
  Eq.list 
  (fn ((id1,tstr1), (id2,tstr2)) => Symbol.equal(id1,id2) 
    andalso TyStr.eq(tstr1,tstr2))
  (Map.listItemsi TE1, Map.listItemsi TE2)

and eqSE (SE1, SE2) =
  Eq.list
  (fn ((id1,E1), (id2,E2)) => Symbol.equal(id1,id2) 
    andalso eq(E1,E2))
  (Map.listItemsi SE1, Map.listItemsi SE2)

and eq (Env(SE1,TE1,VE1), Env(SE2,TE2,VE2)) =
  eqSE (SE1,SE2) andalso eqTE (TE1,TE2) andalso eqVE (VE1,VE2)

end

end
