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
(* Elaboration of top-level structures, signatures and functors         *)
(*======================================================================*)
structure Elab :> ELAB =
struct

type Sig = 
  SMLTy.DatEnv * SMLTy.ExEnv * Env.Sig
type Str = 
  SMLTy.DatEnv * SMLTy.ExEnv * SMLTy.ClassEnv * 
  SMLTy.Realisation * Env.Env*SMLTerm.StrExp

structure T = SMLTerm

local open 
  Syntax ElabOps Env EnvOps SMLTy SMLPrimTy SMLSch SMLSchOps ElabTy ElabPat 
  ElabCore
in

structure Map = Symbol.OrdMap

val tempstrid = Symbol.symbol (JavaString.fromString "$temp")
         
(*----------------------------------------------------------------------*)
(* Structure Expressions (p31 Defn)					*)
(*----------------------------------------------------------------------*)
fun infStrExp (B : Basis) (loc, prestrexp) =
case prestrexp of
  
(* Rule 50 *)
  Struct strdec =>
  let
    val (d, E) = infStrDec B strdec
  in
    (T.StrLet(d, 
    T.Struct (
      Map.mapPartiali (fn (v,ValBind.VarSch _) => SOME v | _ =>NONE) (VEofE E),
      Map.mapi (fn (id, _) => T.Strid (id,[])) (SEofE E))), E)
  end

(* Rule 51 *)
| Strid longstrid =>
  let
    val (E, path) = EnvLookup.lookupStr (EofB B, loc, longstrid)
  in
    (T.Strid path, E)
  end

(* Rule 52 *)
| StrTransparent(strexp, sigexp) =>
  let
    val (e, E) = infStrExp B strexp
    val sigma as (_, sigE) = infSigExp' (BnoPath B) sigexp
    val (psi, exmap) = Match.match1 loc (E, sigma)
    val E' = appRealisationE psi sigE
  in
    addRealisation psi;
    let
      val e' = Match.match2 (tempstrid,loc) (E, E')
    in
      (T.StrLet([T.Structure(tempstrid, e)], e'), appExMapE exmap E')
    end
  end

(* Rule 53 *)
| StrOpaque(strexp, sigexp) =>
  let
    val (e, E) = infStrExp B strexp
    val sigma as (T, E') = infSigExp' (BnoPath B) sigexp
    val (r, T') = makeRenaming (pathofB B, T)
    val _ = appRenamingDE r
    val E' = renameE r E'
    val (psi, exmap) = Match.match1 loc (E, (T',E'))
    val E'' = appRealisationE psi E'
    val _  = if Controls.isOn "showElab"
             then Debug.print ("str:\n" ^ "E = " ^ EnvOps.EtoString E ^ 
               "\nrealised sig:\n" ^
               "E = " ^ EnvOps.EtoString E'' ^ "\n")
             else ()
  in
    addRealisation psi;
    let
      val e' = Match.match2 (tempstrid,loc) (E, E'')
    in
      (T.StrLet([T.Structure(tempstrid, e)], e'), appExMapE exmap E')
    end
  end

(* Rule 54 *)
| FunApp(funid, strexp) =>
  let
    val E = infStrExp B strexp
  in
  case Map.find(FofB B, funid) of
    NONE => 
    (error (Error.error(loc, "unbound functor: " ^ 
      Pretty.idToString funid), []);
    (T.Struct(Map.empty, Map.empty), emptyE))

  | SOME Phi =>    
    Debug.fail "Elab.infStrExp: functor application not implemented yet"
  end

(* Rule 55 *)
| StrLet(strdec, strexp) =>
  let
    val (d, E1) = infStrDec B strdec
    val (e, E2) = infStrExp (BplusE B E1) strexp
  in
    (T.StrLet(d, e), E2)
  end

(*----------------------------------------------------------------------*)
(* Structure-level Declarations (p32 Defn)				*)
(*----------------------------------------------------------------------*)
and infStrDecItem (B : Basis) (strdecitem as (loc, prestrdecitem)) =
case prestrdecitem of

(* Rule 57 *)
  Structure strbind =>
  let
    val (d, SE) = infStrBind B strbind
  in
    (d, SEinE SE)
  end

(* Rule 58 *)
| Local(strdec1, strdec2) =>
  let
    val (d1, E1) = infStrDec B strdec1
    val (d2, E2) = infStrDec (BplusE B E1) strdec2
  in
    ([T.Local(d1, d2)], E2)
  end

(* Rule 58a: Java declarations *)
| JavaDec javadec =>
  ElabJavaDec.elab false (CofB B) (loc, javadec)
  
(* Rule 56 *)
| _ =>
  infDecItem (CofB B) true strdecitem
  
and infStrDec (B : Basis) dec =
case dec of

(* Rule 59 *)
  [] =>
  ([], emptyE)

(* Rule 60 *)
| strdecitem::strdec =>
  let
    val (d1, E1) = infStrDecItem B strdecitem
    val (d2, E2) = infStrDec (BplusE B E1) strdec
  in
    (d1 @ d2, EplusE E1 E2)
  end

(*----------------------------------------------------------------------*)
(* Structure Bindings (p32 Defn)					*)
(*----------------------------------------------------------------------*)
and infStrBind (B : Basis) strbinds =
(* Rule 61 *)
case strbinds of
  [] =>
  ([], Map.empty)

| (strid,strexp as (loc,_),siginfo)::strbind =>
  let
    val strexp' = 
      case siginfo of
        SigNone => strexp
      | SigAbstract sigexp => (loc,StrOpaque(strexp, sigexp))
      | SigConcrete sigexp => (loc,StrTransparent(strexp, sigexp))
    val (e, E) = infStrExp (BplusStr B strid) strexp'
    val (d, SE) = infStrBind B strbind
  in
    (T.Structure(strid, e) :: d, Map.insert(SE, strid, E))
  end

(*----------------------------------------------------------------------*)
(* Signature Expressions (p32-33 Defn)					*)
(*----------------------------------------------------------------------*)
and infSigExp (B : Basis) (loc,presigexp) =
case presigexp of
  
(* Rule 62 *)
  SigSpec spec =>
  infSpec B spec

(* Rule 63: rename bound variables to satisfy side condition *)
| Sigid sigid =>
  (case Map.find(GofB B, sigid) of
    NONE => 
    (error (Error.error(loc, "unbound signature: " ^ 
      Pretty.idToString sigid), []); emptyE)

  | SOME (T,E) =>
    let
      val (r, T') = makeRenaming (pathofB B, T)
    in
      renameE r E
    end)

(* Rule 64 *)
| Where(sigexp, tyvarseq, longtycon, typ) =>
  let
    val E = infSigExp B sigexp
    val ty = infTy (CofB B) typ
  in
    case EnvLookup.lookupTyCon (E, loc, longtycon) of
    NONE => 
    (error (Error.error(loc, 
      "type constructor not bound in signature: " ^ Pretty.longidToString
      longtycon), []); emptyE)

  | SOME tystr =>
    case TyStr.fromAbstract tystr of
      NONE => 
      (error (Error.error(loc, 
      "type constructor not abstract: " ^ Pretty.longidToString longtycon),
      []); emptyE)

    | SOME (tyvars, tyname) =>
      if length tyvars <> length tyvarseq
      then 
        (error (Error.error(loc, "type definition has wrong arity: " 
          ^ Pretty.longidToString longtycon), []); emptyE)
      else
      let 
        val psi = TyName.Map.insert(TyName.Map.empty, tyname, 
          (map TyVar.explicit tyvarseq, ty))
        val E' = appRealisationE psi E
      in
        E'
      end
  end

and infSigExp' (B : Basis) sigexp =

  (* Rule 65 *)
  let
    val stamp = getStamp ()
    val E = infSigExp B sigexp
  in
    (TyName.Set.filter 
    (fn tn => not (TyName.earlier(tn, stamp))) (tynamesE E), E)
  end

(*----------------------------------------------------------------------*)
(* Specifications (p33 Defn)						*)
(*----------------------------------------------------------------------*)
and infSpecItem (B : Basis) ((loc,prespecitem) : SpecItem) = 
case prespecitem of

(* Rule 68 *)
  ValDesc valdesc => 
  let
    val VE = infValDesc loc (CofB B) valdesc
  in
    VEinE (Map.mapi (fn (v,ty) => ValBind.VarSch(polyType ty)) VE)
  end

(* Rule 69 *)
| TypeDesc typdesc =>
  TEinE (infTypeDesc (CofB B) typdesc)

(* Rule 70 *)
| EqTypeDesc typdesc =>
  TEinE (infEqTypeDesc (CofB B) typdesc)

(* Rule 71 *)
| DatatypeDesc (datdesc, typbindopt) => 
  let
    val (VE, TE, r) = infDatBind true (CofB B) (datdesc, typbindopt)
  in
    VETEinE (VE, TE)
  end

(* Rule 72 *)
| DatatypeDescCopy (tycon, longtycon) =>
  infDatCopy (CofB B) loc (tycon, longtycon)

(* Rule 73 *)
| ExceptionDesc exdesc =>
  let
    val EE = infExDesc (CofB B) loc exdesc
  in
    VEinE (Map.map ValBind.ExTy EE)
  end

(* Rule 73a: Java declarations *)
| JavaDesc javadec =>
  #2 (ElabJavaDec.elab true (CofB B) (loc,javadec))

(* Rule 74 *)
| StructureDesc strdesc =>
  SEinE (infStrDesc B strdesc)

(* Rule 75 *)
| Include sigexp =>
  infSigExp B sigexp

(* Rule 78 *)
| Sharing _ =>
(*
  bind (infSpec B spec) (fn E =>
  bind (MOps.map 
    (fn tc => EnvLookup.lookupTyCon (EofB B, loc, tc)) longtycons) (fn ts =>
*)
  Debug.fail "Elab.infSpecItem: sharing not implemented yet"
  
  
and infSpec (B : Basis) (spec : Spec) =
case spec of

(* Rule 76 *)
  [] =>
  emptyE

(* Rule 77 *)
| onespec::spec =>
  let
    val E1 = infSpecItem B onespec
    val E2 = infSpec (BplusE B E1) spec
  in
    EplusE E1 E2
  end
 
(*----------------------------------------------------------------------*)
(* Value Descriptions (p34 Defn)					*)
(* Extended to deal with overloading.                                   *)
(*----------------------------------------------------------------------*)
(* Rule 79 *)
and infValDesc loc C valdesc =
let
  fun infValDesc' [] = 
      Map.empty

    | infValDesc' ((var,tyexp)::valdesc) =
      let
        val ty = infTy C tyexp
        val VE = infValDesc' valdesc
      in
        Map.insert(VE, var, ty)
      end

  val var = #1 (hd valdesc)
in
  if length valdesc > 1
  andalso List.all (fn (var',_) => Symbol.equal(var,var')) valdesc
  then
  let
    val tys = map (infTy C o #2) valdesc
    val SOME (S, ty) = SMLTy.antiunifylist tys
  in
    case TyVar.Map.listItemsi S of
      [(tyvar, basetys)] =>
      let
        val tynameset =
            foldl (fn (basety, tynameset) =>
            let
              val SOME ([], tyname) = SMLTy.fromConsType basety
            in
              TyName.Set.add(tynameset, tyname) 
            end) TyName.Set.empty basetys

        val tyvar' = SMLTy.freshTyVar (TyVar.Overloaded tynameset)
        val ty = SMLTy.appSubst [(tyvar, SMLTy.tyVarType tyvar')] ty
      in
        Map.insert(Map.empty, var, ty)
      end

    | _ =>
      (error (Error.error (loc, 
        "expected just one type variable in overloaded type"), []);
      Map.empty)
  end

  else infValDesc' valdesc
end
  

(*----------------------------------------------------------------------*)
(* Type Descriptions (p35 Defn)						*)
(*----------------------------------------------------------------------*)
(* Rule 80 *)
and infTypeDesc C [] = 
    Map.empty

  | infTypeDesc C ((tyvarseq,tycon,NONE)::typdesc) =
    let
      val TE = infTypeDesc C typdesc
      val tyname = freshTyName (pathofC C @ [tycon], TySort.anySort)
    in
      Map.insert(TE, tycon, 
        TyStr.makeAbstract (map TyVar.explicit tyvarseq, tyname))
    end

  | infTypeDesc C ((tyvarseq,tycon,SOME typ)::typdesc) =
    let
      val ty = infTy C typ
      val TE = infTypeDesc C typdesc
    in
      Map.insert (TE, tycon, 
        TyStr.makeConcrete (map TyVar.explicit tyvarseq, ty))
    end

(*----------------------------------------------------------------------*)
(* Equality type Descriptions (p35 Defn)				*)
(*----------------------------------------------------------------------*)
(* Rule 80 *)
and infEqTypeDesc C [] = 
    Map.empty

  | infEqTypeDesc C ((tyvarseq,tycon)::typdesc) =
    let
       val TE = infEqTypeDesc C typdesc
       val tyname = freshTyName (pathofC C @ [tycon], TySort.eqSort)
    in
      Map.insert(TE, tycon, 
        TyStr.makeAbstract(map TyVar.explicit tyvarseq, tyname))
    end

(* Rule 81/82: we reuse infDatBind *)

(*----------------------------------------------------------------------*)
(* Exception Descriptions (p35 Defn)					*)
(*----------------------------------------------------------------------*)
(* Rule 83 *)
and infExDesc C loc [] = 
    Map.empty

  | infExDesc C loc (((_,excon), tyopt)::exdesc) =
    let
      val EE = infExDesc C loc exdesc
    in
      case tyopt of
        NONE =>
        let
          val exname = freshExName (pathofC C @ [excon], NONE, false)
        in
          Map.insert (EE, excon, (exnType, exname))
        end

      | SOME typ =>
        let
          val ty = infTy C typ
          val exname = freshExName (pathofC C @ [excon], SOME ty, false)
        in
          Map.insert (EE, excon, 
            (SMLTy.funType (ty, exnType), exname))
        end
    end

(*----------------------------------------------------------------------*)
(* Structure Descriptions (p35 Defn)					*)
(*----------------------------------------------------------------------*)
(* Rule 84 *)
and infStrDesc B strdesc =
case strdesc of
  [] =>
  Map.empty

| (strid,sigexp)::strdesc =>
  let 
    val E = infSigExp B sigexp
    val SE = infStrDesc B strdesc
  in
    Map.insert(SE, strid, E)
  end

(*----------------------------------------------------------------------*)
(* Top-level signature							*)
(*----------------------------------------------------------------------*)
fun infTopSigExp (B,CE) (sigid,sigexp) =
let
  val (sigma, errors, DE, EE, _, _) = 
    runelab (Entity.Sig, sigid) CE (fn () => infSigExp' B sigexp)
in
  if Controls.isOn "showElab" (* andalso List.all (not o Error.isSerious) errors *)
  then Debug.print ("results:\n" ^
    "E = " ^ EnvOps.EtoString (#2 sigma) ^ "\n")
  else (); 
  ((DE, EE, sigma), errors)
end

(*----------------------------------------------------------------------*)
(* Top-level structure							*)
(*----------------------------------------------------------------------*)
fun infTopStrExp (B,CE) (strid,strexp as (loc,_),siginfo) =
let
  val strexp' = 
      case siginfo of
        SigNone => strexp
      | SigAbstract sigexp => (loc,StrOpaque(strexp, sigexp))
      | SigConcrete sigexp => (loc,StrTransparent(strexp, sigexp))

  val ((e,E), errors, DE, EE, CE, psi) = 
    runelab (Entity.Str, strid) CE 
    (fn () => infStrExp (BplusStr B strid) strexp')
in
  if Controls.isOn "showElab" (* andalso List.all (not o Error.isSerious) errors *)
  then Debug.print ("results:\n" ^
    "E = " ^ EnvOps.EtoString E ^ "\n" ^
    "DE = " ^ SMLTy.DEtoString DE ^ "\n" ^
    "EE = " ^ SMLTy.EEtoString EE ^ "\n" ^
    "psi = " ^ SMLTy.realisationToString psi ^ "\n")
  else ();
  ((DE, EE, CE, psi, E, e), errors)
end


end (* of local open *)
end (* of struct *)

