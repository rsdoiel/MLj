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
(* Convert SML source syntax to small syntax				*)
(*======================================================================*)
structure SyntaxConvert :> SYNTAXCONVERT =
struct

open Syntax
structure SS = SmallSyntax

fun mention(loc, longid) = (loc, rev (tl (rev longid)))

(*----------------------------------------------------------------------*)
(* List the structure longids referenced in a type expression		*)
(*----------------------------------------------------------------------*)
fun convertTy ((loc, prety) : Ty) acc =
case prety of
  TyVar _             => acc
| TyCon(tys,[_])      => convertTys tys acc
| TyCon(tys,longid)   => convertTys tys (mention(loc,longid) :: acc)
| TyFun(ty1, ty2)     => convertTy ty1 (convertTy ty2 acc)
| TyRecord tyrow      => convertTys (map #2 tyrow) acc
| TyTuple tys         => convertTys tys acc
| TyClass classid     => acc (* Should really mention this class too *)

and convertTys [] acc = acc
  | convertTys (ty::tys) acc = convertTys tys (convertTy ty acc)

fun convertTyOpts [] acc = acc
  | convertTyOpts (SOME ty::rest) acc = convertTyOpts rest (convertTy ty acc)
  | convertTyOpts (_::rest) acc = convertTyOpts rest acc

and convertTypBind (typbind : TypBind) acc = convertTys (map #3 typbind) acc
and convertDatBind ([] : DatBind) acc = acc
  | convertDatBind ((_,_,conbinds)::datbind) acc =
    convertDatBind datbind (convertTyOpts (map #2 conbinds) acc)
    
(*----------------------------------------------------------------------*)
(* List the structure longids referenced in a pattern expression	*)
(*----------------------------------------------------------------------*)
fun convertPat ((loc, prepat) : Pat) acc =
case prepat of
  (PatWild | PatSCon _ | PatVar (Short _)) => acc
| PatVar(Long longid) => mention(loc,longid) :: acc
| PatVar _ => acc
| PatCon([_], pat)    => convertPat pat acc
| PatCon(longid, pat) => convertPat pat (mention(loc,longid) :: acc)
| PatRecord(_,patrow) => convertPats (map #2 patrow) acc
| PatConstraint(pat,ty) => convertPat pat (convertTy ty acc)
| PatLayer(_,tyopt,pat) => convertPat pat (convertTyOpts [tyopt] acc)
| (PatTuple pats | PatList pats | OrPat pats) => convertPats pats acc

and convertPats [] acc = acc
  | convertPats (pat::pats) acc = convertPats pats (convertPat pat acc)

fun makeLocal (dec1, dec2) =
if List.all (fn SS.Mention _ => true | _ => false) dec1
then dec1 @ dec2
else [SS.Local(dec1, dec2)]

fun convertExp ((loc,preexp) : Exp) =
case preexp of
  (SCon _ | Hash _ | DotHash _ | DotHashHash _) => []
| LongVid(Long longid)=> [SS.Mention (mention(loc,longid))]
| LongVid _           => []
| Fn match            => convertMatch match
| Let(dec, exp)       => makeLocal(convertDec dec, convertExp exp)
| (Constraint(e, ty)
| ConstraintGt(e,ty)) => convertExp e @ (map SS.Mention (convertTy ty []))

| Raise e             => convertExp e
| Record exprow       => List.concat (map (convertExp o #2) exprow)
| Java(_, tyopt, _, exps) => 
  (map SS.Mention (convertTyOpts [tyopt] [])) @
  List.concat (map convertExp exps)
| (Case(exp, match) | Handle(exp, match)) => 
  convertExp exp @ convertMatch match

| If(e1, e2, e3) => 
  convertExp e1 @ convertExp e2 @ convertExp e3

| (Orelse(e1, e2) | Andalso(e1,e2) | While(e1,e2) | App(e1,e2)) => 
  convertExp e1 @ convertExp e2

| (Sequence es | List es | Tuple es) => 
  List.concat (map convertExp es)

| FlatApp _ =>
  Debug.fail "SyntaxConvert.convertExp: FlatApp"

and convertMatch [] = []
  | convertMatch ((pat,exp)::match) =
    map SS.Mention (convertPat pat []) @ convertExp exp @ convertMatch match

and convertFValBind [] = []
  | convertFValBind ((_, _, pats, exp, tyopt)::rest) =
    map SS.Mention (convertPats pats []) @ 
    convertExp exp @ convertFValBind rest  (*tyopt !!!!!!!!!!!! *)

and convertDecItem ((loc,predecitem) : DecItem) =
case predecitem of
  (Val(_, match) | ValRec(_, match)) => 
  convertMatch match

| Fun(_, bindings)    => 
  convertFValBind (List.concat bindings)

| Type typbind        => 
  map SS.Mention (convertTypBind typbind [])

| Datatype(datbind, NONE) => 
  map SS.Mention (convertDatBind datbind [])

| Datatype(datbind, SOME typbind) =>
  map SS.Mention (convertDatBind datbind (convertTypBind typbind []))

| Abstype(datbind, NONE, dec) =>
  map SS.Mention (convertDatBind datbind []) @ convertDec dec

| Abstype(datbind, SOME typbind, dec) =>
  map SS.Mention (convertDatBind datbind (convertTypBind typbind []))
  @ convertDec dec

| DatatypeCopy(_, [id]) =>
  []
  
| DatatypeCopy(_, longid) =>
  [SS.Mention (mention(loc, longid))]

| Exception exbinds =>
  map SS.Mention (List.concat 
    (map (fn (_,e) => convertExBind (loc,e)) exbinds))

| Local(dec1, dec2)   => 
  makeLocal(convertDec dec1, convertDec dec2)

| Open longids        => 
  [SS.Open (loc, longids)]

| JavaDec dec         => 
  convertJavaDec dec

| Structure bindings  => 
  [SS.Structure 
  (map (fn (strid, strexp, siginfo) =>     
    (strid, convertSigInfo (siginfo, convertStrExp strexp))) bindings)]

| Signature bindings  => 
  [SS.Signature (map (fn (sigid, sigexp) =>     
    (sigid, convertSigExp sigexp)) bindings)]

| (Infix _ | Infixr _ | Nonfix _) =>
  []

and convertDec decs = List.concat (map convertDecItem decs)

and convertSigInfo ((SigConcrete sigexp | SigAbstract sigexp), strexp) =
    SS.StrConstraint(strexp, convertSigExp sigexp)
  | convertSigInfo (SigNone, strexp) = strexp


and convertExBind (loc, ExDesc (SOME ty)) = convertTy ty []
  | convertExBind (loc, ExBind (Short _)) = []
  | convertExBind (loc, ExBind (Long longid)) = [mention (loc, longid)]
  | convertExBind _ = []

and convertJavaDec (ClassException(_,class)) = [SS.Class class]
  | convertJavaDec (ClassType { super, implements, body, ... }) =
    List.concat (map convertClassItem body) @
    map SS.Mention (convertTyOpts [super] (convertTys implements []))
    
and convertClassItem ((loc, preclassitem) : ClassItem) = 
case preclassitem of
  Field { ty, initial, ... } =>
  map SS.Mention (convertTy ty []) @ 
  (case initial of 
    NONE => []
  | SOME exp => convertExp exp)

| Method { args, result, body, ... } =>
  map SS.Mention (convertTyOpts [result] (convertTys (map #2 args) [])) @
  (case body of
    NONE => []
  | SOME exp => convertExp exp)

| Constructor { args, body, inits, ... } =>
  map SS.Mention (convertTyOpts [] (convertTys (map #2 args) [])) @
  (case body of
    NONE => []
  | SOME exp => convertExp exp) @ convertInits inits

and convertInits (SuperInvoc(exps,fldinits)) = 
    List.concat (map convertExp exps) @
    List.concat (map (convertExp o #2) fldinits)
  | convertInits (ThisInvoc exps) = List.concat (map convertExp exps)

and convertJavaDesc (ClassException _) = []
  | convertJavaDesc (ClassType { super, implements, body, ... }) =
    List.concat (map convertClassItemDesc body) @
    map SS.SpecMention (convertTyOpts [super] (convertTys implements []))
    
and convertClassItemDesc ((loc, preclassitem) : ClassItem) = 
case preclassitem of
  Field { ty, initial, ... } =>
  map SS.SpecMention (convertTy ty [])

| Method { args, result, ... } =>
  map SS.SpecMention (convertTyOpts [result] (convertTys (map #2 args) []))

| Constructor { args, ... } =>
  map SS.SpecMention (convertTyOpts [] (convertTys (map #2 args) []))

and convertStrExp ((loc,prestrexp) : StrExp) = 
case prestrexp of
  Struct dec          => SS.Struct (convertDec dec)
| Strid longid        => SS.Strid (loc, longid)
| StrTransparent(strexp, sigexp) => 
  SS.StrConstraint(convertStrExp strexp, convertSigExp sigexp)
| StrOpaque(strexp, sigexp) => 
  SS.StrConstraint(convertStrExp strexp, convertSigExp sigexp)
| FunApp(id, strexp)  => SS.FunApp(loc, id, convertStrExp strexp)
| StrLet(dec, strexp) => SS.StrLet(convertDec dec, convertStrExp strexp)

and convertSigExp ((loc,presigexp) : SigExp) =
case presigexp of
  SigSpec spec     => SS.SigSpec(convertSpec spec)
| Sigid id         => SS.Sigid(loc, id)
| Where(sigexp, tyvars, longtycon, ty) =>
  SS.Where(convertSigExp sigexp, convertTy ty [])

and convertSpecItem ((loc,prespecitem) : SpecItem) =
case prespecitem of 
  ValDesc valdesc     => map SS.SpecMention (convertTys (map #2 valdesc) [])
| TypeDesc typdesc    => map SS.SpecMention (convertTyOpts (map #3 typdesc) [])
| EqTypeDesc _        => []
| DatatypeDesc(datbind, NONE) => 
  map SS.SpecMention (convertDatBind datbind [])

| DatatypeDesc(datbind, SOME typbind) =>
  map SS.SpecMention (convertDatBind datbind (convertTypBind typbind []))

| DatatypeDescCopy(_, [id]) =>
  []

| DatatypeDescCopy(_, longid) =>
  [SS.SpecMention (mention(loc, longid))]

| ExceptionDesc conbinds => 
  map SS.SpecMention (convertTyOpts (map #2 conbinds) [])

| StructureDesc bindings => 
  [SS.StructureDesc 
  (map (fn (strid, sigexp) => (strid,convertSigExp sigexp)) bindings)]

| Include sigexp =>
  [SS.Include (convertSigExp sigexp)]

| JavaDesc jd =>  
  convertJavaDesc jd

| (Sharing longids | SharingType longids) =>
  List.mapPartial 
  (fn [id] => NONE | longid => SOME (SS.SpecMention (mention (loc,longid)))) 
  longids

and convertSpec spec = List.concat (map convertSpecItem spec)

val convert = convertDec

end
