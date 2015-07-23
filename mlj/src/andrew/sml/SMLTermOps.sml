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

structure SMLTermOps :> SMLTERMOPS =
struct

local 
  open SMLTerm Pretty
in

structure S = Symbol.OrdSet
structure M = Symbol.OrdMap

(*----------------------------------------------------------------------*)
(* Pretty print various lists						*)
(*   Types are written between braces      				*)
(*   Tuple values are written between round brackets			*)
(*   Multiple arguments are written between angle brackets		*)
(*----------------------------------------------------------------------*)
fun tyvec f = vec ("", " {", "}", " {", "}", ",") f
fun argvec f = vec (" <>", " ", "", " <", ">", ",") f
fun bindvec f = vec ("", "Fn ", " => ", "Fn <", "> => ", ",") f
fun conargvec f = vec ("", " ", "", "<", ">", ",") f
fun tuplevec f = vec ("()", "", "", "(", ")", ",") f

fun pathToString [] = ""
  | pathToString ((id,i)::rest) = 
    "." ^ Pretty.idToString id ^ 
    (if Controls.isOn "showPaths" then "[" ^ Int.toString i ^ "]" else "") ^
    pathToString rest

fun displayDecItem depth dec =
case dec of
  Val(loc, tyvars, ty1, pat, e) =>
  "val " ^ displayPat pat ^ " = " ^ 
  bindvec TyVar.toString tyvars ^ display (depth+1) e

| ValRec(tyvars, defs) => 
    "val " ^ bindvec TyVar.toString tyvars ^ 
    "rec" ^ newline (depth+1) ^
    vec("", "", "", "", "", " and" ^ newline (depth+1))
    (displayRecBind (depth+1)) (NList.toList defs)

| Exception exname =>
  "exception " ^ SMLTy.exNameToString exname

| Structure(strid, strexp) =>
  "structure " ^ Pretty.idToString strid ^ " = " ^ displayStrExp depth strexp

| ClassType (classname, classinfo, fields, methods) =>
  "_classtype"

| Local(dec1, dec2) =>
  "local" ^ newline (depth+1) ^ displayDec (depth+1) dec1 ^ newline depth ^ 
  "in" ^ newline (depth+1) ^ displayDec (depth+1) dec2 ^ newline depth ^ "end"

and displayDec depth [] = ""
  | displayDec depth (decitem::dec) = 
    displayDecItem depth decitem ^ newline depth ^ displayDec depth dec

and display depth e =
let
  val nl = newline depth

  fun displayLet (dec, e) =
    newline (depth+1) ^ displayDec (depth+1) dec ^
    (case e of
      Let(dec, e) =>
      displayLet (dec, e)

    | _ =>
      nl ^ "in" ^ newline (depth+1) ^ display (depth+1) e ^ nl ^ "end")

in
    
  case e of
    SCon (scon, ty, loc) => 
    SCon.toString scon

  | JCon _ =>
    "<jcon>"

  | Var ((id,rest), tys) => 
    Pretty.idToString id ^ pathToString rest ^ tyvec SMLTy.toString tys

  | OverloadedVar ((id,rest), tynameset, tys) => 
    Pretty.idToString id ^ pathToString rest ^ "@" ^ 
      tyvec TyName.toString (TyName.Set.listItems tynameset) ^
      tyvec SMLTy.toString tys

  | Con(con, (_,(_,tyname,_)), tyargs) => 
    Pretty.idToString con ^ "{" ^ 
      SMLTy.toString (SMLTy.consType(tyargs,tyname)) ^ "}"

  | ExCon(exname,tyopt) =>
    SMLTy.exNameToString exname

  | App(e1, e2) =>
    display depth e1 ^ " " ^ display depth e2

  | Java((joptype, tyopt, idopt), es, tyopt', effect) => 
    JavaOps.opTypeToString joptype ^ 
    vec("()", "(", ")", "(", ")", ",") (display depth) es

  | Fn (ty, match) => 
    "(fn {" ^ SMLTy.toString ty ^ "}" ^ displayMatch depth match ^ ")"

  | Let(dec,e) =>
    nl ^ "let" ^ displayLet(dec,e)

  | Raise(e, ty, loc) =>
    "raise " ^ display depth e ^ " : " ^ SMLTy.toString ty

  | Record exprow =>
    vec("()", "{", "}", "{", "}", ",")
      (fn (lab,exp) => Pretty.idToString lab ^ "=" ^ display depth exp) exprow

  | Handle(exp, match) =>
    "(" ^ display depth exp ^ " handle " ^ displayMatch depth match ^ ")"

end

and displayStrExp depth strexp =
let
  val nl = newline depth

  fun displayLet (dec,strexp) =
    newline (depth+1) ^ displayDec (depth+1) dec ^
    (case strexp of
      StrLet(dec, strexp) =>
      displayLet (dec, strexp)

    | _ =>
      nl ^ "in" ^ 
      newline (depth+1) ^ displayStrExp (depth+1) strexp ^ nl ^ "end")
in
  case strexp of

    Struct (vals, strs) =>
    "struct " ^ vec("", "", "", "", "", ",")
      (fn (id, var) => Pretty.idToString id ^ " = " ^ Pretty.idToString var) 
      (M.listItemsi vals)
    ^ ";" ^ vec("", "", "", "", "", ",")
      (fn (strid, strexp) => "structure " ^ Pretty.idToString strid ^ " = " ^ 
      displayStrExp depth strexp) (M.listItemsi strs) ^ " end"

  | Strid(id,rest) =>
    Pretty.idToString id ^ pathToString rest

  | StrLet(dec, strexp) =>
    nl ^ "let" ^ displayLet (dec,strexp)

end

and displayRecBind depth (funvar, body, funty) =
    Pretty.idToString funvar ^ " =" ^ newline depth ^ display depth body

and displayMatch depth (ty, mrules) =
  vec("", "","", "", "", newline depth ^ "| ")
  (displayMRule depth) mrules

and displayMRule depth (loc,pat,exp) =
  displayPat pat ^ " => " ^ display depth exp

and displayPat pat =
case pat of
  PatWild => "_"
| PatSCon(scon, loc) => SCon.toString scon
| PatVar (v,ty) => Pretty.idToString v
| PatRef pat => "ref " ^ displayPat pat
| PatCon(con, _, _, NONE) => Pretty.idToString con
| PatCon(con, _, _, SOME pat) => Pretty.idToString con ^ " " ^ displayPat pat
| PatExCon(exname, NONE) => SMLTy.exNameToString exname
| PatExCon(exname, SOME(ty,pat)) => 
  SMLTy.exNameToString exname ^ " " ^ displayPat pat

| PatRecord(isopen, patrow) => 
  vec("{}", 
      "{", if isopen then ",...}" else "}",
      "{", if isopen then ",...}" else "}", ",")
  (fn (lab,pat) => Pretty.idToString lab ^ "=" ^ displayPat pat) patrow
| PatLayer((v,ty), pat) => Pretty.idToString v ^ " as " ^ displayPat pat

val toString = displayStrExp 0
val expToString = display 0

(*----------------------------------------------------------------------*)
(* Is a typed SML term `valuable' (`non-expansive' in Defn)?            *)
(* We extend the definition to permit field access and method lookup    *)
(*----------------------------------------------------------------------*)
fun isValuable e =
case e of
  SCon _ => true
| Var _  => true
| Con _  => true
| ExCon _ => true
| Fn _   => true
| Record fields => List.all (fn (lab,e) => isValuable e) fields
| App(Con _, e) => isValuable e
| App(ExCon _, e) => isValuable e
| _ => false

(*----------------------------------------------------------------------*)
(* Variables bound in a pattern, with their types                       *)
(*----------------------------------------------------------------------*)
fun fvPat pat =
case pat of
  PatVar (v,ty) => 
  M.insert(M.empty, v, ty)

| PatRef pat => 
  fvPat pat

| PatCon(con, _, _, SOME pat) => 
  fvPat pat

| PatExCon(exname, SOME(ty,pat)) => 
  fvPat pat

| PatRecord(isopen, patrow) => 
  foldr (M.unionWith #1) M.empty 
    (map (fvPat o #2) patrow)

| PatLayer((v,ty), pat) => 
  M.insert(fvPat pat, v, ty)

| _ =>
  M.empty
 
(*----------------------------------------------------------------------*)
(* Free variables in a term						*)
(*----------------------------------------------------------------------*)
fun fv e =
  case e of
    Var ((v, []), tys) => 
    S.singleton v

  | App(e1, e2) =>
    S.union(fv e1, fv e2)

  | Fn (ty, match) => 
    fvMatch match

  | Let(d, e) =>
    let  
      val (bound,free) = fvDec d
    in
      S.union (free, S.difference (fv e, bound))
    end

  | Handle(exp, match) =>
    S.union (fv exp, fvMatch match)

  | Raise(e, ty, loc) =>
    fv e

  | Record exprow =>
    foldr S.union S.empty (map (fv o #2) exprow)

  | Java(j, es, tyopt, effect) => 
    foldr S.union S.empty (map fv es) 

  | _ =>
    S.empty

and fvDecItem d =
case d of

  Val(_, _, _, pat, exp) =>
  (M.foldli (fn (v,_,s) => S.add(s,v)) 
    S.empty (fvPat pat), fv exp)

| ValRec(tyvars, defs) => 
  let 
    val funs = NList.foldr (fn ((f,_,_),s) => S.add(s,f)) 
    S.empty defs
  in
    (funs, S.difference (
      NList.foldr (fn ((_,body,_),s) => S.union(s, fv body)) 
      S.empty defs,
      funs))
  end

| Local(d1, d2) =>
  let
    val (bvs1, fvs1) = fvDec d1
    val (bvs2, fvs2) = fvDec d2
  in
    (bvs2, S.union(fvs1, S.difference(fvs2, bvs1)))
  end

| ClassType _ =>
  (S.empty, S.empty)

| Exception _ =>
  (S.empty, S.empty)

| Structure _ =>
  (S.empty, S.empty)

and fvDec [] = (S.empty, S.empty)
  | fvDec (d::ds) =
    let 
      val (bvs1, fvs1) = fvDecItem d
      val (bvs2, fvs2) = fvDec ds
    in
      (S.union(bvs1, bvs2), S.union(fvs1, S.difference(fvs2, bvs1)))
    end

and fvMatch (ty, mrules) =
  foldr 
    (fn ((_,pat,e),s) => S.union(s, S.difference(fv e, 
      M.foldli (fn (v,_,s) => S.add(s,v)) 
      S.empty (fvPat pat))))
    S.empty mrules


end (* of local open*)

end (* of struct *)

