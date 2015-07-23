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
(* Signature matching							*)
(*======================================================================*)
structure Match :> MATCH =
struct

local 
  open SMLTy Env EnvOps SMLSch ValBind 
in

structure T = SMLTerm
structure Map = Symbol.OrdMap

(*----------------------------------------------------------------------*)
(* Enrichment for type schemes						*)
(*----------------------------------------------------------------------*)
fun matchSch (loc,longid) (strSch as TypeScheme(strTyvars, strTy),
                           sigSch as TypeScheme(sigTyvars, sigTy)) =

case SMLTy.match false (strTy, sigTy) of
  NONE => 
  (error (Error.error (loc, "match error: " ^ 
    Pretty.longidToString longid),
    [("type specified", sigTy), ("type inferred", strTy)]); NONE)

| SOME S => 
  SOME (
    sigTyvars, 
    sigTy,
    map (fn tyvar => case TyVar.Map.find(S, tyvar) of 
          NONE => tyVarType tyvar
        | SOME ty => ty) strTyvars)

(*----------------------------------------------------------------------*)
(* First stage of matching for variable bindings: check that the        *)
(* identifier statuses are valid and return an exception map.           *)
(*----------------------------------------------------------------------*)
fun match1vb exmap (loc,longid) (strVB, sigVB) =
case (strVB, sigVB) of

  (* Signature specifies value and structure provides value *)
  (VarSch _, VarSch _) =>
  exmap

  (* Signature specifies excon and structure provides value *)
| (VarSch _, ExTy _) => 
  (error (Error.error (loc, 
    "signature specified excon but structure provided exception value: "
    ^ Pretty.longidToString longid), []); exmap)

  (* Signature specifies constructor and structure provides value *)
| (VarSch _, ConSch _) => 
  (error (Error.error (loc,
    "signature specified constructor but structure provided value: "
    ^ Pretty.longidToString longid), []); exmap)

  (* Signature specifies value and structure provides constructor *)
| (ConSch _, VarSch _) =>
  exmap

  (* Both specifiy constructor *)
| (ConSch _, ConSch _) => 
  exmap

  (* Signature specifies excon and structure provides constructor *)
| (ConSch _, ExTy ty) =>
  (error (Error.error (loc,
    "signature specified excon but structure provided data constructor: "
    ^ Pretty.longidToString longid), []); exmap)

  (* Signature specifies value and structure provides excon *)
| (ExTy _, VarSch _) =>
  exmap

  (* Signature specifies constructor and structure provides excon *)
| (ExTy _, ConSch _) => 
  (error (Error.error (loc,
    "signature specified data constructor but structure provided excon: "
    ^ Pretty.longidToString longid), []); exmap)

  (* Signature specifies excon and structure provides excon *)
| (ExTy(_,strexname), ExTy(_,sigexname)) =>
  SMLTy.ExMap.insert(exmap, sigexname, strexname)

(*----------------------------------------------------------------------*)
(* Second stage of matching for variable bindings: enrichment.		*)
(* For constructors and exceptions, check that the types match exactly. *)
(* For variables, return an appropriate declaration item and variable.  *)
(*----------------------------------------------------------------------*)
fun match2vb (loc, longid, longid' : SMLTerm.longid) (strVB, sigVB) =
let
  val v = List.last longid
in
case (strVB, sigVB) of
  (* Signature specifies value and structure provides value *)
  (VarSch strsch, VarSch sigsch) =>
  (case matchSch (loc,longid) (strsch, sigsch) of
    NONE => NONE
  | SOME (tyvars, ty, tys) => 
    SOME 
    (T.Val(loc,tyvars,ty,T.PatVar(v,ty), T.Var(longid', tys)), v)
  )

  (* Signature specifies value and structure provides constructor *)
| (ConSch(strsch,CE), VarSch sigsch) =>
  (case matchSch (loc,longid) (strsch, sigsch) of
    NONE => NONE
  | SOME (tyvars, ty, tys) => 
    SOME 
    (T.Val(loc,tyvars,ty,T.PatVar(v,ty), 
     T.Con(v, CE, tys)), v)
  )

  (* Both specify constructor *)
| (ConSch(strsch as TypeScheme(strtyvars,strty),_), 
   ConSch(sigsch as TypeScheme(sigtyvars,sigty),_)) => 
  if SMLTy.eq (strty, 
         appSubst (ListPair.zip(sigtyvars, map tyVarType strtyvars)) sigty)
  then NONE
  else (error (Error.error (loc, 
    "type in signature doesn't match structure spec: " ^ 
    Pretty.longidToString longid),
    [("in signature", sigty), ("in structure", strty)]); NONE)

  (* Signature specifies value and structure provides excon *)
| (ExTy(strty,excon), VarSch(TypeScheme(_,sigty))) =>
  if SMLTy.eq (strty, sigty)
  then SOME 
  (T.Val(loc,[],sigty,T.PatVar(v,sigty),
    T.ExCon(excon, Option.map #1 (fromFunType strty))), v)
  else
    (error (Error.error (loc, 
    "type in signature doesn't match structure spec: " ^ 
    Pretty.longidToString longid),
    [("in signature", sigty), ("in structure", strty)]); NONE)

  (* Signature specifies excon and structure provides excon *)
| (ExTy(strty,excon), ExTy(sigty,_)) =>
  if SMLTy.eq (strty, sigty)
  then NONE
  else 
    (error (Error.error (loc, 
    "type in signature doesn't match structure spec: " ^ 
    Pretty.longidToString longid),
    [("in signature", sigty), ("in structure", strty)]); NONE)

  (* Otherwise it's an error that's already been reported *)
| _ =>
  NONE
end

(*----------------------------------------------------------------------*)
(* First stage of signature matching: check structural stuff (if 	*)
(* signature defines an entity (var, type, substructure) then structure *)
(* should define it too). Also do instantiation of tynames to typefcns. *)
(*----------------------------------------------------------------------*)
fun match1 loc (strE : Env.Env, sigma : Env.Sig as (N,sigE)) =
let
  fun matchSE path acc (strSE : Env.StrEnv, sigSE : Env.StrEnv) =
  let
    fun matchBindings acc [] = 
        acc

      | matchBindings acc ((strid, sigE)::rest) =
        case Map.find(strSE, strid) of
          NONE => 
          (error (Error.error(loc, 
            "unmatched structure specification: " ^ 
            Pretty.longidToString (path @ [strid])), []);
          matchBindings acc rest)
  
        | SOME strE => 
          matchBindings (matchE (path @ [strid]) acc (strE, sigE)) rest
  in
    matchBindings acc (Map.listItemsi sigSE)
  end

  and matchTE path acc (strTE : Env.TyEnv,sigTE : Env.TyEnv) =
  let
    fun matchBindings acc [] = 
        acc

      | matchBindings (acc as (psi,exmap)) ((tycon, sigtystr)::rest) =
        case Map.find(strTE, tycon) of
          NONE => 
          (error (Error.error(loc, "unmatched type specification: " ^
            Pretty.longidToString (path @ [tycon])), []);
          matchBindings acc rest)
  
        | SOME strtystr => 
          let
            val acc = 
            case TyStr.match1 psi (strtystr, sigtystr) of
              TyStr.Failure message => 
              (error (Error.error(loc, message ^ ": " ^
                Pretty.longidToString (path @ [tycon])), []); acc)
            | TyStr.Success psi => (psi,exmap)
          in
            matchBindings acc rest
          end
   in
     matchBindings acc (Map.listItemsi sigTE)
   end

  and matchVE path acc (strVE : Env.ValEnv, sigVE : Env.ValEnv) =
  let
    fun matchBindings acc [] = 
        acc

      | matchBindings (acc as (psi,exmap)) ((id, sigvb)::rest) =
        case Map.find(strVE, id) of
          NONE => 
          (error (Error.error(loc, "unmatched value specification: " ^
            Pretty.longidToString (path @ [id])), []);
          matchBindings acc rest)
  
        | SOME strvb => 
          let
            val exmap = match1vb exmap (loc, path @ [id]) (strvb, sigvb)
          in
            matchBindings (psi,exmap) rest
          end
   in
     matchBindings acc (Map.listItemsi sigVE)
   end

 and matchE path acc (strE : Env.Env, sigE : Env.Env) =
   let
     val acc = matchVE path acc (VEofE strE, VEofE sigE)
     val acc = matchTE path acc (TEofE strE, TEofE sigE)
     val acc = matchSE path acc (SEofE strE, SEofE sigE)
   in
     acc
   end
in
  matchE [] (TyName.Map.empty, SMLTy.ExMap.empty) (strE, sigE)
end

(*----------------------------------------------------------------------*)
(* Second stage of signature matching: enrichment.			*)
(* Wrt elaboration, this is just a check (does it match?) but we also   *)
(* require a term in which polymorphic variables are specialised        *)
(* appropriately.                                                       *)
(*----------------------------------------------------------------------*)
fun match2 (topstrid,loc) (strE : Env.Env, sigE : Env.Env) =
let
  fun matchSE (path, (topstrid,tail)) 
              (strSE : Env.StrEnv, sigSE : Env.StrEnv) =
  let
    fun matchBindings strval [] = 
        strval

      | matchBindings strval ((strid, sigE)::rest) =
        case EnvLookup.lookupStrSE(strSE, strid) of
         
          (* We've already reported this error *)
          NONE => matchBindings strval rest

        | SOME (strE, i) => 
          let
            val strexp = matchE (path @ [strid], (topstrid,tail @ [(strid,i)]))
              (strE, sigE)
          in
            matchBindings (Map.insert(strval, strid, strexp)) rest
          end
  in
    matchBindings Map.empty (Map.listItemsi sigSE)
  end

  and matchTE path (strTE : Env.TyEnv, sigTE : Env.TyEnv) =
  let
    fun matchBindings [] = 
        ()

      | matchBindings ((tycon, sigtystr)::rest) =
        case Map.find(strTE, tycon) of

          (* We've already reported this error *)
          NONE => 
          matchBindings rest
  
        | SOME strtystr => 
          ((case TyStr.match2 (strtystr, sigtystr) of
            SOME message => 
            error (Error.error(loc, message ^ ": " ^
              Pretty.longidToString (path @ [tycon])), [])
          | NONE => ());
          matchBindings rest)
   in
     matchBindings (Map.listItemsi sigTE)
   end

  and matchVE (path, (topstrid, tail)) 
              (n, strVE : Env.ValEnv, sigVE : Env.ValEnv) =
  let
    fun matchBindings args [] = 
        args

      | matchBindings (ds,sval) ((id, sigvb)::rest) =          
        case EnvLookup.lookupVarVE(strVE, id) of

          (* We've already reported this error *)
          NONE => (ds,sval)
  
        | SOME (strvb, i) => 
          case match2vb (loc, path @ [id], (topstrid, tail @ [(id, i + n)])) 
               (strvb, sigvb) of
            NONE => matchBindings (ds,sval) rest
          | SOME (d,v) => matchBindings (d::ds,Map.insert(sval, id, v)) rest
   in
     matchBindings ([],Map.empty) (Map.listItemsi sigVE)
   end

 and matchE (path, longid : SMLTerm.longid) (strE : Env.Env, sigE : Env.Env) =
   let
     val strs = matchSE (path,longid) (SEofE strE, SEofE sigE)
     val (ds, vals) = matchVE (path,longid) 
     (Map.numItems (SEofE strE), VEofE strE, VEofE sigE)
     val _ = matchTE path (TEofE strE, TEofE sigE)
   in
     T.StrLet(ds, T.Struct(vals, strs))
   end
in

  matchE ([], (topstrid, [])) (strE, sigE)

end

end (* of local open *)
end (* of struct *)
