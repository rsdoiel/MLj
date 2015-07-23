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
(* Auxiliary stuff for elaboration					*)
(*======================================================================*)
structure ElabOps :> ELABOPS = 
struct

local
  open Syntax SMLTy SMLPrimTy EnvOps
in

structure T = SMLTerm
structure Map = Symbol.OrdMap

fun vidToLongid (Short id) = [id]
  | vidToLongid (Long longid) = longid
  | vidToLongid (OpShort id) = [id]

(*----------------------------------------------------------------------*)
(* Merge two pattern environments (bindings of pattern variables to 	*)
(* their types and paths) raising an error if duplicates are found,	*)
(* enforcing the first "syntactic" restriction in Section 2.9 of Defn.	*)  
(*----------------------------------------------------------------------*)
fun patmerge loc (m1, m2) =
  let fun f [] = m1
        | f ((x,v)::m') = 
          let val m = f m'
          in
            case Map.find (m,x) of
              NONE => Map.insert(m,x,v)
          | _ => 
            (error (Error.error(loc, "duplicate variable in pattern: " 
              ^ Pretty.idToString x), []); m)
          end
  in 
    f (Map.listItemsi m2) 
  end

(*----------------------------------------------------------------------*)
(* A monotyped unqualified variable					*)
(*----------------------------------------------------------------------*)
fun monovar v = T.Var((v,[]),[])

(*----------------------------------------------------------------------*)
(* Typed term for a case construct					*)
(*----------------------------------------------------------------------*)
fun caseTerm (e, ty, match) = T.App(T.Fn(ty, match), e)

(*----------------------------------------------------------------------*)
(* Typed term for a conditional.					*)
(*----------------------------------------------------------------------*)
fun condTerm (e1, (e2,loc2), (e3,loc3), ty) =
  caseTerm(e1, boolType,
    (ty, [(loc2, T.PatCon(Ids.trueSym, TopEnv.boolCE, [], NONE), e2), 
          (loc3, T.PatCon(Ids.falseSym, TopEnv.boolCE, [], NONE), e3)]))

(*----------------------------------------------------------------------*)
(* Typed term for a tuple.						*)
(*----------------------------------------------------------------------*)
fun tupleTerm es =
let fun make [] n = []
      | make (e::es) n = (Ids.symbol (Int.toString n), e) :: make es (n+1)
in
  T.Record (make es 1)
end

(*----------------------------------------------------------------------*)
(* List pattern derived form						*)
(*----------------------------------------------------------------------*)
fun patList loc [] = 
    (loc, PatVar(Short Ids.nilSym))

  | patList loc ((pat as (loc',_))::pats) = 
    (loc, PatCon([Ids.consSym], 
      (loc, PatRecord(false, 
      [(Ids.symbol "1", pat), (Ids.symbol "2", patList loc' pats)]))))

(*----------------------------------------------------------------------*)
(* While derived form							*)
(*----------------------------------------------------------------------*)
fun makeWhileTerm (loc, e1, e2) =
  let
    val v = SMLTy.freshVar ()
  in
    T.Let([T.ValRec(
    [], 
    ((v, 
      T.Fn(unitType, 
           (unitType, [(loc, T.PatWild, 
             condTerm(
               e1, 
               (T.Let([T.Val(loc, [], unitType, T.PatWild, e2)], 
                T.App(monovar v, T.Record[])), loc), 
               (T.Record[], loc),
               unitType)
            )])),
      SMLTy.funType (unitType, unitType)
     ),[]))],
    T.App(monovar v, T.Record []))
  end
  

(*----------------------------------------------------------------------*)
(* Construct fn x1 => ... => fn xn => e					*)
(*----------------------------------------------------------------------*)
fun abs [] e = e
  | abs (v::vs) (e as (loc,_)) = 
    (loc, Fn [((loc, PatVar(Short v)), abs vs e)])

(*----------------------------------------------------------------------*)
(* Tuple pattern derived form						*)
(*----------------------------------------------------------------------*)
fun patTuple loc [pat] = pat
  | patTuple loc pats =
let fun make [] n = []
      | make (pat::pats) n = 
        (Ids.symbol (Int.toString n), pat) :: make pats (n+1)
in
  (loc, PatRecord(false, make pats 1))
end

(*----------------------------------------------------------------------*)
(* Tuple expression derived form					*)
(*----------------------------------------------------------------------*)
fun expTuple loc [exp] = exp
  | expTuple loc exps =
let fun make [] n = []
      | make (exp::exps) n = 
        (Ids.symbol (Int.toString n), exp) :: make exps (n+1)
in
  (loc, Record(make exps 1))
end

(*----------------------------------------------------------------------*)
(* Generate code corresponding to a source-level open construct		*)
(*----------------------------------------------------------------------*)
fun makeOpen (loc, E, (topstrid, path) : T.longid) =
let
  val (i1,d1) = 
    Map.foldli (fn 
      (strid, E, (i, d)) => (i+1, 
      T.Structure(strid, 
        T.Strid(topstrid, path @ [(strid,i)]))::d))
    (0, []) 
    (SEofE E)

  val (i2,d2) = 
    Map.foldli (fn 
      (id, ValBind.VarSch(SMLSch.TypeScheme(tyvars,ty)), (i, d)) => 
      (case map TyVar.sort tyvars of
        [TyVar.Overloaded tynameset] => 
         (i+1,
         T.Val(loc, [], ty, T.PatVar (id, ty), 
           T.OverloadedVar((topstrid, path @ [(id,i)]), tynameset, []))::d)

      | _ =>
         (i+1,
         T.Val(loc, tyvars, ty, 
         T.PatVar (id, ty), T.Var((topstrid, path @ [(id,i)]), 
            map tyVarType tyvars))::d)
      )
    | (_, _, (i, d)) => (i, d)) 
   (i1, d1)
   (VEofE E)
in
  d2
end

end (* of local open *)

end (* of struct *)