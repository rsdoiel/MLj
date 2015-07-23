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
(* Operations on free variable information                              *)
(*======================================================================*)
structure FVInfoOps =
struct

local 
  open MILTerm 
in

(*----------------------------------------------------------------------*)
(* Free variable information:                                           *)
(*   (1) the true free variables with their types;                      *)
(*       (the types are required by the letclos construct)              *)
(*   (2) the local function apps that occur;                            *)
(*   (3) the known function apps that occur;                            *)
(*   (4) the global variables that occur.                               *)
(*                                                                      *)
(* For local functions, the free variables and global variables are     *)
(* passed in as extra parameters.                                       *)
(*                                                                      *)
(* For known functions, the free variables are passed in and the global *)
(* variables are dereferenced at the head of the function.              *)
(*                                                                      *)
(* For closures, the free variables are read from instance variables    *)
(* and the global variables are dereferenced at the head of the function*)
(*----------------------------------------------------------------------*)
type VarsInfo = 
{
  fvs      : MILTy.Type Var.Map.map,
  locals   : Var.Set.set,
  known    : Var.Set.set,
  globvars : MILTy.Type Var.Map.map
}

fun infoEq 
({ fvs = fvs1, locals = locals1, known = known1, globvars = globvars1 },
 { fvs = fvs2, locals = locals2, known = known2, globvars = globvars2 }) =
 Var.Map.collate (fn _ => EQUAL) (fvs1,fvs2) = EQUAL andalso
 Var.Set.equal (locals1, locals2) andalso  
 Var.Set.equal (known1, known2) andalso
 Var.Map.collate (fn _ => EQUAL) (globvars1,globvars2) = EQUAL
  

(*----------------------------------------------------------------------*)
(* Pretty-print free var info.						*)
(*----------------------------------------------------------------------*)
fun fvsToString description fvs =
  if null fvs
  then ""
  else description ^ " = {" ^ Pretty.simpleVec "," Var.toString fvs ^ "} "

fun typedfvsToString description fvs =
  if null fvs
  then ""
  else description ^ " = {" ^ Pretty.simpleVec "," 
  (fn (x, ty) => Var.toString x ^ ":" ^ MILTy.toString ty) fvs ^ "} "

fun varsInfoToString ({ fvs, locals, known, globvars } : VarsInfo) =
  typedfvsToString "fvs" (Var.Map.listItemsi fvs) ^
  fvsToString "local" (Var.Set.listItems locals) ^
  fvsToString "known" (Var.Set.listItems known) ^
  fvsToString "globvars" (map #1 (Var.Map.listItemsi globvars))

(*----------------------------------------------------------------------*)
(* Empty free variable info.						*)
(*----------------------------------------------------------------------*)
val empty : VarsInfo = 
{
  fvs = Var.Map.empty, 
  locals = Var.Set.empty, 
  known = Var.Set.empty,
  globvars = Var.Map.empty
}

(*----------------------------------------------------------------------*)
(* Calculate the union of a pair of fv info.				*)
(*----------------------------------------------------------------------*)
fun union 
  ({fvs = fvs1, locals = locals1, known = known1, globvars = globvars1},
   {fvs = fvs2, locals = locals2, known = known2, globvars = globvars2}) =
  { fvs = Var.Map.unionWith #1 (fvs1,fvs2), 
    locals = Var.Set.union(locals1, locals2),
    known = Var.Set.union(known1, known2),
    globvars = Var.Map.unionWith #1 (globvars1, globvars2) }

(*----------------------------------------------------------------------*)
(* Remove a number of (typed) free variables from fv info.		*)
(*----------------------------------------------------------------------*)
fun removeFree ({ fvs, locals, known, globvars } : VarsInfo, vars) =
  { fvs = foldl (fn (v, m) => (#1 (Var.Map.remove(m, v))) handle _ => m) 
          fvs vars,
    locals = locals,
    known = known,
    globvars = globvars } : VarsInfo

(*----------------------------------------------------------------------*)
(* Remove a number of known function variables from fv info.            *)
(*----------------------------------------------------------------------*)
fun removeKnown ({ fvs, locals, known, globvars } : VarsInfo, vars) =
  { fvs = fvs,
    locals = locals,
    known = Var.Set.difference(known, Var.Set.addList(Var.Set.empty, vars)),
    globvars = globvars } : VarsInfo

(*----------------------------------------------------------------------*)
(* Remove a number of global variables from fv info.                    *)
(*----------------------------------------------------------------------*)
fun removeGlobVars ({ fvs, locals, known, globvars } : VarsInfo, vars) =
  { fvs = fvs,
    locals = locals,
    known = known,
    globvars = foldl (fn (v, m) => (#1 (Var.Map.remove(m, v))) handle _ => m) 
          globvars vars } : VarsInfo

(*----------------------------------------------------------------------*)
(* Remove a number of local function variables from fv info.            *)
(*----------------------------------------------------------------------*)
fun removeLocal ({ fvs, locals, known, globvars } : VarsInfo, vars) =
  { fvs = fvs,
    locals = Var.Set.difference(locals, Var.Set.addList(Var.Set.empty, vars)),
    known = known,
    globvars = globvars } : VarsInfo

(*----------------------------------------------------------------------*)
(* Add a single typed free variable.					*)
(*----------------------------------------------------------------------*)
fun addFree { fvs, locals, known, globvars } (v,ty) = 
  { fvs = Var.Map.insert(fvs, v, ty), 
    locals = locals,
    known = known,
    globvars = globvars }

(*----------------------------------------------------------------------*)
(* Add a single known function variable.       				*)
(*----------------------------------------------------------------------*)
fun addKnown { fvs, locals, known, globvars } v = 
  { fvs = fvs, 
    locals = locals, 
    known = Var.Set.add(known, v),
    globvars = globvars }

(*----------------------------------------------------------------------*)
(* Add a single global variable.       				        *)
(*----------------------------------------------------------------------*)
fun addGlobVar { fvs, locals, known, globvars } (v,ty) = 
  { fvs = fvs,
    locals = locals,
    known = known,
    globvars = Var.Map.insert(globvars, v, ty) }

(*----------------------------------------------------------------------*)
(* Add a single local function variable.       				*)
(*----------------------------------------------------------------------*)
fun addLocal { fvs, locals, known, globvars } v = 
  { fvs = fvs,
    locals = Var.Set.add(locals, v),
    known = known,
    globvars = globvars }

(*----------------------------------------------------------------------*)
(* A single typed free variable.					*)
(*----------------------------------------------------------------------*)
fun singleFree (v,ty) = 
  { fvs = Var.Map.insert(Var.Map.empty, v, ty), 
    locals = Var.Set.empty, 
    known = Var.Set.empty,
    globvars = Var.Map.empty }

(*----------------------------------------------------------------------*)
(* A single known function variable.         				*)
(*----------------------------------------------------------------------*)
fun singleKnown v = 
  { fvs = Var.Map.empty, 
    locals = Var.Set.empty, 
    known = Var.Set.singleton v,
    globvars = Var.Map.empty }

(*----------------------------------------------------------------------*)
(* A single global variable.         				        *)
(*----------------------------------------------------------------------*)
fun singleGlobVar (v,ty) = 
  { fvs = Var.Map.empty, 
    locals = Var.Set.empty, 
    known = Var.Set.empty,
    globvars = Var.Map.insert(Var.Map.empty, v, ty) }

(*----------------------------------------------------------------------*)
(* A single local function variable.         				*)
(*----------------------------------------------------------------------*)
fun singleLocal v = 
  { fvs = Var.Map.empty, 
    locals = Var.Set.singleton v, 
    known = Var.Set.empty,
    globvars = Var.Map.empty }

end (* of local open *)
end (* of struct *)

