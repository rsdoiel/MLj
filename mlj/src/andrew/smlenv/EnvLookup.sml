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
(* Lookup functions for SML environments				*)
(* We must keep track of the actual position of an identifier within    *)
(* the domain of VE or SE so that translation into MIL tuple            *)
(* projections is straightforward.                                      *)
(*======================================================================*)
structure EnvLookup :> ENVLOOKUP = 
struct

local
  open SMLTy ValBind
in

(*----------------------------------------------------------------------*)
(* What position is atom s in map m?					*)
(*----------------------------------------------------------------------*)
fun lookup (m,s) =
let
  fun find i [] = NONE
    | find i ((s',v)::rest) = 
      if Symbol.equal(s,s') then SOME (v,i) else find (i+1) rest
in
  find 0 m
end

(*----------------------------------------------------------------------*)
(* Lookup an identifier, following path down substructures		*)
(*----------------------------------------------------------------------*)
fun lookupVid (E, loc, []) = 
    Debug.fail "Env.lookupVid: empty path"

  | lookupVid (E, loc, [var]) = 
    Symbol.OrdMap.find(EnvOps.VEofE E, var)

  | lookupVid (E, loc, strid :: vid) =
    case Symbol.OrdMap.find(EnvOps.SEofE E, strid) of
      NONE => 
      (error (Error.error (loc, "unbound structure: " ^ 
        Pretty.idToString strid), []); NONE)

    | SOME E => 
      lookupVid (E, loc, vid)


(*----------------------------------------------------------------------*)
(* Look up a variable in a value environment				*)
(*----------------------------------------------------------------------*)
fun lookupVarVE (VE, v) =
let
  fun lookup i [] = NONE
    | lookup i ((v',vb)::rest) =
      if Symbol.equal(v,v') then SOME (vb, i)
      else case vb of
        VarSch _ => lookup (i+1) rest
      | _ => lookup i rest
in
  lookup 0 (Symbol.OrdMap.listItemsi VE)
end

(*----------------------------------------------------------------------*)
(* Look up a structure identifier in a structure environment        	*)
(*----------------------------------------------------------------------*)
fun lookupStrSE (SE, strid) =
let
  fun lookup i [] = NONE
    | lookup i ((strid',E)::rest) =
      if Symbol.equal(strid,strid') then SOME (E, i)
      else lookup (i+1) rest
in
  lookup 0 (Symbol.OrdMap.listItemsi SE)
end

(*----------------------------------------------------------------------*)
(* Lookup an identifier, following path down substructures		*)
(* Also return positions of identifiers within environments.            *)
(*----------------------------------------------------------------------*)
fun lookupVid' (E, loc, []) = 
    Debug.fail "Env.lookupVid': empty path"

  | lookupVid' (E, loc, [var]) = 
    (case Symbol.OrdMap.find(EnvOps.VEofE E, var) of
      NONE => 
      NONE

    | SOME vbind =>
      (SOME (vbind, (var, []))))

  | lookupVid' (E, loc, strid::(op:: rest)) =
    let
      fun lookupSubStr (E, (var, [])) =
          (case lookupVarVE (EnvOps.VEofE E, var) of 
            NONE => 
            NONE
    
          | SOME (vb, i) =>
            (SOME (vb, [(var, 
              Symbol.OrdMap.numItems (EnvOps.SEofE E) + i)])))

        | lookupSubStr (E, (strid, op:: rest)) =
          case lookup(Symbol.OrdMap.listItemsi (EnvOps.SEofE E), strid) of
            NONE => 
            (error (Error.error(loc, "missing substructure: " ^ 
              Pretty.idToString strid), []); NONE)

          | SOME (E, i) => 
            case lookupSubStr (E, rest) of
              NONE =>
              NONE

            | SOME (vbind, path) =>
              SOME (vbind, (strid,i)::path)
    in
      case Symbol.OrdMap.find(EnvOps.SEofE E, strid) of
        NONE =>
(* should be error *)
        (error (Error.warning(loc, "missing structure: " ^ 
          Pretty.idToString strid), []); NONE)

      | SOME E =>
        case lookupSubStr (E, rest) of
          NONE => 
          NONE

        | SOME (vbind, path) =>
          (SOME (vbind, (strid, path)))
    end

(*----------------------------------------------------------------------*)
(* Lookup a longtycon, following path down substructures.		*)
(* If it doesn't exist, try looking for a Java class instead.           *)
(*----------------------------------------------------------------------*)
fun lookupTyCon (E, loc, longtycon) =
let
  fun lookup (E, [tycon]) = 
      Symbol.OrdMap.find (EnvOps.TEofE E, tycon)

    | lookup (E, strid :: longtycon) =
      case Symbol.OrdMap.find(EnvOps.SEofE E, strid) of
        NONE => NONE
      | SOME E => lookup (E, longtycon)
in
  case lookup (E, longtycon) of
    SOME tystr => 
    SOME tystr

  | NONE =>
    (error (Error.error(loc, "unbound type constructor: " ^ 
    Pretty.longidToString longtycon),[]); NONE)
end

(*----------------------------------------------------------------------*)
(* Lookup a structure, following path down substructures		*)
(*----------------------------------------------------------------------*)
fun lookupStr (E, loc, []) = 
    Debug.fail "EnvOps.lookupStr: empty path"

  | lookupStr (E, loc, strid :: longid) = 
    let
      fun lookupSubStr (E, []) = 
          (E, [])

        | lookupSubStr (E, strid :: longid) =
          case lookup(Symbol.OrdMap.listItemsi (EnvOps.SEofE E), strid) of
            NONE => 
            (error (Error.error(loc, "missing substructure: " ^ 
              Pretty.idToString strid), []); (EnvOps.emptyE, []))

          | SOME (E, i) => 
            let 
              val (E', longid') = lookupSubStr (E, longid)
            in
              (E', (strid,i)::longid')
            end
    in
      case Symbol.OrdMap.find(EnvOps.SEofE E, strid) of
        NONE =>
        (error 
          (Error.error(loc, "missing structure: " ^ 
            Pretty.idToString strid), []); (EnvOps.emptyE, (strid,[])))

      | SOME E =>
        let 
          val (E', longid') = lookupSubStr (E, longid)
        in
          (E', (strid, longid'))
        end
    end

end

end
