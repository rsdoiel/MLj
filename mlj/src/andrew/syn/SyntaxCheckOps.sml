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
(* Auxiliary functions for SyntaxCheck.					*)
(*======================================================================*)
structure SyntaxCheckOps =
struct

(*----------------------------------------------------------------------*)
(* Check atoms for duplicates, creating error message if they exist.	*)
(*----------------------------------------------------------------------*)
fun checkDupAtoms (loc,atoms,message) errors =
case Dups.duplicateAtoms atoms of
    [] => 
    errors

  | atoms => 
    Error.error(loc, message ^ ": " ^ Pretty.simpleVec "," 
      Pretty.idToString atoms)::errors

(*----------------------------------------------------------------------*)
(* Check located atoms for duplicates, creating error message if they   *)
(* exist. The error message includes each duplicate's position.        	*)
(*----------------------------------------------------------------------*)
fun checkDupLocAtoms (locatoms,message) errors =
  foldr (fn ((a,locs as loc::_),errors) => Error.error(loc,message)::errors)
  errors (Dups.duplicateAtoms' locatoms)

(*----------------------------------------------------------------------*)
(* Check Java modifiers for duplicates.                                 *)
(*----------------------------------------------------------------------*)
fun checkDupMods (loc,mods,message) errors =
case Dups.duplicates (op=) mods of
    [] => 
    errors

  | mods => 
    Error.error(loc, message)::errors

(*----------------------------------------------------------------------*)
(* Check if a list of modifiers contains more than one of		*)
(* _public, _private or _protected.                                     *)
(*----------------------------------------------------------------------*)
fun checkAccessMods (loc,mods) errors =
  if length (List.filter 
    (fn m => m=JavaFlags.PUBLIC orelse m=JavaFlags.PRIVATE
       orelse m=JavaFlags.PROTECTED) mods) > 1
  then 
    Error.error(loc, "more than one public/private/protected modifier")::errors
  else 
    errors




end (* of struct *)

