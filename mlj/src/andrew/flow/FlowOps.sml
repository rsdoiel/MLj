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
(* Auxiliary functions for flow analysis and flow-based function opts.  *)
(*======================================================================*)
structure FlowOps =
struct

local 
  open FlowTypes
in

(*----------------------------------------------------------------------*)
(* Pretty-print a scope.						*)
(*----------------------------------------------------------------------*)
local
  fun posToString InTail = "C"
    | posToString InLet = "L"
    | posToString InTry = "T"
in
  fun scopeToString (pos, path) = 
    "(" ^ posToString pos ^ ")" ^ MILPathOps.toString path
end

(*----------------------------------------------------------------------*)
(* `Enter' a new scope       						*)
(*----------------------------------------------------------------------*)
fun newScope ((p,vars),v) = (p,v :: vars)

(*----------------------------------------------------------------------*)
(* Let and Try scopes.							*)
(*----------------------------------------------------------------------*)
fun inLet (p,vars) = (InLet,vars)
fun inTry (p,vars) = (InTry,vars)
fun inTail (p,vars) = (InTail,vars)

(*----------------------------------------------------------------------*)
(* Top-level tail scope (i.e. <clinit>)					*)
(*----------------------------------------------------------------------*)
val topTail = (InTail, [])

fun normaliseScope keep (p,vars) = (p,
  List.filter 
  (fn MILPath.LetFun f => keep f 
    | MILPath.LetClass _ => true 
    | _ => false) vars)

fun hoistScope (vars1 : MILPath.Path, vars2 : MILPath.Path) =
let
  fun loop' (changed,prefix) [] = 
      if changed then SOME prefix else NONE

    | loop' (changed,prefix) (items as ((item as (MILPath.LetFun f))::items')) = 
      if changed then SOME (rev items @ prefix) else NONE

    | loop' (changed,prefix) (item::items) = 
      loop' (true,item::prefix) items
       
  fun loop prefix (item1::items1, item2::items2) =
      if MILPathOps.eqItem (item1,item2) then loop (item1::prefix) (items1,items2)
      else loop' (false,prefix) (item2::items2)
    | loop prefix (_,items2) = loop' (false,prefix) items2
in
  loop [] (rev vars1,rev vars2)
end

fun eq ((p1,vars1) : Scope, (p2,vars2) : Scope) =
  p1=p2 andalso MILPathOps.eq (vars1, vars2)



end (* of local open *)

end (* of struct *)

