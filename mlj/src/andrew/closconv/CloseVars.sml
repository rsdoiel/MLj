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

  fun closeVars { fvs = otherfvs, locals = localfvs, known = knownfvs, 
                  globvars } =
  let
    fun loop (result, done, localvars, knownvars) =
    case (localvars, knownvars) of
      ([], []) => 
      { fvs = Var.Map.unionWith #2 (result, otherfvs),
        locals = localfvs,
        known = knownfvs,
        globvars = globvars }

    | (localvar::localvars, _) =>
      (case Var.Map.find(funs, localvar) of
        SOME (_,{fvs=otherfvs,locals = localfvs,known = knownfvs,globvars})=>
        loop(Var.Map.unionWith #1 (globvars,  
             Var.Map.unionWith #1 (result, otherfvs)), 
          Var.Set.union(done, 
            Var.Set.union(Var.Set.add(localfvs, localvar), knownfvs)),
          Var.Set.listItems (Var.Set.difference(localfvs,done)) @ localvars,
          Var.Set.listItems (Var.Set.difference(knownfvs,done)) @ knownvars)

      | NONE => 
        raise Fail 
        ("ClosConv.closeVars: no such local function "^Var.toString localvar))

    | (_, knownvar::knownvars) =>
      (case Var.Map.find(funs, knownvar) of
        SOME (_,{fvs=otherfvs,locals = localfvs,known = knownfvs,globvars})=> 
        loop(Var.Map.unionWith #1 (result, otherfvs), 
          Var.Set.union(done, 
            Var.Set.union(Var.Set.add(localfvs, knownvar), knownfvs)),
          Var.Set.listItems (Var.Set.difference(localfvs,done)) @ localvars,
          Var.Set.listItems (Var.Set.difference(knownfvs,done)) @ knownvars)
      | NONE => 
        raise Fail 
        ("ClosConv.closeVars : no such known function "^Var.toString knownvar))

  in
    loop (otherfvs,
      Var.Set.empty, 
      Var.Set.listItems localfvs, 
      Var.Set.listItems knownfvs)
  end

