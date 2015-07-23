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
(* 									*)
(*======================================================================*)
structure FVClose =
struct

fun trans funs =
let

fun loop (funs : FVInfo.FunInfo, iter) =
let
  val changed = ref false
  fun process 
  (f, { kind, args, appmeth, tyvars, fvs = { fvs, locals, known, globvars }}) =
  case kind of
    (SOME _) =>
    let
      val fvs' = 
        Var.Set.foldl (fn (f, fvs) =>
        let
          val SOME { fvs = { fvs = fvs', globvars = globvars', ... }, ... } =
            Var.Map.find(funs, f)
        in
          Var.Map.foldli (fn (x, ty, fvs) => 
            if List.exists (fn (x',_) => x=x') args
            then fvs else Var.Map.insert(fvs, x, ty)) fvs fvs'
        end) fvs (if kind <> SOME MILTerm.LocalFun then known else
                  Var.Set.union(locals, known))
    in
      if Var.Map.numItems fvs = Var.Map.numItems fvs' then ()
      else changed := true;
      { kind = kind, args = args, appmeth = appmeth, tyvars = tyvars,
        fvs = {fvs = fvs', locals = locals, known = known, globvars = globvars}
      }
    end

  | _ =>
      { kind = kind, args = args, appmeth = appmeth, tyvars = tyvars,
        fvs = {fvs = fvs, locals = locals, known = known, globvars = globvars}
      }

  val funs = Var.Map.mapi process funs
in
  if !changed then loop (funs, iter+1)
  else 
  (
    if Controls.isOn "showFVInfo" 
    then Debug.print("\nClosure after " ^ Int.toString iter ^ " iterations.")
    else ();
    funs
  )
end 

in
  loop (funs, 1)
end


end
