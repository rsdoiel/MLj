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
(* Compile everything that came back from closure conversion.		*)
(*======================================================================*)
structure CompileAll :> COMPILEALL =
struct

local open CompileOps
in

fun compile projname 
  { fundefs, globvars, classdefs, closdefs, clinit, funenv, appmeths,
    methtys } = 
let
  val (_,globenv) = Var.Map.foldli (fn (x, ty, (i,m)) => 
    (i+1,Var.Map.insert(m,x,(i,ty)))) (0,Var.Map.empty) globvars

  val env = { known = funenv, blocks = Var.Map.empty, 
              tyenv = Var.Map.empty, globenv = globenv, 
              kindenv = Var.Map.empty,
              apps = appmeths }
(*
  val B = (B, map (fn { fvtys, meths } => 
    (fvtys, 
    IMap.map (fn { tyvars, fvtys, fundef = (_,(vs,_),cty) } => 
       MILTy.forall(tyvars, MILTy.arrow(map #2 vs,cty))) meths))
    closdefs)
*)
in
  SaveClass.openZip projname;
  CompileReps.start ();
  CompileOnePlus.init ();

  (app (SaveClass.save o CompileClassType.compile env) classdefs;
  Gen.appi (SaveClass.save o CompileClosure.compile env) closdefs;
  if null methtys then () 
  else SaveClass.save (CompileClosure.makeTopFun methtys);
  app SaveClass.save 
    (CompileGlobals.compile env (Var.Map.listItems globvars,fundefs,clinit))
  )
  handle e => (CompileReps.finish false; SaveClass.closeZip (); raise e);

  CompileReps.finish true; SaveClass.closeZip ()
end

end (* of local open *)

end (* of struct *)
