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
(* Type check the results of closure conversion.			*)
(*======================================================================*)
structure ClosConvCheck :> CLOSCONVCHECK =
struct

(*
fun classToString (name,info,fields,methods) =
  divide ^  
  MILPretty.classdefToString (name,info,fields,methods) ^ "\n"

*)

fun check 
  ( {fundefs, funenv, globvars, classdefs, closdefs, appmeths, clinit,methtys} 
  : ClosConvTypes.Result) =
let
  val funenv = Var.Map.map #2 funenv
  val globenv = globvars (* Var.Map.map (fn ty => MILTy.refty ty) globvars *)

(*
  val B = (#1 B, map (fn { fvtys, meths } => 
    (fvtys, 
      IMap.map (fn { tyvars, fvtys, fundef = (f,(vs,_),cty) } => 
      MILTy.forall(tyvars, MILTy.arrow(map #2 vs,cty))) meths))
    closdefs)
*)

  fun checkFun (tyvars, f, (xs, e)) =
    (Debug.print ("\nChecking global function " ^ Var.toString f);
    ignore (TypeCheck.check true  
      { kindenv = Var.extend(Var.Map.empty, tyvars), 
        tyenv = Var.extend(Var.Map.empty, xs), 
        funenv = funenv,
        globenv = globenv } e))

  fun checkClosMeth (i, { tyvars, fvtys, fundef = (f, (xs,e), cty) }) =
    (Debug.print ("\nChecking app method " ^ Var.toString f);
    ignore (TypeCheck.check true 
      { kindenv = Var.extend(Var.Map.empty, tyvars), 
        tyenv = Var.Map.insert(Var.extend(Var.Map.empty, xs), f, 
          MILTy.closure (SOME i, fvtys)), 
        funenv = funenv,
        globenv = globenv } e))

  fun checkClos (i, { fvtys, meths }) =
    (Debug.print ("\nChecking closure " ^ Int.toString i);
      IMap.appi checkClosMeth meths)
in
  List.app checkFun fundefs;
  Gen.appi checkClos closdefs;
  Debug.print "\nChecking clinit";
  ignore (TypeCheck.check true { tyenv = Var.Map.empty, 
    kindenv = Var.Map.empty, globenv = globenv, funenv = funenv } clinit);
  Debug.print "\nChecking complete.\n"
end

end

