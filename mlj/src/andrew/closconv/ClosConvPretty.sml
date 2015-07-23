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
(* Pretty-print the results of closure conversion.			*)
(*======================================================================*)
structure ClosConvPretty :> CLOSCONVPRETTY =
struct

fun line 0 = ""
  | line n = "-" ^ line (n-1)

val divide = line 78 ^ "\n"

fun varsToString vs =
  Pretty.simpleVec "\n" (fn (i,ty) => "  " ^ 
    Int.toString i ^ ":" ^ MILTy.toString ty)
  (Gen.mapi Gen.id vs)


fun globvarsToString vs =
  Pretty.simpleVec "\n" (fn (x,ty) => "  " ^ 
    Var.toString x ^ ":" ^ MILTy.toString ty)
  (Var.Map.listItemsi vs)

fun globfunToString (i, (tyvars, f, tabs)) =
  divide ^
  "Global function " ^ Int.toString i ^ " (" ^ Var.toString f ^ "):\n" ^
  "(" ^ Pretty.simpleVec "," MILTy.boundTyVarToString tyvars ^ ") " ^
  MILPretty.tabstrToString tabs ^ "\n"

fun classToString (name,info,fields,methods) =
  divide ^  
  MILPretty.classdefToString (name,info,fields,methods) ^ "\n"

fun appToString (i, { tyvars, fvtys, fundef = (f, tabs, cty) }) =
  "\nApp method " ^ Int.toString i ^ " as " ^ 
  Var.toString f ^ ":\n" ^
  "(" ^ Pretty.simpleVec "," MILTy.boundTyVarToString tyvars ^ ") " ^
  "Free vars:\n" ^ varsToString fvtys ^ "\nDef:\n" ^
  MILPretty.tabstrToString tabs ^ "\n"
 
fun closdefToString (i, { fvtys, meths }) =
  divide ^ "Closure " ^ Int.toString i ^ "\nFree vars:\n" ^ 
  varsToString fvtys ^ "\n" ^ 
  Pretty.simpleVec "\n" appToString (IMap.listItemsi meths) ^ "\n"

fun methTysToString methtys =
  Pretty.simpleVec "\n" (fn (n, ty) => "  " ^ Int.toString n ^ ":" ^
    MILTy.toString ty) (Gen.mapi Gen.id methtys)

fun dump {fundefs, funenv, globvars, classdefs, closdefs, appmeths, clinit,
  methtys } =
  (Debug.print (divide ^ "Global vars:\n" ^ globvarsToString globvars ^ "\n");
   Debug.print (divide ^ "Method types:\n" ^ methTysToString methtys ^ "\n");
   Debug.print (divide ^ "App methods:\n" ^ 
     Pretty.simpleVec "\n" (fn (v,i) => Var.toString v ^ " : " ^Int.toString i)
     (Var.Map.listItemsi appmeths) ^ "\n");
   Debug.print (divide ^ "<clinit>:\n" ^ MILPretty.cmpToString clinit ^ "\n");
   Gen.appi (fn p => Debug.print ("\n" ^ globfunToString p)) fundefs;
   app (fn p => Debug.print ("\n" ^ classToString p)) classdefs;
   Gen.appi (fn p => Debug.print ("\n" ^ closdefToString p)) closdefs)

end

