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
(* Version information							*)
(*======================================================================*)
structure Version =
struct

val MLJversion = "0.2c"
val compilerVersion = #version_id Compiler.version
val compilerName = #system Compiler.version
val compileTime = ref ""
val basisTime = ref ""
val JDKversion = ref "jdk1.1.1"
val hasBigIntegers = ref true

val versionToString = Pretty.simpleVec "." Int.toString

(*----------------------------------------------------------------------*)
(* Message printed upon entering an interactive session			*)
(* Also put in log.                                                     *)
(*----------------------------------------------------------------------*)
fun welcomeMessage () =
  "MLj " ^ MLJversion ^ 
  " on " ^ Compiler.architecture ^
  " under " ^ SMLofNJ.SysInfo.getOSName () ^
  " with basis library for " ^ (!JDKversion) ^

  "\nCopyright (C) 1999 Persimmon IT Inc.\n\
  \\nMLj comes with ABSOLUTELY NO WARRANTY. It is free software, and you are\
  \\nwelcome to redistribute it under certain conditions.\
  \\nSee COPYING for details.\
  \\n"
(*
  \\nCompiled by " ^ compilerName ^ " " ^ versionToString compilerVersion ^
  " on " ^ !compileTime ^
  "\nBasis library for " ^ (!JDKversion) ^ "\n"
*)

end
