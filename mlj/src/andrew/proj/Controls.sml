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
(* Global controls for the compiler					*)
(*======================================================================*)
structure Controls :> CONTROLS =
struct

fun makeFlags (yes,no) = 
foldr (fn (flag, m) => Map.insert(m, flag, ref true))
(foldr (fn (flag, m) => Map.insert(m, flag, ref false)) Map.empty no) yes

val flags = makeFlags
(
[
(*......................................................................*)
(* Essential rewrites							*)
(*......................................................................*)
(* cc's *)
  "caseCC", "condCC", "tryCC", "letFunCC", 

(* beta's *)
  "letVal", "tryThrow", 
  "prodBeta", "arrowBeta", "caseBeta", "condBeta", "bindBeta",
  "forallBeta", "inlineEq", "caseBind", "unfoldFold",

(* eta's *)
  "prodEta", "forallEta", "arrowEta", "letEta", "letEta2", "caseEta",

(* branch introduction *)
  "branchBlock",
        
(* eliding *)
  "coerceId", "coerceComp", "constOp", "elideEncap", "elideCont",
  "javaField",

(* dead code *)
  "deadTry", "deadBind", "deadBind2", "deadLetRec", "deadLet", "deadLet2",
  "deadLetFun", "deadEncap", "deadHandler",

(*......................................................................*)
(* Non-essential rewrites						*)
(*......................................................................*)
  "condCase", "commonVal", 

(* iso's *)
  "argArity", "resultArity", "uncurry",

(* flattening *)
  "flattenCon", "unitArg", "unitResult", "unitProd", "unitCon", "unitExCon",
  "flattenArg",

(* Function optimisation and binding hoisting *)
  "knownFunctions", "localFunctions", "hoistLocalFunctions",
  "inline", "hoistVal", "floatVal",

(* Monomorphisation and equality compilation *)
  "monoShare",

(* Globalisation *)
  "globalRef", "globalVal", 

(* Closure conversion *)
  "tailRec", "tailCons", "tailConsAlt",

(*......................................................................*)
(* Compiler warnings							*)
(*......................................................................*)
  "valueWarning",
  "deprecatedMLj",
  "deprecatedJava",
  "downcastWarning",

(*......................................................................*)
(* Basic block compilation						*)
(*......................................................................*)
  "peephole",
  "mergeAllLocals",
  "mergeSomeLocals",
  "mergeHandlers",

  "shareClosures"],
 [
(*......................................................................*)
(* Diagnostics								*)
(*......................................................................*)

  "showClassImport",
  "showFileRefs",
  "showRewrites",
  "showSingleRewrites",
  "showStamps",
  "showElab",
  "showBasis",
  "showQuantifiers",
  "showPaths",
  "showPats",
  "showConstOps",
  "showSorts",
  "showIsos",
  "showSpecs",
  "showReps",
  "showCensus",
  "showSimp",
  "showInlining",
  "showEffects",
  "showGlobalFuns",
  "showClosures",
  "showClosInfo",
  "showFVInfo",
  "showFlow",
  "showClosFlow",
  "showSubroutines",
  "showMethods",
  "showTime",
  "showGC",
  "showEq",
  "showHoist",
  "showFloat",
  "showTypedLet",
  "showSMLTerm",
  "showSMLTypes", 
  "showSMLEnv",
  "showMILTypes",
  "showClasses",
  "showGlobalRefs",
  "defaultsWarning",
  "verbose",
  "pointerEq",


(*......................................................................*)
(* MIL type checks							*)
(*......................................................................*)
  "iterateDump",
  "checkCensus",
  "checkTypes",
  "checkCensusI",
  "checkTypesI",
  "checkBlockArgs",

  "hoistLetContinuation", "hoistTryContinuation",
  "listFiles", "globalClos",  "unroll",
  "noneVals",
  "removeRec",
  "letPolyFunCC",

  (* Code generation *)
  "MicrosoftBug", "naiveCode",
  "SymantecBug", "countInits",
  "omitInits",
  "exnLocs",
  "genThrows",
  "fillInStackTrace"

 ]
)

val universal = 
  Set.addList (Set.empty, 
  ["verbose", "exnLocs", "valueWarning", "defaultsWarning", "MicrosoftBug",
   "deprecatedMLj", "deprecatedJava", "SymantecBug",
   "downcastWarning"])

(*----------------------------------------------------------------------*)
(* Fast, case-sensitive lookup for testing a flag.			*)
(*----------------------------------------------------------------------*)
fun isOn flag = 
case Map.find(flags, flag) of
  SOME (ref b) => b
| NONE => Debug.fail ("Controls.isOn: " ^ flag ^ " undefined")

(*----------------------------------------------------------------------*)
(* Slower, case-insensitive lookup (for interactive loop). 		*)
(*----------------------------------------------------------------------*)
fun lookup flag =
let
  fun find [] = NONE
    | find ((flag',r)::rest) =
      if String.collate (fn (c1,c2) => 
        Char.compare(Char.toUpper c1, Char.toUpper c2)) (flag,flag') = EQUAL
      andalso (!Debug.debugOn orelse Set.member(universal, flag'))
      then SOME r else find rest
in
  find (Map.listItemsi flags)
end

end
