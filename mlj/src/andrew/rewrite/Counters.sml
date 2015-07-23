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
(* Counters and flags for various rewriting operations			*)
(*======================================================================*)
structure Counters =
struct

val showRewrites = ref false

type Flag = string * bool ref * int ref

(*----------------------------------------------------------------------*)
(* Total number of rewrites performed on one pass			*)
(*----------------------------------------------------------------------*)
val total = ref 0
fun init s = (s, ref true, ref 0) : Flag

(*----------------------------------------------------------------------*)
(* If the option is enabled then increment its count.			*)
(*----------------------------------------------------------------------*)
fun enabled ((s, b, n) : Flag) = 
let
  val bval = !b
in
  if bval then 
  let 
    val t = !total 
  in
    n := !n + 1; 
    total := t + 1;
    if !showRewrites
    then PrintManager.print (s ^ " ")
    else 
      if Int.rem(t, 200) = 0 
      then PrintManager.print "." else ();
    bval
  end
  else bval
end

(*----------------------------------------------------------------------*)
(* Just test to see if this is enabled.					*)
(*----------------------------------------------------------------------*)
fun justEnabled ((s, b, n) : Flag) = !b

(*----------------------------------------------------------------------*)
(* Just increment           						*)
(*----------------------------------------------------------------------*)
fun justInc ((s, b, n) : Flag) =
let
  val t = !total
in
    n := !n + 1; 
    total := t + 1;
    if !showRewrites
    then PrintManager.print (s ^ " ")
    else 
      if Int.rem(t, 200) = 0 
      then PrintManager.print "." else ()
end

fun getCount ((s, b, n) : Flag) = !n

val flags as 
[
(* cc's *)
  caseCC, condCC, tryCC, letFunCC, letPolyFunCC,

(* beta's *)
  letVal, tryThrow, 
  prodBeta, arrowBeta, caseBeta, condBeta, bindBeta, 
  forallBeta, inlineEq, condCase, unfoldFold,

(* eta's *)
  prodEta, forallEta, arrowEta, letEta, letEta2, caseEta,
  
(* branch introduction *)
  branchBlock,
    
(* eliding *)
  coerceId, coerceComp, constOp, elideEncap, elideCont,
  commonVal, removeRec, javaField,

(* dead code *)
  deadTry, deadBind, deadBind2, deadLetRec, deadLet, deadLet2, deadLetFun,
  deadEncap, deadHandler,

(* iso's *)
  argArity, resultArity, uncurry,

(* flattening *)
  flattenCon, unitArg, unitResult, unitProd, unitCon, unitExCon,
  flattenArg,

(* Function optimisation *)
  knownFunctions, localFunctions, hoistLocalFunctions, 
  hoistLetContinuation, hoistTryContinuation, inline, unroll,
  hoistVal, floatVal,

(* Tail recursion *)
  tailRec, tailCons,

(* Globalisation *)
  globalRef, globalVal
] =


map init 
[
(* cc's *)
  "caseCC", "condCC", "tryCC", "letFunCC", "letPolyFunCC",

(* beta's *)
  "letVal", "tryThrow", 
  "prodBeta", "arrowBeta", "caseBeta", "condBeta", "bindBeta",
  "forallBeta", "inlineEq", "condCase", "unfoldFold",

(* eta's *)
  "prodEta", "forallEta", "arrowEta", "letEta", "letEta2", "caseEta",
    
(* branch introduction *)
  "branchBlock",
    
(* eliding *)
  "coerceId", "coerceComp", "constOp", "elideEncap", "elideCont", 
  "commonVal", "removeRec", "javaField",

(* dead code *)
  "deadTry", "deadBind", "deadBind2", "deadLetRec", "deadLet", "deadLet2",
  "deadLetFun", "deadEncap", "deadHandler",

(* iso's *)
  "argArity", "resultArity", "uncurry",

(* flattening *)
  "flattenCon", "unitArg", "unitResult", "unitProd", "unitCon", "unitExCon",
  "flattenArg",

(* Function optimisation *)
  "knownFunctions", "localFunctions", "hoistLocalFunctions",  
  "hoistLetContinuation", "hoistTryContinuation", "inline", "unroll",
  "hoistVal", "floatVal",

(* Tail recursion *)
  "tailRec", "tailCons",

(* Globalisation *)
  "globalRef", "globalVal"
]

(*----------------------------------------------------------------------*)
(* Reflect user controls in the flags here				*)
(*----------------------------------------------------------------------*)
fun setup () = 
(app (fn (s,b,n) => b := Controls.isOn s) flags;
 showRewrites := Controls.isOn "showSingleRewrites")

(*----------------------------------------------------------------------*)
(* Print all non-zero counts						*)
(*----------------------------------------------------------------------*)
fun printCounts () = 
  if Controls.isOn "showRewrites"
  then
    (PrintManager.print ("[" ^ Int.toString (!total) ^ "]"); 
    app (fn (s,b,n) => if !b andalso !n > 0 
    then PrintManager.print (s ^ ":" ^ Int.toString (!n) ^ " ") else ()) flags)
  else if Controls.isOn "verbose" then PrintManager.print "/" else ()


(*----------------------------------------------------------------------*)
(* Reset all counts to zero						*)
(*----------------------------------------------------------------------*)
fun reset () = (total := 0; app (fn (s,b,n) => n := 0) flags)

end
