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
(* Apply selected optimisations to a MIL term:                          *)
(*    insertion of isomorphisms between types                           *)
(*    sharing of types (ML base types -> Java, sum types -> unisum,     *)
(*      enumeration types -> ints)                                      *)
(*                                                                      *)
(*======================================================================*)
structure ApplyOpt :> APPLYOPT =
struct

val memory = ref ([] : MILTerm.Cmp list)

fun fullArity tyenv ce =
    let
      val ce = 
        GlobalOpt.transform {doInline=true,doHoistFloat=false} tyenv ce
      val _ = 
        if Controls.isOn "iterateDump" 
        then (PrintManager.print "d..."; 
          Debug.print ("\n" ^ MILPretty.cmpToString ce ^ "\n"))
        else ()
      val ce = Arity.transform tyenv ce
    in
      if !Counters.total <> 0 then fullArity tyenv ce else ce
    end

fun tc tyenv e =
  (TypeCheck.check false 
    { tyenv = tyenv, kindenv = Var.Map.empty, globenv = Var.Map.empty,
      funenv = Var.Map.empty } e;
   e)

fun whatOpt opt =
  case opt of
    "arity"        => ("Arity raising functions", Arity.transform)
  | "share"        => ("Sharing data representations", Share.transform)
  | "fullarity"    => ("Optimising", fullArity)
  | "monomorphise" => ("Monomorphising", Monomorphise.transform)

  | "presimplify" =>  ("Pre-simplifying", Simplify.simplify 
    { removeEncaps = false, doInlineEq = false, 
      doComplex = false, doBranches = false })

  | "simplify"     => ("Simplifying", Simplify.simplify 
    { removeEncaps = false, doInlineEq = false, 
      doComplex = true, doBranches = false })

  | "finalsimplify"=> ("Final simplifying", Simplify.simplify
    { removeEncaps = true, doInlineEq = false, 
      doComplex = true, doBranches = true })

  | "condsimplify" => ("Simplifying", Simplify.simplify
    { removeEncaps = false, doInlineEq = true, 
      doComplex = true, doBranches = false })

  | "flatten"      => ("Flattening constructors", Flatten.transform)
  | "equality"     => ("Compiling polymorphic equality", Equality.transform)
  | "tailrec"      => ("Introducing tail recursion", TailRec.transform)
  | "localise"     => ("Inlining and localising functions", 
    GlobalOpt.transform {doInline=true,doHoistFloat=false})
  | "finallocalise"=> ("Localising functions", 
    GlobalOpt.transform {doInline=false,doHoistFloat=true})
  | "dump"         => ("Writing term to log", 
    fn tyenv => fn ce => 
      (Debug.print ("\n" ^ MILPretty.cmpToString ce ^ "\n"); ce))

  | "typecheck"    => ("Type checking term", tc)

  | "census"       => ("Checking census",
    fn tyenv => fn ce => (Census.checkCmp ce; ce))

  | "profile"      => ("Profiling on", 
    fn tyenv => fn ce => (Compiler.Profile.reset (); 
                          Compiler.Profile.setTimingMode true; ce))
  | "noprofile"    => ("Profiling off", 
    fn tyenv => fn ce => (Compiler.Profile.setTimingMode false; 
                          (case !Debug.logFile of 
                            NONE => ()
                          | SOME f => Compiler.Profile.reportAll f); ce))

  | _              => 
    Debug.fail ("ApplyOpt.whatOpt: no such transformation: " ^ opt)

fun isBenign opt = List.exists (fn opt' => opt=opt') 
  ["dump", "dumpBasis", "typecheck", "census"]

fun applyOpt [] tyenv ce = 
    let
      val _ = if Controls.isOn "checkTypes" 
              then ignore (tc tyenv ce)
              else ()
      val _ = if Controls.isOn "checkCensus" 
              then Census.checkCmp ce
              else ()
    in
      ce
    end

  | applyOpt ("on"::flag::opts) tyenv ce =
    (case Controls.lookup flag of
      NONE => applyOpt opts tyenv ce
    | SOME b => (b := true; Counters.setup (); applyOpt opts tyenv ce))

  | applyOpt ("off"::flag::opts) tyenv ce =
    (case Controls.lookup flag of
      NONE => applyOpt opts tyenv ce
    | SOME b => (b := false; Counters.setup (); applyOpt opts tyenv ce))

  | applyOpt ("memo"::opts) tyenv ce =
    (memory := ce :: !memory; applyOpt opts tyenv ce)

  | applyOpt (opt::opts) tyenv ce =
    let
      val (description, function) = whatOpt opt
      val _ = if Controls.isOn "checkTypes" andalso not (isBenign opt)
              then ignore (tc tyenv ce)
              else ()
      val _ = if Controls.isOn "checkCensus" 
              then Census.checkCmp ce
              else ()
      val ce = PrintManager.process (description, false) 
        (fn () => function tyenv ce)
    in 
      applyOpt opts tyenv ce
    end

fun applyOpts opts tyenv (ce,supply) =
  (Census.initCensus (ce, supply);
  let
    val ce = applyOpt opts tyenv ce
  in
    (ce,Census.maxVar ())
  end)
      
end (* of struct *)

