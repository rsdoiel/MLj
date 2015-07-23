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

structure Make :> MAKE =
struct

  val basisStrs = ref
    ["Basis",
     "Array", "Array2",
     "BoolVector", "BoolArray", 
     "CharVector", "CharArray",
     "CommandLine",
     "Date",
     "IntVector", "IntArray",
     "Int64Vector", "Int64Array",
     "LargeReal", "LargeInt",
     "ListPair", "Math", "OS", "PackRealBig",
     "RealVector", "RealArray",
     "Real64",
     "Time", "Timer",
     "Vector", 
     "WideChar", "WideString", "WideSubstring", "WideCharVector",
     "WordVector", "WordArray",
     "Word8Vector", "Word8Array",
     "Word64Vector", "Word64Array"]

  val opts = ref 
    ["presimplify", "simplify", 
     "fullarity", 
     "condsimplify", "equality", 
     "simplify", "monomorphise", 
     "simplify", "share", "flatten",
     "simplify", "fullarity", "tailrec", 
     "simplify", "finallocalise", "finalsimplify"]

  fun complete (projname, entities, names) =
    let
(*......................................................................*)
(* Merge all modules into one huge term and simplify it.		*)
(*......................................................................*)
      val (e,supply,names) = 
        PrintManager.process ("Linking modules", true)
          (fn () => Link.link ((Entity.Str, Ids.symbol "_export")::entities,
            names))
      val _ = CompileReps.classnames := names
    in
      PrintManager.process ("Compiling whole program", true)
      (fn () =>
      let

      val (e,supply) = ApplyOpt.applyOpts (!opts) Var.Map.empty (e,supply)

(*......................................................................*)
(* Turn the whole expression into a bunch of basic blocks in classes    *)
(*......................................................................*)
      val _ = (
        Variables.be_naive := Controls.isOn "naiveCode";
        Variables.do_peephole := Controls.isOn "peephole";
        Variables.symantec_bug := Controls.isOn "SymantecBug";
        Variables.local_merge_strategy :=
          (if Controls.isOn "mergeAllLocals"
          then Variables.ALWAYS
          else if Controls.isOn "mergeSomeLocals"
          then Variables.SAMETYPE
          else Variables.NEVER))

      val convresult = PrintManager.process ("Closure converting", false)
        (fn () => ClosConv.conv e)

      val _ = if Controls.isOn "showClosInfo"
              then ClosConvPretty.dump convresult
              else ()      
      val _ = if Controls.isOn "checkTypes"
              then ClosConvCheck.check convresult
              else ()
    in
      if true (* CheckExn.check (#clinit convresult) *)
      then
      (PrintManager.process ("Generating code", false)
        (fn () => CompileAll.compile projname convresult); true)
      else 
      (PrintManager.print "Error: exception always raised at top-level.\n";
      false)
    end)
    end

  val exports = ref ([] : (string list * string) list)
  val mainclass = ref (NONE : string option)

  fun getDeps () = 
  case !exports of
    es as ((rootstrid::_,_)::_) =>
    (case DepManager.sync (map (hd o #1) es) of
      NONE => 
      NONE

    | SOME info =>
      if not (SMLClassDefOps.checkExplicit 
         (map Ids.symbol ["java", "lang", "Object"]))
      then (print 
        "Cannot find Object class. Have you set classpath correctly?\n"; NONE)
      else SOME (rootstrid, info))

  | _ => 
    (print "Exports not set.\n"; NONE)

  fun make () = 
  (let
    fun finish success = (Debug.finish (); success)
    val timer = Timer.startCPUTimer ()
    val _ = mainclass := NONE
  in
    Counters.setup ();
    Debug.start ();
    Debug.print (Version.welcomeMessage ());
    case getDeps () of
      NONE => finish false
    | SOME (rootstrid, info as { order, ... }) =>
      if SepComp.make info 
      then
        case ProcessExports.process (!exports) of
          NONE => 
          finish false

        | SOME (tynames, mainclassopt) =>
          let           
            val _ = mainclass := mainclassopt
            val done = complete (rootstrid, order, tynames)
          in            
            if Controls.isOn "showTime" 
            then (PrintManager.printTime "\nTotal compilation time: " timer;
              PrintManager.print "\n")
            else PrintManager.print "\n";
            if Controls.isOn "listFiles" 
            then 
            let
              val f = TextIO.openOut ("files")
              val _ = TextIO.output (f, 
                Pretty.simpleVec "\n" Gen.id
                (rev (List.mapPartial SourceManager.fileFor order)))
              val _ = TextIO.closeOut f
            in
              ()
            end else ();
            finish done
          end
      else finish false
  end) handle e => (Debug.finish(); raise e)

  fun makeBasis strids = 
    (
      Counters.setup ();
      Debug.start ();
      (let val done = 
        case DepManager.sync strids of
          NONE => 
          false

        | SOME info =>
          SepComp.make info
      in
        Debug.finish (); done
      end) handle e => (Debug.finish(); raise e)
    )

  fun makeAndFreezeBasis (isJava11, log) =
  (
    Debug.log := log;
    Version.hasBigIntegers := isJava11;
    if isJava11
    then
    (
      Version.JDKversion := "JDK 1.1";
      SourceManager.strExts := ["sml.jdk1.1.1", "sml"];
      SourceManager.sigExts := ["sig.jdk1.1.1", "sig"]
    )
    else
    (
      Version.JDKversion := "JDK 1.0";
      SourceManager.strExts := ["sml.jdk1.0.2", "sml"];
      SourceManager.sigExts := ["sig.jdk1.0.2", "sig"]
    );
    if makeBasis (!basisStrs)
    then
    (
      Version.basisTime := Date.toString (Date.fromTimeLocal (Time.now ()));
      SourceManager.freezeBasis ();
      ParseManager.freezeAll ();
      SMLClassDefOps.cache := Symbol.OrdMap.empty;
      PackageManager.setBootClassPath [];
      Debug.log := "";
      SourceManager.strExts := ["sml"];
      SourceManager.sigExts := ["sig"];
      true
    ) else false
  )
 
  fun getMainClass () = !mainclass

end