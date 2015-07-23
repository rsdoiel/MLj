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
(* Top-level loop							*)
(*======================================================================*)
structure TopLevel :> TOPLEVEL =
struct

open Command

(*----------------------------------------------------------------------*)
(* Message printed when control-C is typed.				*)
(*----------------------------------------------------------------------*)
val intMessage = "\nInterrupt\n"

(*----------------------------------------------------------------------*)
(* Prompt for command entry.						*)
(*----------------------------------------------------------------------*)
val prompt = "\\ "

(*----------------------------------------------------------------------*)
(* Message printed when "help" or "?" is entered.			*)
(*----------------------------------------------------------------------*)
val helpMessage =
"Commands:\n\
\  bootclasspath [+] <directory> ... <directory> [+]\n\
\  cd <directory>\n\
\  classpath [+] <directory> ... <directory> [+]\n\
\  export [+] <classty> <classname>, ..., <classty> <classname> [+]\n\
\  help [<command>]\n\
\  help structure <strid>\n\
\  help signature <sigid>\n\
\  help class <classid> [+]\n\
\  jvm <OS command>\n\
\  log [<file>]\n\
\  make [<classty> <classname>, ..., <classty> <classname>]\n\
\  off <switch>\n\
\  on <switch>\n\
\  quit\n\
\  run [<arg> ... <arg>]\n\
\  save <commandfile>\n\
\  signaturefrom [+] <sigid> <file>, ..., <sigid> <file> [+]\n\
\  sourcepath [+] <directory> ... <directory> [+]\n\
\  structurefrom [+] <strid> <file>, ..., <strid> <file> [+]\n\
\  target <file>\n\
\  \"<OS command>\"\n"

val commandHelp =
foldl (fn ((command, help), m) => Map.insert(m, command, help)) Map.empty
[
( "help",
  "help\n\
  \  List commands available\n\
  \help <command>\n\
  \  Display more detailed help on a particular command\n\
  \help structure <strid>\n\
  \  Display the value, type and substructure bindings of a compiled structure\n\
  \help signature <sigid>\n\
  \  Display a compiled signature\n\
  \help class <classid>\n\
  \  Display the signature for an external Java class\n\
  \help class <classid> +\n\
  \  Display the signature for an external Java class with inherited members"),
( "make",
  "make\n\
  \  Compile and link current project\n\
  \make <classty> <classname>, ..., <classty> <classname>\n\
  \  Set exports and compile and link"),
( "export",
  "export [+] <classty> <classname>, ..., <classty> <classname> [+]\n\
  \  Set actual name of exported class type <classty> to be <classname>"),
( "quit",
  "quit\n\
  \  End MLJ session and return to command line"),
( "sourcepath",
  "sourcepath <directory> ... <directory>\n\
  \  set path for source file searching\n\
  \sourcepath + <directory> ... <directory>\n\
  \  append path for source file searching\n\
  \sourcepath <directory> ... <directory> +\n\
  \  prepend path for source file searching\n\
  \sourcepath?\n\
  \  query current path"),
( "cd", 
  "cd <directory>\n\
  \  change current directory\n\
  \cd?\n\
  \  query current directory"),
( "classpath",
  "classpath <directory> ... <directory>\n\
  \  set path for external class file searching\n\
  \classpath + <directory> ... <directory>\n\
  \  append path for external class file searching\n\
  \classpath <directory> ... <directory> +\n\
  \  prepend path for external class file searching\n\
  \classpath?\n\
  \  query current class path"),
( "target",
  "target <file>\n\
  \  change the filename used for compiled code\n\
  \target\n\
  \  return target to default setting (first structure in exports list)\n\
  \target?\n\
  \  query current target filename"),
( "log",
  "log [<file>]\n\
  \  change the filename used for the log (default is no log)\n\
  \log?\n\
  \  query current log filename"),
( "run",
  "run [<arg> ... <arg>]\n\
  \  run the current project with (optional) arguments specified passed to main"),
( "structurefrom",
  "structurefrom [+] <strid> <file>, ..., <strid> <file> [+]\n\
  \  set a mapping between SML structure identifiers and actual filenames\n\
  \structurefrom?\n\
  \  query current mapping"),
( "signaturefrom",
  "signaturefrom [+] <sigid> <file>, ..., <sigid> <file> [+]\n\
  \  set a mapping between SML signature identifiers and actual filenames\n\
  \signaturefrom?\n\
  \  query current mapping"),
( "on",
  "on exnlocs\n\
  \  turn uncaught exception location information on (default: off)\n\
  \on valuewarning\n\
  \  turn non-generalised type variables warnings on (default: on)"),
( "off",
  "off exnlocs\n\
  \  turn uncaught exception location information off (default: off)\n\
  \off valuewarning\n\
  \  turn non-generalised type variables warnings off (default: on)")
]

(*----------------------------------------------------------------------*)
(* Message printed when there's a syntax error in a command.		*)
(*----------------------------------------------------------------------*)
val miniHelpMessage = 
  "Commands: " ^ Pretty.simpleVec "," 
  #1 (Map.listItemsi commandHelp) ^ "\n"

fun setArg (arg, SOME [s]) = 
    (arg := s; true)

  | setArg (arg, NONE) =
    (print (!arg ^ "\n"); true)

  | setArg (arg, SOME []) =
    (print "Too few arguments\n"; false)

  | setArg (arg, SOME _) =
    (print "Too many arguments\n"; false)

(*----------------------------------------------------------------------*)
(* Do syntatic checks on list of exports of form          		*)
(*   classty classname, ..., classty classname                          *)
(* ensuring that:                                                       *)
(*   (1) classty is a qualified type constructor. Return Syntax.longid. *)
(*   (2) classnames are distinct.                                       *)
(*----------------------------------------------------------------------*)
fun parseExports extopt args =
let
  fun gather [] = SOME []
    | gather ((classty,classname)::rest) =
      case String.fields (fn c => c = #".") classty of
        longid as (_::_) => 
        (case gather rest of
          NONE => NONE
        | SOME result => 
          SOME ((longid,if classname="" then List.last longid else classname)
            ::result))

  fun removeDupLongids ([], result) = rev result
    | removeDupLongids ((longid,name)::rest, result) = 
      if List.exists (fn (longid',_) => longid=longid') result
      then removeDupLongids (rest, result)
      else removeDupLongids (rest, (longid,name)::result)
in
  case gather args of
    NONE => NONE
  | SOME newexports =>
    let
      val exports = removeDupLongids ((case extopt of 
        NONE => newexports
      | SOME Prefix => newexports @ !Make.exports
      | SOME Postfix => !Make.exports @ newexports), [])
    in
      case Dups.duplicateStrings (map #2 exports) of
        [] => SOME exports
      | ds =>
        (print ("Duplicate class names: " ^ Pretty.simpleVec "," 
         Gen.id ds ^ "\n"); NONE)
    end
end

(*----------------------------------------------------------------------*)
(* Print a class defn, first removing private fields & methods and the	*)
(* <clinit> method. Also sort by name/no. of args.                      *)
(*----------------------------------------------------------------------*)
fun printClass (class, (classinfo, fields, methods)) =
let
  val fields = 
    List.filter (fn (_,mods,_,_) => 
      not (List.exists (fn m => m=Field.PRIVATE) mods)) fields

  val methods = 
    List.filter (fn (name,mods,_,_) => not (JavaString.equal(name,
      JavaString.fromString "<clinit>")) andalso 
      not (List.exists (fn m => m=Method.PRIVATE) mods)) methods

  val fields = 
    QuickSort.sort (fn ((name1,_,_,_),(name2,_,_,_)) => 
    JavaString.compare(name1, name2) = LESS) fields

  val methods = 
    QuickSort.sort (fn ((name1,_,args1,_),(name2,_,args2,_)) => 
      if JavaString.equal(name1,name2) then length args1 < length args2
      else JavaString.compare(name1,name2) = LESS) methods

  val cd = (class, (classinfo, fields, methods))
in
  print (EnvOps.classDefToString cd)
end

val saved =
  [Command(SourcePath, NONE, NONE), 
   Command(ClassPath, NONE, NONE), 
   Command(Target, NONE, NONE),
   Map(Export, NONE, NONE),
   Map(StructureFrom, NONE, NONE), 
   Map(SignatureFrom, NONE, NONE)]

(*----------------------------------------------------------------------*)
(* Interpret a single command. Return true if successfully completed.	*)
(* Note that "quit" never completes.                                    *)
(*----------------------------------------------------------------------*)
fun interpretCommand print command =
let
  fun extArgs (desc, rd, wr, extopt, SOME dirs) = 
      wr (case extopt of
        NONE => dirs
      | SOME Prefix => dirs @ rd ()
      | SOME Postfix => rd () @ dirs)

    | extArgs (desc, rd, wr, extopt, NONE) =
      print (desc ^ " " ^ Pretty.simpleVec " " Gen.id (rd ()) ^ "\n")
in

case command of
  Map(Make, NONE, SOME []) => 
  Make.make ()

| Map(Make, extopt, SOME args) =>
  (if interpretCommand print (Map(Export, extopt, SOME args))
   then Make.make ()
   else false)

| Map(Export, extopt, SOME (args as (_::_))) => 
  (case parseExports extopt args of
    NONE => false
  | SOME result =>
    (Make.exports := result; true)
  )

| Map(Make, NONE, NONE) =>
  (print "Query not applicable.\n"; false)

| Map(Export, NONE, SOME []) =>
  (print "No exported classes specified.\n"; false)

| Map(Export, NONE, NONE) =>
  (print ("export " ^ Pretty.simpleVec "," (fn (longid,classname) =>
    Pretty.longidToString (map Ids.symbol longid) ^ " " ^ classname) 
    (!Make.exports) ^ "\n"); 
  true)

| Command(Transforms, extopt, args) => 
  (extArgs ("transforms", fn () => !Make.opts,  
    fn n => Make.opts := n, extopt, args); true)

| Command(LocalTransforms, extopt, args) => 
  (extArgs ("localtransforms", fn () => !SepComp.opts,
    fn n => SepComp.opts := n, extopt, args); true)

| Command(Quit, NONE, SOME []) => 
  false

| Command(ChDir, NONE, SOME [s]) =>  
  ((OS.FileSys.chDir s; true) 
  handle OS.SysErr (s, _) => (print (s ^ "\n"); false))

| Command(ChDir, NONE, NONE) =>
  (print (OS.FileSys.getDir () ^ "\n"); true)

| Command(Exec, NONE, SOME [s]) =>
  OS.Process.system s = OS.Process.success

| Command(JVM, NONE, NONE) =>
  (print (!EnvVars.javaCommand ^ "\n"); true)

| Command(Java10, NONE, SOME []) =>
  (true)

| Command(Java11, NONE, SOME []) =>
  (true)

| Command(JVM, NONE, SOME args) =>
  (EnvVars.javaCommand := Pretty.simpleVec " " Gen.id args; true)

| Command(Run, NONE, SOME ss) =>
  (case Make.getMainClass () of
    SOME class =>
    OS.Process.system 
      (!EnvVars.javaCommand ^ " " ^ 
    Path.listToPath(TargetManager.getZipName (hd (#1 (hd (!Make.exports)))) :: 
        PackageManager.getClassPath ()) ^ " " ^ class ^ " " ^
        (Pretty.simpleVec " " Gen.id ss)) = OS.Process.success

  | NONE => (print "No main method in any exported class\n"; false)
  )

| Command(On, NONE, SOME [flag]) =>
  (case Controls.lookup flag of
    NONE => (print "No such switch.\n"; false)
  | SOME b => (b := true; true)
  )

| Command(Off, NONE, SOME [flag]) =>
  (case Controls.lookup flag of
    NONE => (print "No such switch.\n"; false)
  | SOME b => (b := false; true)
  )

| Command(Query, NONE, SOME [flag]) =>
  (case Controls.lookup flag of
    NONE => (print "No such switch.\n"; false)
  | SOME b => (if !b then print "on\n" else print "off\n"; true)
  )

| UserCommand name =>
  let
    val name = OS.Path.joinBaseExt { base = name, ext = SOME "mlj" } 
    val result = 
          (let
            val file = TextIO.openIn name
            val s = (TextIO.inputAll file ^ "\n") 
              handle e => (TextIO.closeIn file; raise e)
            val _ = TextIO.closeIn file
          in
            case ParseCommand.parse s of
              ParseCommand.Failure s =>
              ParseCommand.Failure ("Error in file " ^ name ^ ": " ^ s)

            | other => other
          end) 
          handle IO.Io _ => ParseCommand.Failure ("Cannot open file " ^ name)

    fun loop' [] = true
      | loop' (command::commands) = 
        if interpretCommand print command 
        then loop' commands
        else false

  in
    case result of
      ParseCommand.Failure s =>
      (print (s ^ "\n"); false)

    | ParseCommand.Success commands =>
      (print ("[Interpreting commands in file " ^ name ^ "...]\n"); 
      loop' commands)
  end
    
| Command(ClassPath, ext, dirs) => 
  (extArgs("classpath", PackageManager.getClassPath, 
    ignore o PackageManager.setClassPath, ext, dirs); true)

| Command(BootClassPath, ext, dirs) => 
  (extArgs("bootclasspath", PackageManager.getBootClassPath, 
    ignore o PackageManager.setBootClassPath, ext, dirs); true)

| Command(SourcePath, ext, dirs) => 
  (extArgs("sourcepath", fn () => !SourceManager.projPath, 
    fn n => SourceManager.projPath := n, ext, dirs); true)

| Command(Help, NONE, SOME []) => 
  (print helpMessage; true)

| Command(Help, NONE, SOME [command]) =>
  ((case Map.find(commandHelp, String.map Char.toLower command) of
    NONE => print "Help not available: no such command\n"
  | SOME s => print (s ^ "\n")); true)

| Command(Help, NONE, SOME ["structure", strid]) =>
  (case SepComp.getE (Entity.Str, Ids.symbol strid) of
    SOME E =>
    (print ("structure " ^ strid ^ " : " ^ EnvOps.EasSig E); true)

  | NONE => 
    (print "Structure not defined\n"; false)
  )

| Command(Help, NONE, SOME ["signature", sigid]) =>
  (case SepComp.getE (Entity.Sig, Ids.symbol sigid) of
      SOME E =>
      (print ("signature " ^ sigid ^ " = " ^ EnvOps.EasSig E); true)
  
    | NONE => 
      (print "Signature not defined\n"; false)
  )

| Command(Help, NONE, SOME ["class", classid]) =>
  let
    val longid = map Ids.symbol (String.tokens (fn c => c= #".") classid)
  in
    if not( SMLClassDefOps.checkExplicit longid)
    then (print "No such class.\n"; false)
    else
      let    
        val ct = SMLTy.baseType (TyName.externalClass longid)
        val cd = SMLClassDefOps.tyToClassDef TyName.Map.empty ct
      in
        (printClass (ct,cd); true)
      end
  end

| Command(Help, SOME _, SOME ["class", classid]) =>
  let
    val longid = map Ids.symbol (String.tokens (fn c => c= #".") classid)
  in
    if not( SMLClassDefOps.checkExplicit longid)
    then (print "No such class.\n"; false)
    else
      let
        val classty = SMLTy.baseType (TyName.externalClass longid)
        val (classinfo,_,_) =   
          SMLClassDefOps.tyToClassDef TyName.Map.empty classty
        val (fields,methods) = 
          SMLJavaOps.getInherited TyName.Map.empty
            (classty,true,fn _ => true,fn _ => true)

        val cr = (classty, (classinfo, map #2 fields, map #2 methods))
      in
        (printClass cr; true)
      end
  end

| Command(Target, NONE, NONE) =>
  (case (!Make.exports, !TargetManager.target) of
    ([], "") => 
    (print ("No target set.\n"); false)

  | (((strid::_,_)::_),_) => 
    (print ("target " ^ TargetManager.getZipName strid ^ "\n"); true)

  | ([], _) => 
    (print ("target " ^ TargetManager.getZipName "" ^ "\n"); true))

| Command(Target, NONE, SOME [arg]) =>
  (TargetManager.target := arg; true)

| Command(Target, NONE, SOME []) =>
  (TargetManager.target := ""; true)

| Command(Log, NONE, SOME []) =>
  (Debug.log := ""; true)

| Command(Log, NONE, SOME [arg]) =>
  (Debug.log := arg; true)

| Command(Log, NONE, NONE) =>
  (print (!Debug.log ^ "\n"); true)

| Command(Save, NONE, SOME [arg]) =>
  let
    val name = OS.Path.joinBaseExt { base = arg, ext = SOME "mlj" } 
    val f = TextIO.openOut name
  in
    map (interpretCommand (fn s => TextIO.output(f, s))) saved;
    TextIO.closeOut f;
    true
  end

| Command(_, SOME _, _) =>
  (print "Append/prepend not applicable\n"; false)

| Command(_, _, SOME []) =>
  (print "Too few arguments\n"; false)

| Command(_, _, SOME args) =>
  (print "Too many arguments\n"; false)  

| Command(_, _, NONE) =>
  (print "Query not applicable\n"; false)

| Map(c, extopt, pairsopt) =>
  let
    val (kind,desc) = 
      case c of 
        StructureFrom => (Entity.Str, "structurefrom")
      | SignatureFrom => (Entity.Sig, "signaturefrom")
      | _ => Debug.fail "TopLevel.interpretCommand: invalid map type"
  in
    case pairsopt of
      NONE =>
      (print (desc ^ " " ^ Pretty.simpleVec "," 
      (fn ((kind,id),name) => Pretty.idToString id ^ "=" ^ name) 
      (Entity.Map.listItemsi 
        (Entity.Map.filteri (fn ((kind',_),_) => kind=kind') 
          (!SourceManager.translation))) ^ "\n"); true)

    | SOME pairs =>
    let
      val newmap = 
      foldl 
        (fn ((id, name), t) => Entity.Map.insert(t, (kind,Ids.symbol id), name))
        (if isSome extopt then Entity.Map.empty
         else Entity.Map.filteri (fn ((kind',_),_) => kind<>kind') 
           (!SourceManager.translation)) pairs
    in
      SourceManager.translation :=
      (case extopt of
        NONE => 
        newmap
  
      | SOME Prefix =>
        Entity.Map.unionWith #2 (!SourceManager.translation, newmap)

      | SOME Postfix =>
        Entity.Map.unionWith #1 (!SourceManager.translation, newmap));
      true
    end
  end

end
  handle e => (print ("\n!!! COMPILER BUG: " ^ exnMessage e ^ " raised at "
    ^ Pretty.simpleVec "/" Gen.id (SMLofNJ.exnHistory e) ^ "\n"); false)

(*----------------------------------------------------------------------*)
(* Top level interactive loop						*)
(*----------------------------------------------------------------------*)
fun topLoop () =
let
  val _ = print (Version.welcomeMessage ())
  val oldaction = Signals.inqHandler Signals.sigINT
  val _ = 
  SMLofNJ.Cont.callcc (fn k => ignore (Signals.setHandler (Signals.sigINT, 
      Signals.HANDLER (fn (signal, n, k') => 
      (print intMessage; k)))))

  fun loop () =
  let
    val _ = PrintManager.restart ()
    val _ = print prompt
    val line = TextIO.inputLine TextIO.stdIn
    fun quit () = (Signals.setHandler (Signals.sigINT, oldaction); ())
  in
    if line="" then quit ()
    else
    let
      val result = ParseCommand.parse line
    in
      case result of
        ParseCommand.Failure s =>
        (print ("Error: " ^ s ^ "\n" ^ miniHelpMessage); loop ())

      | ParseCommand.Success commands =>
        let
          fun loop' [] = 
              loop ()

            | loop' (Command(Quit, NONE, SOME [])::_) = 
              quit ()

            | loop' (command::commands) = 
              if interpretCommand print command 
              then loop' commands 
              else loop ()
        in
          loop' commands
        end
    end
  end
in
  loop ()
end

(*----------------------------------------------------------------------*)
(* Entry point for command-line compiler				*)
(*----------------------------------------------------------------------*)
fun main (name : string, args) =
let
  val _ = SMLofNJ.Internals.GC.messages false
 
  val _ = SourceManager.projPath := ["."];
  val success = EnvVars.getVars ()

  val line = String.concat (map (fn x => x ^ " ") args) ^ "\n"
in
  if not success then OS.Process.failure
  else 
  case ParseCommand.parse line of
    ParseCommand.Failure s =>
    (print ("Error: " ^ s ^ "\n"); OS.Process.failure)

  | ParseCommand.Success commands =>
    let
      fun loop' [] = 
          (topLoop (); OS.Process.success)

        | loop' (Command(Quit, NONE, SOME [])::_) = 
          OS.Process.success

        | loop' (command::commands) = 
          if interpretCommand print command then loop' commands 
          else OS.Process.failure
    in
      loop' commands
    end
end


end

fun m () = TopLevel.main("",[]);

