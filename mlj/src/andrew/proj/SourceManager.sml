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
(* Source file manager.  						*)
(* See signature for more details.                                      *)
(*======================================================================*)
structure SourceManager :> SOURCEMANAGER =
struct

(*----------------------------------------------------------------------*)
(* If set, then the basis will not be checked for updates.		*)
(*----------------------------------------------------------------------*)
val basisFrozen = ref false

(*----------------------------------------------------------------------*)
(* Path for basis and project source files.				*)
(*----------------------------------------------------------------------*)
val basisPath = ref (["basis"] : string list)
val projPath  = ref ([] : string list)

(*----------------------------------------------------------------------*)
(* Extensions for the three kinds of SML entity + Java classes       	*)
(*----------------------------------------------------------------------*)
val sigExts = ref ["sig.jdk1.1.1", "sig"]
val strExts = ref ["sml.jdk1.1.1", "sml"]
val funExts = ref ["fun", "sml"]
val funsigExts = ref ["sig"]

(*----------------------------------------------------------------------*)
(* What is the extension for this entity type?   			*)
(*----------------------------------------------------------------------*)
fun typeToExts etype =
case etype of
  Entity.Sig => !sigExts
| Entity.Str => !strExts
| Entity.Fun => !funExts
| Entity.FunSig => !funsigExts

(*----------------------------------------------------------------------*)
(* Entity translation       						*)
(*----------------------------------------------------------------------*)
val translation = ref (Entity.Map.empty : string Entity.Map.map) 

(*----------------------------------------------------------------------*)
(* Type of datestamp information for directories and zip files.		*)
(*----------------------------------------------------------------------*)
datatype LibInfo = Dir of Time.time Map.map
withtype DirInfo = string * LibInfo

(*----------------------------------------------------------------------*)
(* Datestamp info for the basis and project paths.			*)
(* Absolute file names (from translation) are maintained separately     *)
(*----------------------------------------------------------------------*)
val basisdirinfo = ref ([] : DirInfo list)
val projdirinfo = ref ([] : DirInfo list)
val absfileinfo = ref (Map.empty : Time.time Map.map)

(*----------------------------------------------------------------------*)
(* Freeze basis ready for binary distribution				*)
(*----------------------------------------------------------------------*)
fun freezeBasis () = 
(basisPath := [];
 absfileinfo := Map.empty;
 basisFrozen := true)

fun isFrozen () = !basisFrozen

(*----------------------------------------------------------------------*)
(* Obtain timestamp of file if possible without causing IO error.	*)
(* There's a problem on Windows 95 (and NT + samba?): time not          *)
(* available if file is open.                                           *)
(*----------------------------------------------------------------------*)
fun getTime name = (OS.FileSys.modTime name) 
  handle OS.SysErr(s,_) => 
  (print ("\n[Warning: " ^ s ^ " for " ^ name ^ "] "); Time.now ())

fun moreRecent (time1, time2) = Time.> (time1, time2)

(*----------------------------------------------------------------------*)
(* Gather datestamp information about a particular path.		*)
(*----------------------------------------------------------------------*)
fun getStampsForPath (path, description) =
let

fun listDir ([], result) = 
    SOME (rev result)

  | listDir (name::names, result) =
    if OS.FileSys.access(name, [])
    then
      if OS.FileSys.isDir name
      then   
      let
        val dirstream = OS.FileSys.openDir name
        fun make result =
          case OS.FileSys.readDir dirstream of
            "" => 
            result

          | s => 
            let
              val fname = OS.Path.joinDirFile { dir = name, file = s }
            in
              if not (OS.FileSys.access(fname, [])) orelse OS.FileSys.isDir fname
              then make result
              else make (Map.insert(result, s, getTime fname))
            end

        val d = (make Map.empty) handle e => 
                (OS.FileSys.closeDir dirstream; raise e)
      in
        OS.FileSys.closeDir dirstream;
        listDir (names, (name, Dir d) :: result)
      end    
      else Debug.fail "file not directory"
    else
    (
      PrintManager.print
        ("\nDirectory " ^ name ^ " in " ^ description ^ " does not exist\n");
      NONE
    )
in
  listDir (path, [])
end

(*----------------------------------------------------------------------*)
(* Gather datestamps for the absolute files in the entity map		*)
(*----------------------------------------------------------------------*)
fun getStampsForFiles () =
let
  fun gather (filename, result) =
    if OS.Path.isAbsolute filename
    then Map.insert(result, filename, getTime filename)
    else result
in
  absfileinfo := Entity.Map.foldl gather Map.empty (!translation)
end


(*----------------------------------------------------------------------*)
(* Update the datestamp info.						*)
(*----------------------------------------------------------------------*)
fun sync () =
  PrintManager.process ("Checking timestamps on source files...", true)
  (fn () =>
    (getStampsForFiles ();
    (case (if !basisFrozen then SOME (!basisdirinfo)
            else getStampsForPath (!basisPath, "basis path")) of
        NONE => false
      | SOME b =>
        (basisdirinfo := b;
        case getStampsForPath (!projPath, "sourcepath") of
          NONE => false
        | SOME p => 
          (projdirinfo := p; true))))
    handle 
      OS.SysErr(s,_) => 
      (PrintManager.print ("\n" ^ s); false)

    | IO.Io { name, function, cause } =>
      (PrintManager.print ("\n" ^ exnMessage cause ^ " in " ^ name ^ "\n"); 
       false)
  )

(*----------------------------------------------------------------------*)
(* Read a text file / zip archived file to produce a string		*)
(*----------------------------------------------------------------------*)
fun readText (name, time) =
    let val f = TextIO.openIn name
        val str = (TextIO.inputAll f) handle e => (TextIO.closeIn f; raise e)
    in
      (TextIO.closeIn f; str)
    end

(*----------------------------------------------------------------------*)
(* Try each directory/archive in the list for a filename		*)
(*----------------------------------------------------------------------*)
fun find [] filenames = NONE
  | find ((name,libinfo)::dirs) filenames =
    case libinfo of
      Dir times =>
      let
        fun find' [] = 
            find dirs filenames

          | find' (filename::rest) = 
            let 
              val fullname = OS.Path.joinDirFile {dir = name,file = filename}
            in
              case Map.find(times, filename) of
                NONE => 
                ((SOME (fullname, OS.FileSys.modTime fullname))
                handle OS.SysErr _ => find' rest)

              | SOME time =>
                let 
                  val fullname = OS.Path.joinDirFile {dir = name,file = filename}
                in
                  SOME (fullname, time)
                end
            end
      in
        find' filenames
      end

(*----------------------------------------------------------------------*)
(* Find a file for this entity reference.       			*)
(*----------------------------------------------------------------------*)
fun refToFile (entity as (etype, id)) =
let
  (*..................................................................*)
  (* Apply the translation to get a single filename or add the        *)
  (* appropriate extensions to the entity name given.                 *)
  (*..................................................................*)
  val filenames = 
    case Entity.Map.find(!translation, entity) of
      NONE => 
      map (fn ext => OS.Path.joinBaseExt 
        { base = valOf(JavaString.toString (Symbol.toJavaString id)), 
          ext = SOME ext }) 
        (typeToExts etype)

    | SOME name => [name]

  (*..................................................................*)
  (* Look first in basisPath and then in projPath.|                    *)
  (*..................................................................*)
  val result = 
      case find (!basisdirinfo) filenames of
        SOME result =>
        SOME (result, true)

      | NONE => 
        case find (!projdirinfo) filenames of
          SOME result =>
          SOME (result, false)

        | NONE => NONE
in
  result
end

fun fileFor entityref =
  case refToFile entityref of
    SOME ((sourcefilename, sourcetime), isbasis) => 
    if isbasis then NONE else SOME sourcefilename

  | _ => 
    NONE

datatype Result =
  Changed of string * bool * Entity.FileRef option
| Failed
| Unchanged

(*----------------------------------------------------------------------*)
(* Has the source to an entity with timestamp targettime been updated?	*)
(*----------------------------------------------------------------------*)
fun load (entityref, targetfilerefopt) =
  case refToFile entityref of
    NONE => 
    Failed

  | SOME (sourcefileref, isbasis) => 
    case targetfilerefopt of
      NONE => 
      Changed (readText sourcefileref, isbasis, SOME sourcefileref)

    | SOME targetfileref => 
      if EntityOps.>(sourcefileref, targetfileref) 
      then Changed (readText sourcefileref, isbasis, SOME sourcefileref)
      else Unchanged


end

