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
(* Java package manager.  						*)
(* See signature for more details.                                      *)
(*======================================================================*)
structure PackageManager :> PACKAGEMANAGER =
struct

(*----------------------------------------------------------------------*)
(* Path for imported classes						*)
(*   (1) for the standard run-time (bootclasspath)                      *)
(*   (2) for user Java stuff (classpath)                                *)
(* For zip archives the paths include the opened archive.               *)
(*----------------------------------------------------------------------*)
val bootClassPath = ref ([] : (string * Zip.zipIn option) list)
val classPath = ref ([] : (string * Zip.zipIn option) list)

(*----------------------------------------------------------------------*)
(* Package structure with locations: either in a zip directory or in    *)
(* an ordinary directory.                                               *)
(*----------------------------------------------------------------------*)
datatype ClassDir = 
  ZipDir of string * Zip.zipIn
| NormalDir of string

datatype Package = Package of Env
withtype Env =
  { 
    classes : ClassDir Symbol.OrdMap.map, 
    packages : Package ref Symbol.OrdMap.map 
  }


val emptyPackage = 
  Package 
  { 
    classes = Symbol.OrdMap.empty, 
    packages = Symbol.OrdMap.empty 
  }

val topPackage = 
  ref emptyPackage

(*----------------------------------------------------------------------*)
(* Pretty-print a package						*)
(*----------------------------------------------------------------------*)
fun packageToString p =
let
  fun ps (depth, Package { classes, packages }) =
  Pretty.newline depth ^ "{" ^ 
  (if Symbol.OrdMap.numItems classes <> 0 then
    Pretty.newline (depth+1) ^
    Pretty.simpleVec (Pretty.newline (depth+1))
      (fn (c,_) => "class " ^ Pretty.idToString c)
      (Symbol.OrdMap.listItemsi classes) 
   else "") ^
  (if Symbol.OrdMap.numItems packages <> 0 then
    Pretty.newline (depth+1) ^
    Pretty.simpleVec (Pretty.newline (depth+1)) 
      (fn (n,p) => "package " ^ Pretty.idToString n
      ^ " = " ^ ps (depth+1, !p)) (Symbol.OrdMap.listItemsi packages)
   else "") ^
  Pretty.newline depth ^ "}"
in
  ps (0, p)
end
  
(*----------------------------------------------------------------------*)
(* We unzip compressed files using a shell command			*)
(*----------------------------------------------------------------------*)
val unzipCommand = ref "unzip -Uqqp"

(*----------------------------------------------------------------------*)
(* Need a hack for Windows systems so that temp dir is used 		*)
(* Should be fixed in the next release of SML/NJ 			*)
(*----------------------------------------------------------------------*)
fun tmpName () =
  if SMLofNJ.SysInfo.getOSKind () = SMLofNJ.SysInfo.UNIX
  then OS.FileSys.tmpName ()
  else 
    case OS.Process.getEnv "TMP" of
      NONE => OS.FileSys.tmpName ()
    | SOME tmp =>
      let
        val dir = OS.FileSys.getDir ()
        val _ = OS.FileSys.chDir tmp
        val result = OS.Path.joinDirFile 
          {dir = tmp, file = OS.FileSys.tmpName ()}
        val _ = OS.FileSys.chDir dir
      in
        result
      end

(*----------------------------------------------------------------------*)
(* Unzip a file using the "unzip" command				*)
(*----------------------------------------------------------------------*)
fun unzip (archive, name) = 
  let
    val tmpfilename = OS.FileSys.tmpName ()
    val command = 
      !unzipCommand ^ " \"" ^ archive ^ "\" " ^ name ^ " >" ^ tmpfilename
    val result = OS.Process.system command
  in
    tmpfilename
  end

(*----------------------------------------------------------------------*)
(* Add a new (empty) package if is not present already			*)
(*----------------------------------------------------------------------*)
fun addPackage (r as ref (Package { packages, classes }), p) =
  case Symbol.OrdMap.find(packages, Ids.symbol p) of
    NONE =>
    let
      val r' = ref (Package 
        { classes = Symbol.OrdMap.empty, packages = Symbol.OrdMap.empty })
    in
      r := Package 
        { classes = classes, 
          packages = Symbol.OrdMap.insert(packages, Ids.symbol p, r') };
      r'
    end

  | SOME r =>
    r

(*----------------------------------------------------------------------*)
(* Add a new class 							*)
(*----------------------------------------------------------------------*)
fun addClass (r as ref (Package { packages, classes }), c, info) =
    r := 
    Package { classes = Symbol.OrdMap.insert(classes, Ids.symbol c, info),
              packages = packages }
         
(*----------------------------------------------------------------------*)
(* Add an entry to the package map					*)
(*----------------------------------------------------------------------*)
fun addEntry (name, info) =
let
  val items = String.fields (fn x => x = #"/" orelse x = #"\\") name

  fun traverse (r, [file]) = 
      (case OS.Path.splitBaseExt file of
        { base, ext = SOME "class" } =>
        addClass (r, base, info)          
  
      | _ => ())

    | traverse (r, p::ps) =
      traverse (addPackage(r, p), ps)
in
  traverse (topPackage, items)
end

(*----------------------------------------------------------------------*)
(* Add a directory, interpreting subdirectories as packages and any	*)
(* files with the extension ".class" as classes.			*)
(*----------------------------------------------------------------------*)
fun addDir basedir =
let
  fun traverse (r, dir) =
    let
      val dirstream = OS.FileSys.openDir dir
      fun make () =
        case OS.FileSys.readDir dirstream of
          "" => 
          ()

        | s => 
          let
            val name = OS.Path.concat (dir, s)
          in
            (if OS.FileSys.isDir name
            then traverse (addPackage (r, s), name)
            else
              case OS.Path.splitBaseExt s of
                { base, ext = SOME "class" } => 
                addClass (r, base, NormalDir basedir)

              | _ => ());
            make ()
          end

        val d = (make ()) handle e => 
                (OS.FileSys.closeDir dirstream; raise e)
      in
        (make ()) handle e => (OS.FileSys.closeDir dirstream; raise e);
        OS.FileSys.closeDir dirstream
      end    
in
  traverse (topPackage, basedir)
end


(*----------------------------------------------------------------------*)
(* Set a path: first close the existing archives, then open new ones.   *)
(* Finally recalculate the whole package map.				*)
(*----------------------------------------------------------------------*)
fun setPath (r, names) =
let
  fun closeEntry (_, SOME zf) = Zip.zipArchiveCloseIn zf 
    | closeEntry _ = ()

  fun checkEntry name =
    if OS.FileSys.access(name, [])
    then true
    else
    (
      PrintManager.print ("\nDirectory or file \"" ^ name ^ 
        "\" does not exist or is not accessible\n");
      false
    )

  fun openEntry name = 
  if OS.FileSys.isDir name
  then (name, NONE)
  else (name, SOME (Zip.zipArchiveOpenIn name))

  fun add (zipname, NONE) = addDir zipname
    | add (zipname, SOME zipin) = 
      let
        val allfiles = Zip.zipDir zipin
      in
        app (fn name => addEntry(name, ZipDir (zipname, zipin))) allfiles
      end
in
  if List.all checkEntry names
  then
  (
    app closeEntry (!r);
    r := map openEntry names;
    topPackage := emptyPackage;
    app add (!bootClassPath);
    app add (!classPath);
    true
  )
  else false
end      

fun setBootClassPath names = setPath(bootClassPath, names)
fun setClassPath names = setPath(classPath, names)

fun getBootClassPath () = map #1 (!bootClassPath)
fun getClassPath () = map #1 (!classPath)

fun longidToClass longid =
let
  val jid::jlongid = map (JavaString.toMLString o Symbol.toJavaString) longid
  val jstrs = 
    jid :: foldr (fn (jid,jstrs) => "/" :: jid :: jstrs) [] jlongid
  val jclassname = String.concat jstrs
in
  jclassname ^ ".class"
end


(*----------------------------------------------------------------------*)
(* Decode a class							*)
(*----------------------------------------------------------------------*)
fun getClass longid =
let
  val name = longidToClass longid

  fun traverse (ref (Package { classes, packages }), [id]) =
      (case Symbol.OrdMap.find(classes, id) of
        NONE => 
        NONE

      | SOME (NormalDir dir) =>
        let
          val f = BinIO.openIn (OS.Path.concat(dir, name))          
          val class = (Decode.decode f) handle e => (BinIO.closeIn f; raise e)
        in
          BinIO.closeIn f;
          SOME class
        end

      | SOME (ZipDir (zipname, zf)) =>   
        (SOME (Decode.decode_zip (zf, name))
        handle IO.Io { name=_,function=_,cause=Zip.UnknownCompressionMode _}=>
        let
          val tmpfilename = unzip (zipname, name)
          val result = Decode.decode_file tmpfilename
        in
          OS.FileSys.remove tmpfilename;
          SOME result
        end
        )
      )

    | traverse (ref (Package { classes, packages }), id::ids) =
      case Symbol.OrdMap.find(packages, id) of
        SOME r' => traverse(r', ids)
      | NONE => NONE
in
  traverse (topPackage, longid)
end

fun getTop () = !topPackage

end

