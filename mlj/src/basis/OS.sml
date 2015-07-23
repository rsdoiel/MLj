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
(* Operating system functionality					*)
(*======================================================================*)
structure OS :> OS where type Process.status = int =
struct

local 
  open General Option List Bool
  val op= = Prim.=
  val op<> = Prim.<>
in

type File = java.io.File
exception Security = java.lang.SecurityException

(* Use POSIX names here just for convention's sake *)
datatype syserror = noent | acces | exist | notdir
exception SysErr of (string * syserror option) 

fun errorMsg noent = "No such file or directory"
  | errorMsg acces = "Permission denied"
  | errorMsg exist = "File exists"
  | errorMsg notdir = "Not a directory"

fun syserror "noent" = SOME noent
  | syserror "acces" = SOME acces
  | syserror "exist" = SOME exist
  | syserror "notdir" = SOME notdir
  | syserror _ = NONE

fun errorName noent = "noent"
  | errorName acces = "acces"
  | errorName exist = "exist"
  | errorName notdir = "notdir"

fun syserr code = raise SysErr (errorMsg code, SOME code)
fun unknownsyserr message = raise SysErr (message, NONE)

structure FileSys :> OS_FILE_SYS = 
struct 

type dirstream = string option array * int ref

fun openDir (s:string) =
(let
  val file = _new File (s)
in
  if file.#exists ()
  then 
    case file.#list() of
      NONE => unknownsyserr "Cannot open directory"
    | SOME array => (array, Prim.ref 0)
  else syserr noent
end)
handle Security => syserr acces

fun readDir (array, pos) =
if Int.<(!pos,0)
then unknownsyserr "Operation applied to closed directory stream"
else
if !pos = Array.length array
then ""
else 
let
  val name = Prim.unsafeValOf(Array.sub(array, !pos))
in
  pos := Int.+(!pos,1); name
end

fun closeDir (array, pos) = pos := ~1

fun isDir (s:string) = 
  (let
    val file = _new File (s)    
  in
    if file.#exists ()
    then file.#isDirectory()
    else syserr noent
  end) 
  handle Security => syserr acces

fun getDir () = 
  Prim.unsafeValOf (java.lang.System.getProperty("user.dir"))

fun mkDir (s:string) = 
  (let
    val file = _new File (s)    
  in
    if file.#exists ()
    then syserr exist
    else 
      if (_new File (s)).#mkdir ()
      then ()
      else unknownsyserr "Cannot create directory"
  end) handle Security => syserr acces

fun rmDir (s:string) = 
  (let
    val file = _new File (s)
  in
    if file.#exists () then
      if file.#isDirectory()
      then 
        if file.#delete()
        then ()
        else unknownsyserr "Cannot remove directory"
      else syserr notdir
    else syserr noent
  end) 
  handle Security => syserr acces

fun fullPath (s:string) = 
(let
  val file = _new File (s)
  val name = Prim.unsafeValOf(file.#getAbsolutePath())
in
  if file.#exists()
  then name
  else syserr noent
end) 
handle Security => syserr acces


fun fileSize (s:string) = 
(let
  val file = _new File (s)
in
  case file.#length() of
    0 => 
    if file.#exists()
    then 0 
    else syserr noent

  | n => n
end)
handle Security => syserr acces
  
fun remove (s:string) = 
  (let
    val file = _new File (s)
  in
    if file.#exists () then
      if file.#isDirectory()
      then unknownsyserr "Not a file"
      else 
        if file.#delete()
        then ()
        else unknownsyserr "Cannot remove file"
    else syserr noent
  end) 
  handle Security => syserr acces

fun rename { old:string, new:string } =
  (if (_new File (old)).#renameTo (_new File (new))
  then ()
  else unknownsyserr "Cannot rename file")
  handle Security => syserr acces

datatype access_mode
       = A_READ
       | A_WRITE
       | A_EXEC

fun access (s:string, flags) =
let
  val file = _new File (s)
  val exists = (file.#exists()) handle _ => false
  fun test [] = true
    | test (flag::flags) =
      case flag of
        A_READ => file.#canRead() andalso test flags
      | A_WRITE => file.#canWrite() andalso test flags
      | A_EXEC => raise General.NotImplemented "OS.FileSys.access with A_EXEC"
in
  exists andalso test flags
end
  
type file_id = int

fun fileId (s:string) = 
  ((_new File (s)).#hashCode ()) handle Security => syserr acces

fun hash fid = Word.fromInt fid

val compare = Int.compare

end

structure Path :> OS_PATH = 
struct 

exception Path 

(* These are common to both Unix and Windows *)
val parentArc  = ".."
val currentArc = "."

local 
    val op @ = List.@
    infix 9 sub
    val op sub = String.sub
    val substring = String.extract
    val op ^ = String.^


val slash = Prim.unsafeValOf(java.io.File.separator)
val slashChar = java.io.File.separatorChar

(* We distinguish DOS from other systems because it's the only one with non-empty
   volumes *)
val isDOS = slashChar = #"\\"
val volslash = slash
fun isslash c = c = slashChar

fun splitabsvolrest s =
    if isDOS andalso Int.>=(String.size s, 2) andalso s sub 1 = #":" 
    then
      if Int.>=(String.size s, 3) andalso isslash (s sub 2) then
          (true, substring(s, 0, SOME 2), substring (s, 3, NONE))
      else
          (false, substring(s, 0, SOME 2), substring (s, 2, NONE))
    else
      if Int.>=(String.size s, 1) andalso isslash (s sub 0) then 
          (true, "", substring(s, 1, NONE))
      else 
          (false, "", s)

in

fun isAbsolute p = #1 (splitabsvolrest p)

fun isRelative p = not (isAbsolute p);

fun fromString p = 
    case splitabsvolrest p of

        (false, v,   "") => {isAbs=false, vol = v, arcs = []}

      | (isAbs, v, rest) => {isAbs=isAbs, vol = v, 
                             arcs = String.fields isslash rest};

fun isRoot p = 
    case splitabsvolrest p of
        (true, _, "") => true
      | _             => false

fun getVolume p = #2 (splitabsvolrest p)

fun validVolume{isAbs, vol} = 
  if isDOS
  then 
    if isAbs 
    then String.size vol = 2 andalso Char.isAlpha(vol sub 0) andalso vol sub 1 = #":"
    else vol=""
  else vol=""

fun toString (path as {isAbs, vol, arcs}) =
    let fun h []        res = res 
          | h (a :: ar) res = h ar (a :: slash :: res)
    in  
        if validVolume{isAbs=isAbs, vol=vol} then 
            case (isAbs, arcs) of

                (false, []         ) => vol
              | (false, "" :: _    ) => raise Path
              | (false, a1 :: arest) => 
                    String.concat (vol :: List.rev (h arest [a1]))

              | (true,  []         ) => vol ^ volslash
              | (true, a1 :: arest ) => 
                    String.concat (List.rev (h arest [a1, volslash, vol])) 
        else
            raise Path
    end;


fun concat (p1, p2) =
    let fun stripslash path = 
            if isslash (path sub (Int.-(String.size path, 1))) then
                substring(path, 0, SOME(Int.-(String.size path, 1)))
            else path
    in
        if isAbsolute p2 then raise Path
        else
            case splitabsvolrest p1 of
                (false, "",   "") => p2
              | (false, v,  path) => v ^ stripslash path ^ slash ^ p2
              | (true,  v,  ""  ) => v ^ volslash ^ p2
              | (true,  v,  path) => v ^ volslash ^ stripslash path ^ slash ^ p2
    end;



fun getParent p =
    let open List
	val {isAbs, vol, arcs} = fromString p 
	fun getpar xs = 
	    rev (case rev xs of
		     []              => [parentArc]
		   | [""]            => if isAbs then [] else [parentArc]
		   | ""   :: revrest => parentArc :: revrest
		   | "."  :: revrest => parentArc :: revrest
		   | ".." :: revrest => parentArc :: parentArc :: revrest
		   | last :: revrest => revrest)
    in
        case getpar arcs of 
            []   => 
                if isAbs then toString {isAbs=true, vol=vol, arcs=[""]}
                else currentArc
          | arcs => toString {isAbs=isAbs, vol=vol, arcs=arcs}
    end;



fun mkCanonical p =
    let val {isAbs, vol, arcs} = fromString p 
        fun backup []          = if isAbs then [] else [parentArc]
          | backup (".."::res) = parentArc :: parentArc :: res
          | backup ( _ :: res) = res
        fun reduce arcs = 
            let fun h []         []  = if isAbs then [""] else [currentArc]
                  | h []         res = res
                  | h (""::ar)   res = h ar res
                  | h ("."::ar)  res = h ar res
                  | h (".."::ar) res = h ar (backup res)
                  | h (a1::ar)   res = h ar (a1 :: res)
            in h arcs [] end
    in
        toString {isAbs=isAbs, vol=vol, arcs=List.rev (reduce arcs)}
    end;



fun parentize []      = []
  | parentize (_::ar) = parentArc :: parentize ar;

fun mkRelative (p1, p2) =
    case (fromString p1, fromString (mkCanonical p2)) of
        (_ ,                {isAbs=false,...}) => raise Path
      | ({isAbs=false,...}, _                ) => p1
      | ({vol=vol1, arcs=arcs1,...}, {vol=vol2, arcs=arcs2, ...}) =>
            let fun h [] [] = ["."]
                  | h a1 [] = a1
                  | h [] a2 = parentize a2
                  | h (a1 as (a11::a1r)) (a2 as (a21::a2r)) =
                    if a11=a21 then h a1r a2r
                    else parentize a2 @ (if arcs1 = [""] then [] else a1)
            in
                if vol1 <> vol2 then raise Path 
                else toString {isAbs=false, vol="", arcs=h arcs1 arcs2}
            end;


fun mkAbsolute (p1, p2) =
    if isRelative p2 then raise Path
    else if isAbsolute p1 then p1
    else mkCanonical(concat(p2, p1));

fun isCanonical p = mkCanonical p = p;

fun joinDirFile {dir, file} = concat(dir, file)

fun splitDirFile p =
    let open List
        val {isAbs, vol, arcs} = fromString p 
    in
        case rev arcs of
            []            => 
                {dir = toString {isAbs=isAbs, vol=vol, arcs=[]}, file = ""  }

          | arcn :: farcs => 
                {dir = toString {isAbs=isAbs, vol=vol, arcs=rev farcs}, 
                 file = arcn}

    end

fun dir s  = #dir (splitDirFile s);
fun file s = #file(splitDirFile s);

fun joinBaseExt {base, ext = NONE}    = base
  | joinBaseExt {base, ext = SOME ex} = base ^ "." ^ ex;

fun splitBaseExt s =
    let val {dir, file} = splitDirFile s
        open Substring 
        val (fst, snd) = splitr (fn c => c <> #".") (all file)
    in 
        if isEmpty snd         (* dot at right end     *) 
           orelse isEmpty fst  (* no dot               *)
           orelse size fst = 1 (* dot at left end only *) 
            then {base = s, ext = NONE}
        else 
            {base = joinDirFile{dir = dir, 
                                file = string (trimr 1 fst)},
             ext = SOME (string snd)}
    end;

fun ext s  = #ext  (splitBaseExt s);
fun base s = #base (splitBaseExt s);

end

end

structure Process :> OS_PROCESS where type status = int =
struct

  val exitActions = Prim.ref ([] : (unit -> unit) list)
  val inExit = Prim.ref false

  type status = int
  val success = 0
  val failure = 1

  (* Currently we do nothing about stdin, stdout and stderr of the process. *)
  (* Problem: how do we get the right behaviour i.e. order of input/output *)
  fun system (command:string) = 
  let
    val runtime = 
      Prim.unsafeValOf(java.lang.Runtime.getRuntime())
    val process = 
      Prim.unsafeValOf(runtime.#exec(command))
    val result = process.#waitFor ()
  in    
    result
  end

  fun loop () = loop ()

  fun atExit f = 
    if !inExit then ()
    else exitActions := f :: !exitActions

  fun terminate status = 
    (java.lang.System.exit(status); loop ())

  fun exit status = 
    if !inExit then raise General.Fail "exit entered"
    else
    (
      inExit := true;
      List.app (fn f => (f ()) handle _ => ()) (!exitActions); 
      terminate status
    )

  fun getEnv key = java.lang.System.getProperty(key:string)

  end

end (* of local open *)

end (* of struct *)

