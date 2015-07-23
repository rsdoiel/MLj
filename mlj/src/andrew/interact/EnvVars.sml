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
(* Environment variables						*)
(*======================================================================*)
structure EnvVars =
struct

(*----------------------------------------------------------------------*)
(* Command used to run applications					*)
(*----------------------------------------------------------------------*)
val javaCommand = ref "java -classpath"

fun getVars () =
(
  if (case OS.Process.getEnv "BOOTCLASSPATH" of
    NONE => true
  | SOME path => (PackageManager.setBootClassPath (Path.pathToList path)))
  then
  if (case OS.Process.getEnv "MLJCLASSPATH" of
    NONE => true
  | SOME path => (PackageManager.setClassPath (Path.pathToList path)))
  then
  (
    (case OS.Process.getEnv "MLJJVM" of
      NONE => ()
    | SOME command => javaCommand := command);

    (case OS.Process.getEnv "MLJDEBUG" of
      NONE => ()
    | SOME _ => Debug.debugOn := true);

    (case OS.Process.getEnv "MLJUNZIP" of
    NONE => ()
    | SOME command => PackageManager.unzipCommand := command);

    true
  )
  else false
  else false
)

end
