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
(* Take a class definition and add it to the zip file.              	*)
(*======================================================================*)
structure SaveClass =
struct

local
val zipfile = ref (NONE : Zip.zipOut option)
in

fun name (Class.middling { this, ... }) = 
    valOf (JavaString.toString (ClassHandle.name this))

fun openZip projname = zipfile := 
  SOME (Zip.zipArchiveOpenOut (TargetManager.getZipName projname))

fun save (classdef : Class.class_data) = 
(
  if Controls.isOn "showClasses"
  then PrintManager.print ("[" ^ name classdef ^ "] ")
  else ();
  Class.zip_quick (valOf (!zipfile), classdef)
)

fun closeZip () = Zip.zipArchiveCloseOut (valOf (!zipfile))
end

end
