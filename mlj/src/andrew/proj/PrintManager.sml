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

structure PrintManager :> PRINTMANAGER = 
struct

fun print s = 
  (TextIO.print s; Debug.print s)

fun printTime message timer =
let
  val t = Timer.checkCPUTimer timer
in
  print (message ^ Time.toString (#usr t) ^ 
  (if Controls.isOn "showGC" 
   then "s including " ^ Time.toString (#gc t) ^ "s gc."
   else "s."))
end

(* Current `process level' *)
val level = ref 0

(* Set when a new process has finished *)
val finished = ref false

fun restart () = (level := 0; finished := false)

fun process (message,oblig) f =
let
  val currentlevel = !level
  val timer = Timer.startCPUTimer ()
  val messages = oblig orelse Controls.isOn "verbose"
in
  level := currentlevel + 1;
  if messages
  then (print (Pretty.newline currentlevel ^ message ^ "..."); 
        finished := false)
  else ();

  let
    val result = (f ()) handle e => (level := currentlevel; raise e)
  in
    if messages
    then 
    (
      if !finished then print (Pretty.newline currentlevel ^ "...") else ();
      if Controls.isOn "showTime" then printTime "" timer else print "done.";
      finished := true
    )
    else ();
    level := currentlevel; 
    result
  end
end

fun printErrors (sm, []) = ()
  | printErrors (sm, errors) =
    (print "\n"; Error.print (fn s => 
      print (CharVector.tabulate(2 + 2 * !level, fn _ => #" ") ^ s), 
     sm, errors))

end
