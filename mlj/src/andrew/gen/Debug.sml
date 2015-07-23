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

structure Debug =
struct

val log = ref ""
val debugOn = ref false
val logFile = ref (NONE : TextIO.outstream option)

fun print s = 
  case !logFile of
    NONE => ()
  | SOME f => (TextIO.output(f, s); TextIO.flushOut f)

fun finish () =
  case !logFile of
    NONE => ()
  | SOME f => (TextIO.closeOut f; logFile := NONE)

fun time (f : 'a -> 'b) x =
let
  val timer = Timer.startCPUTimer ()
  val y = f x 
  val t = Timer.checkCPUTimer timer
in 
  print ("Time elapsed = " ^ 
    Time.toString (#usr t) ^ "s usr, " ^
    Time.toString (#sys t) ^ "s sys, " ^
    Time.toString (#gc t) ^ "s gc.\n");
  y
end

fun start () = 
  (finish ();
  if !log = "" then ()
  else logFile := SOME (TextIO.openOut (!log))
  )

fun fail message = 
  (print ("Compiler bug: " ^ message); raise Fail message)


fun dump n (s:string) =
   let
     val f = TextIO.openOut n
     val _ = TextIO.output(f,s)
     val _ = TextIO.closeOut f
   in () end

fun dumpMany n ss = 
   let
     fun dump' i [] = ()
       | dump' i (s::ss) = 
         (dump (n ^ Int.toString i) s; dump' (i+1) ss)
   in
     dump' 0 ss
   end

end
