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
(* The Standard Basis CommandLine structure, extended to deal with	*)
(* Java's main method.							*)
(*======================================================================*)
structure CommandLine :> COMMAND_LINE =
struct

local 
  open List General 
in

val args = Prim.ref ([] : string list)

fun init (env : string option array option) = 
  case env of
    Option.NONE => 
    args := []

  | Option.SOME array =>
    let     
      val nargs = Array.length array
      fun gatherArgs (0, result) = args := result
        | gatherArgs (i, result) =
          let 
            val i = Int.-(i,1)
          in
            case Array.sub(array, i) of
              Option.NONE => gatherArgs (i, ""::result)
            | Option.SOME str => gatherArgs (i, str::result)
          end
    in
      gatherArgs (nargs, [])
    end
fun name () = ""
fun arguments () = !args

end

end
