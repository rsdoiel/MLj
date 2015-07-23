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
(* Operations on entity references (compilation units).			*)
(*======================================================================*)
structure EntityOps :> ENTITYOPS =
struct

local 
  open Entity
in

(*----------------------------------------------------------------------*)
(* Describe an entity for use in user messages.				*)
(*----------------------------------------------------------------------*)
local
  fun typeToString Sig = "signature"
    | typeToString Str = "structure"
    | typeToString Fun = "functor"
    | typeToString FunSig = "funsig"
in
  fun description (t,id) = typeToString t ^ " " ^ Pretty.idToString id
end

(*----------------------------------------------------------------------*)
(* Describe the entity and (optionally) its file.			*)
(*----------------------------------------------------------------------*)
fun descriptionWithFile (entity, fileref : FileRef) = 
    description entity ^ 
    (if Controls.isOn "showFileRefs" then " [" ^ #1 fileref ^ "]" else "")

(*----------------------------------------------------------------------*)
(* String representation for diagnostic purposes only.			*)
(*----------------------------------------------------------------------*)
fun toString (_,id) = Pretty.idToString id

fun op> ((x1,t1), (x2,t2)) = x1<>x2 orelse Time.> (t1, t2)

(*----------------------------------------------------------------------*)
(* Equality test.							*)
(*----------------------------------------------------------------------*)
fun eq ((t1,s1),(t2,s2)) = t1 = t2 andalso Symbol.equal(s1,s2)

end

end

