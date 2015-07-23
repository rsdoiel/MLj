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

structure Bool :> BOOL =
struct

datatype bool = datatype Datatypes.bool

val op= = Prim.=

fun not false = true
  | not true = false

fun toString false = "false"
  | toString true = "true"

fun scan getc src =
let
  fun scanBool (str, pos, src, result) =
  if pos=0
  then Datatypes.SOME (result, src)
  else 
    case getc src of
      Datatypes.NONE => 
      Datatypes.NONE

    | Datatypes.SOME (c, src') =>
      let
        val pos' = Prim.sub(pos, 1)
      in
        if Prim.=(MLJUtils.Char.toLower c, MLJUtils.String.sub(str, pos'))
        then scanBool (str, pos', src', result)
        else Datatypes.NONE 
      end

  fun skip src =
  case getc src of
    Datatypes.NONE => Datatypes.NONE
  | Datatypes.SOME (c, src') =>
    if MLJUtils.Char.isSpace c then skip src'
    else if MLJUtils.Char.toLower c = #"t"
    then scanBool ("eur", 3, src', true)
    else if MLJUtils.Char.toLower c = #"f"
    then scanBool ("esla", 4, src', false)
    else Datatypes.NONE
in
  skip src
end

fun fromString s = MLJUtils.String.scanString scan s

end
