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
(* General Java stuff       						*)
(*======================================================================*)
signature JAVAOPS =
sig

(* Legal class modifiers *)
val classModifiers    : Class.flag Map.map

(* Legal field modifiers *)
val fieldModifiers    : Field.flag Map.map

(* Legal method modifiers *)
val methodModifiers   : Method.flag Map.map

(* Legal constructor modifiers *)
val constructorModifiers : Method.flag Map.map

(* Legal Java operations *) 
val ops               : Java.OpType Map.map

(* Legal core operations *)
val coreops           : Java.OpType Map.map

(* Legal optional (basis-only) operations *)
val optops            : Java.OpType Map.map

val classInfoToString : Java.ClassInfo -> string
val opTypeToString    : Java.OpType -> string
val exconToString     : Java.ExCon -> string

val classModToString  : Class.flag -> string
val fieldModToString  : Field.flag -> string
val methodModToString : Method.flag -> string

(* Java constant comparison *)
val compareConsts     : Constants.constant * Constants.constant -> order

(* Is this a legal Java field or method identifier? *)
val isLegalJava       : string -> bool

(* Is this a legal Java class name? Note: dots not slashes must be used *)
val isLegalJavaClass  : string -> bool

end





