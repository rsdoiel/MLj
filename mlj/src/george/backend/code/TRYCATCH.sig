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

(* TryCatch:>TRYCATCH makes it easier to construct exception tables *)
signature TRYCATCH=
sig
   type t (* One for each exception+range to catch *)
   val new_exception':ClassHandle.Handle option*int -> t
   (* create a new exception for the given catch type with priority [int] *)
   val new_exception:ClassHandle.Handle option -> t
   (* create a new exception with priority 0 *)

   val start_lab:t->Labels.label
   val end_lab:t->Labels.label
   val handler_lab:t->Labels.label
   (* Obtain the labels corresponding to the beginning, end and handler for the exception *)
   val catch_type:t->ClassHandle.Handle option
   (* Obtain the catch type of the exception *)
   val priority:t->int
   (* Obtain the priority of the exception *)
end



