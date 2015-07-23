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
(* Types required by flow analysis.                                     *)
(*======================================================================*)
structure FlowTypes =
struct

(*----------------------------------------------------------------------*)
(* The position of an application within a term.			*)
(* (1) In a tail position (defined inductively).                        *)
(* (2) In a non-tail position in a let i.e. let x = [.] in e            *)
(* (3) In a non-tail position in a try i.e. try [.] {handlers} success. *)
(*----------------------------------------------------------------------*)
datatype Position = 
  InTail | InLet | InTry 

(*----------------------------------------------------------------------*)
(* The scope in which an application lives                    		*)
(*----------------------------------------------------------------------*)
type Scope = Position * MILPath.Path

end

