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

signature TIMER =
sig
     type cpu_timer
     type real_timer
     val startCPUTimer : unit -> cpu_timer 
     val checkCPUTimer : cpu_timer -> {usr : Time.time, sys :
     Time.time, gc : Time.time} 
     val totalCPUTimer : unit -> cpu_timer 
     val startRealTimer : unit -> real_timer 
     val checkRealTimer : real_timer -> Time.time 
     val totalRealTimer : unit -> real_timer 
end