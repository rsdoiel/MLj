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

signature OS_IO =
sig
     eqtype iodesc
     val hash : iodesc -> word 
     val compare : (iodesc * iodesc) -> order 
     eqtype iodesc_kind
     val kind : iodesc -> iodesc_kind 
     structure Kind : sig
         val file : iodesc_kind 
         val dir : iodesc_kind 
         val symlink : iodesc_kind 
         val tty : iodesc_kind 
         val pipe : iodesc_kind 
         val socket : iodesc_kind 
         val device : iodesc_kind 
       end
     type poll_desc
     type poll_info
     val pollDesc : iodesc -> poll_desc option 
     val pollToIODesc : poll_desc -> iodesc 
     exception Poll
     val pollIn : poll_desc -> poll_desc 
     val pollOut : poll_desc -> poll_desc 
     val pollPri : poll_desc -> poll_desc 
     val poll : (poll_desc list * Time.time option) -> poll_info
     list 
     val isIn : poll_info -> bool 
     val isOut : poll_info -> bool 
     val isPri : poll_info -> bool 
     val infoToPollDesc : poll_info -> poll_desc 
end
