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

signature OS_FILE_SYS =
sig

     type dirstream
     val openDir : string -> dirstream 
     val readDir : dirstream -> string 
(*
     val rewindDir : dirstream -> unit 
*)
     val closeDir : dirstream -> unit 
(*
     val chDir : string -> unit 
*)
     val getDir : unit -> string 
     val mkDir : string -> unit 
     val rmDir : string -> unit 
     val isDir : string -> bool 
(*
     val isLink : string -> bool 
     val readLink : string -> string 
*)
     val fullPath : string -> string 
(*
     val realPath : string -> string 
*)
(*
     val modTime : string -> Time.time 
*)
     val fileSize : string -> Position.int 
(*
     val setTime : (string * Time.time option) -> unit 
*)
     val remove : string -> unit 
     val rename : {old : string, new : string} -> unit 
     datatype access_mode
       = A_READ
       | A_WRITE
       | A_EXEC
     val access : (string * access_mode list) -> bool 
(*
     val tmpName : unit -> string 
*)
     eqtype file_id
     val fileId : string -> file_id 
     val hash : file_id -> word 
     val compare : (file_id * file_id) -> order 

end