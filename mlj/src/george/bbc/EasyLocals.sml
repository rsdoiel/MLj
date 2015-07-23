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

(* EasyLocals:EASYLOCALS has the job of allocating new local numbers.  It is
   easy because right now it just allocates them naively in order 0,1,2,....,
   allowing for where single or double words are needed.

   When we do more sophisticated register allocation, it will not be here;
   however it may not be necessary then for EasyLocals to look at where
   single or double words are required.
   *)
structure EasyLocals:>EASYLOCALS=
struct
   type localpool=int (* this is the next local number to allocate *)

   fun new_local(lp,jt)=(lp+Types.java_type_size jt,lp)

   fun new_localpool(jtlist)=
   let
      val (pool,rev_args)=
         List.foldl
         (fn (jt,(lp,rev_args_so_far))=>
            let
               val (new_lp,lno)=new_local(lp,jt)
            in
               (new_lp,lno::rev_args_so_far)
            end
            )
         (0,[])
         jtlist
   in
      (pool,List.rev rev_args)
   end

   fun max_locals lp=lp
end
