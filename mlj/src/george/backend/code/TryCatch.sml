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

(* TryCatch:TryCatch makes it easier to construct exception tables *)
structure TryCatch:>TRYCATCH=
struct
   datatype t=E of
     {start_lab:Labels.label,
      end_lab:Labels.label,
      handler_lab:Labels.label,
      catch_type:ClassHandle.Handle option,
      priority:int
      }
   fun new_exception'(e,p)=E
      {start_lab=Labels.new_label(),
       end_lab=Labels.new_label(),
       handler_lab=Labels.new_label(),
       catch_type=e,
       priority=p
       }

   fun new_exception e=new_exception'(e,0)

   fun start_lab(E e)= #start_lab e
   fun end_lab(E e)= #end_lab e
   fun handler_lab(E e)= #handler_lab e
   fun catch_type(E e)= #catch_type e
   fun priority(E e)= #priority e
end



