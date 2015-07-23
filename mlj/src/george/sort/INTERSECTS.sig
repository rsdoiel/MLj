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

(* Intersects is a functor which takes an ORD_KEY to
   INTERSECTS, a structure containing the function intersects which tests if
   two lists contain a common element. *)
signature INTERSECTS=
sig
   type t (* item we can sort *)
   val intersects:t list*t list->bool
   val find_intersects:(t*'a) list*(t*'b) list->(t*'a*'b) list
   (* find_intersects(alist,blist) returns the list containing, for each t such that
      a (t,_) occurs in alist and blist, (t,a,b) where (t,a) is in alist and (t,b) is in blist.
      What happens when t occurs more than once in either list is undefined. *)

   val intersects_sorted:t list*t list->bool
   val find_intersects_sorted:(t*'a) list*(t*'b) list->(t*'a*'b) list
   (* intersects_sorted and find_intersects_sorted are like intersects and find_intersects, but
      they assume their arguments are sorted in increasing order with respect to t. *)
end
