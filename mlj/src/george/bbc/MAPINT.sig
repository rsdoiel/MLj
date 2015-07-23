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

(* MapInt is a functor taking an ORD_KEY K to a MAPINT structure which finds the intersections of lists.
   (the implementation should get changed when we can use the New Jersey map intersection functions). *)
signature MAPINT=
sig
   type key
   val multi_intersect:key list list * (key*key->bool) -> key list * (key list list)
   (* Given a list of lists, where the elements of each list can be distinguished by
      K, find the sublist of elements occurring in all the lists, such that in addition the
      second equality function supplied also reveals them equal; also return lists corresponding to
      the original ones with the elements in common taken out.

      If the argument is [], []*[] is returned. *)
end
