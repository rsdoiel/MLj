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

structure ListPair :> LIST_PAIR =
struct

local open List Bool in

fun zip (x::xs, y::ys) = (x,y) :: zip(xs,ys)
  | zip _ = []

fun unzip [] = ([],[])
  | unzip ((x,y)::zs) = 
    let
      val (xs,ys) = unzip zs
    in
      (x::xs,y::ys)
    end

fun map f (xs, ys) =
let
  fun map' (x::xs, y::ys) = f (x,y) :: map' (xs,ys)
    | map' (_, _) = []
in
  map' (xs,ys)
end

fun app f (xs, ys) = 
    let fun app' (x::xr, y::yr) = (f (x, y); app' (xr, yr))
	  | app' _              = ()
    in app' (xs, ys) end

fun all p (xs, ys) = 
    let fun h (x::xr, y::yr) = p(x, y) andalso h (xr, yr)
	  | h _              = true
    in h (xs, ys) end

fun exists p (xs, ys) = 
    let fun h (x::xr, y::yr) = p(x, y) orelse h (xr, yr)
	  | h _              = false
    in h (xs, ys) end

fun foldr f e (xs, ys) = 
    let fun h (x::xr, y::yr) = f(x, y, h (xr, yr))
	  | h _              = e
    in h (xs, ys) end

fun foldl f e (xs, ys) = 
    let fun h (e, x::xr, y::yr) = h (f(x, y, e), xr, yr)
	  | h (e, _,     _    ) = e
    in h (e, xs, ys) end

end

end
