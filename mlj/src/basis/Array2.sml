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

structure Array2 :> ARRAY2 =
struct

  local open Int General in

  type 'a array = 
  { rows : int, cols : int, a : 'a Array.array }

  type 'a region = {base : 'a array, row : int, col : int, nrows
     : int option, ncols : int option} 
  datatype traversal
       = RowMajor
       | ColMajor

  fun array (r,c,v) = { rows = r, cols = c, a = Array.array(r*c,v) }

  fun sub ({ rows, cols, a }, i, j) = 
    if j >= cols orelse j < 0 
    then raise Subscript else Array.sub(a, i*cols + j)

  fun update ({ rows, cols, a }, i, j, v) =
    if j >= cols orelse j < 0
    then raise Subscript else Array.update(a, i*cols + j, v)

  fun tabulate trav (r,c,f) = raise Fail "Array2.tabulate not implemented"
 
  fun fromList [] = { rows = 0, cols = 0, a = Array.fromList [] }
    | fromList l =
      { rows = List.length l, cols = List.length (List.hd l), 
        a = Array.fromList (List.concat l) }
  
  fun dimensions { rows, cols, a} = (rows,cols)
  fun nCols { rows, cols, a} = cols
  fun nRows { rows, cols, a} = rows

  end


end