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

(* RowMat:ROWMAT implements a sparse matrix as it is reduced by row reduction.  Operations are mostly
   done by side-effects. *)
signature ROWMAT=
sig
   type elem=real
   (* We do not mention reals from now on (in case we need Real128s or something). *)
   type entry={row:int,column:int,value:elem ref}
   (* elem refs are used rather than elems for practical reasons *)

   type rowmat
   val create: entry list*int -> rowmat
   (* create(l,n) creates an n*n matrix containing the entries in l, which may be in any order.
      The resulting matrix is (sigma over entries in this list) {E_{row column} * value}, where
      E_ij is the matrix with a 1 in row i, column j and 0s elsewhere.  The rows and columns
      are indexed by 0..n-1; entries outside this range will raise exceptions. *)

   (* getrow, lrow, getcol, lcolumn should all take unit time modulo gc *)
   val getrow: rowmat*int -> entry list
   (* getrow(mat,i) returns the contents of row i, sorted by column. *)
   val lrow:rowmat*int -> int
   (* lrow(mat,i) returns the number of entries in row i. *)
   val getcol: rowmat*int -> entry list
   (* getcol(mat,i) returns the contents of column i, provided that
      delcol(mat,i) has not been called.
      The entries are NOT sorted (unless by accident). *)
   val delrow: rowmat*int -> unit
   (* delrow(mat,i) affects only the result of l_remaining_column, decrementing l_remaining_column(mat,j)
      by 1 for each (i,j) entry in row i. *)
   val delcol:rowmat*int -> unit
   (* delcol(mat,i) clears its list of entries in column i, hopefully saving
      space.  After it, getcol(mat,i) should not be used. *)
   val l_remaining_column:rowmat*int -> int
   (* l_remaining_column(mat,i) returns the number of entries in column i which have not been
      in rows deleted by delrow *)

   val row_op:rowmat*{pivot_row:int,pivot_col:int,target:int,multiplier:elem}->unit
   (* row_op replaces row #target by row #target + #multiplier* row #pivot.  #pivot & #target should
      be different.  The entry in column pivot_col, which should be in both the
      pivot and target row, is assumed to be zeroed by this and is removed
      from the row list; in the column list it is set to 0.0. *)
end
