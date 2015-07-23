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

(* Matrix:MATRIX implements some matrix operations needed for the Basic
   Block code (though it is intended that it may be recycled later).

   Matrices are effectively two-dimensional arrays of reals.
   Rows and columns are numbered from 0;
   the row number is always first.
   *)

signature MATRIX=
sig
   type elem=real (* Reals are not mentioned elsewhere in this signature *)
   type matrix
   type vector
   val vector_sub:vector*int->elem
   (* vector_sub is like Vector.sub *)

   (* raised if there is a problem with the number of dimensions *)

   val create:int*int->matrix
   (* create a new matrix, all zeros, with the given dimensions *)

   val update:matrix*int*int*elem->unit
   (* update(m,i,j,r) adds r to  m(i,j) *)

   exception IllConditioned
   (* This is raised by invertonevec when the matrix is found to be illconditioned or
      singular. *)

   val invertonevec:matrix->vector
   (* invertvec A clobbers A (which it uses as workspace) but returns
      A^{-1} e_1, where e_1 is the vector with 1 in its first entry and
      0s elsewhere.

      This is the only computational function provided for now.  *)
end
