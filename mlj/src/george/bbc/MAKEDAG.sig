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

(* MakeDag:MAKEDAG finds the dag corresponding to a basic block, working out
   all the dependency information.  (It does not work out how the blocks
   connect together.)  It also contains the map_args function
    for converting the
   argument list into a map from value numbers to DagNodes containing the
   argument data, which is also used by the Interblock structure *)
signature MAKEDAG=
sig
   val map_args:Operations.block->Dag.DagNode IntBinaryMap.map

   val make_dag:int*Operations.block*int ref->Dag.DagBlock*
      (Operations.block*Dag.DagBlock option ref) list
   (* make_dag turns a single block into a DAG, putting it in a
      DagBlock with the specified id number.  However it does not
      do this conversion for the blocks
      required in the labels from the block.  Instead it returns, as we
      as the DagBlock, a list of pairs (block,reference cell).
      There is one block for each label that occurs in the given block.
      The reference cell should later be set, by Wire.wire, to the
      DagBlock corresponding to this block.

      The int ref is used for numbering the labels; the value when the
      function is called is the first number to use, and when it returns
      is the first remaining one to use. *)
end
