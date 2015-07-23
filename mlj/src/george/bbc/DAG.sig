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

(* Dag:DAG defines the intermediate language in the middle of the basic
   block code, which consists mainly of directed
   acyclic graphs encoding the flow of data in a basic block.  Most
   optimisations in the basic block code should be rearrangements of these
   DAGs.
   *)
signature DAG=sig
   datatype DagNode=N of Types.java_void_type * Parts
   (* A DagNode is a node in the Dag, and corresponds to some value
      in the basic block.  Each DagNode has its type attached. *)
   and Parts= (* this denotes where the node comes from. *)
      Arg of int (* Arg(i) denotes argument number i, where arguments
                    are numbered from 0. *)
   |  Constant of Constants.constant
   |  Operation of {
           comp: Operations.operation,
           args: DagNode list,
           needs: DagNode list,
           id:int
           }
   |  Thrown
      (* The information in the operation record correspond to some
         computation in the block.  args is a list, in order, of the
         arguments required.  needs is a list of dependencies; these
         are constructed so that any ordering of the operations is valid
         provided that we do the operations in args and needs first.

         The operations in a basic block are numbered by id, starting at
         id=0.  The numbering respects the args and needs lists, and comes
         from the supplied list. *)

   structure NodeKey:ORD_KEY
   sharing type NodeKey.ord_key=DagNode
   (* NodeKey provides an ordered key structure on Operation and Arg nodes in the
      same basic block (Constant nodes are not supported and will cause matching failures *)

   (* We now define the connections between blocks in terms of these
      DagNodes.  This is parallel to the basic block datatypes.
      *)
   datatype
      DagBlock=DagB of
        {id:int, (* identifies this basic block uniquely for this method.
                    These ids will be consecutive integers, starting at
                    0 for the beginning of the method. *)
         vale:DagNode, (* vale instruction for the block. *)
         descriptor:Operations.block_descriptor, (* descriptor for the block *)
         exit:DagExit, (* exit *)
         exceptions:DagExcep list, (* exceptions for the block *)
         is_handler:bool (* true if this block is an exception handler *)
         }
   and
      DagExcep= DagE of ClassHandle.Handle option * DagLabel
      (* all the arguments in this label should be argument nodes *)
   and
      DagExit=
         cond of
           {test:Tests.test,
            yes:DagLabel,
            no:DagLabel
            }
      |  cond0 of (* compare integer or reference with 0 *)
           {test:Tests.test,
            yes:DagLabel,
            no:DagLabel
            }
      |  goto of DagLabel
      |  athrow
      |  lookupswitch of
           {lookuptable: (JavaInt.t*DagLabel) list,
            default:DagLabel
            }
      |  tableswitch of
           {low:JavaInt.t,
            jumptable:DagLabel list,
            not_in_interval:DagLabel
            }
      |  return
   withtype
      DagLabel=DagBlock option ref*DagNode list*int

   val inorder:DagNode*DagNode->bool
   (* inorder(A,B) computes whether B must be done before A,
      or more precisely whether there is
      a chain of length >=0 from A to B where each element of the
      chain appears in the args or needs entry of its successor.
      Currently A and B must be operations, though this may change. *)

 
   val inorder2:(DagNode list*DagNode list) -> DagNode -> bool
   (* inorder2(A,B) c is defined when
      there exists b in B with inorder(c,b) and is
      true if there exists a in A with inorder(c,a).
      *) 

(* the XXX_toString functions are intended for debugging purposes only and
   should not be used in production code or in any other way relied on. *)

   val DagBlock_toString:DagBlock->string

   val DagNode_brief_toString:DagNode->string
end
