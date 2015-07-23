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

(* Dag:>DAG defines the intermediate language in the middle of the basic
   block code, which consists mainly of directed
   acyclic graphs encoding the flow of data in a basic block.  Most
   optimisations in the basic block code should be rearrangements of these
   DAGs.
   *)
structure Dag:>DAG=
struct
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

   structure NodeKey:>ORD_KEY where type ord_key=DagNode =
   struct
      type ord_key=DagNode
      fun compare(N(_,p1),N(_,p2))=
         (case (p1,p2) of
            (Operation _,Arg _)=>GREATER
         |  (Arg _,Operation _)=>LESS
         |  (Operation _,Thrown)=>GREATER
         |  (Thrown,Operation _)=>LESS
         |  (Thrown,Arg _)=>GREATER
         |  (Arg _,Thrown)=>LESS
         |  (Operation o1,Operation o2)=>Int.compare(#id o1,#id o2)
         |  (Arg a1,Arg a2)=>Int.compare(a1,a2)
         |  (Thrown,Thrown)=>EQUAL
         )
   end

   fun is_constant(N(_,Constant _))=true
   |   is_constant _ =false

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
(* the ints label the labels as it were; the labels of a method run from 0 upwards *)

      (* inorder(A,B) computes whether B must be done before A,
         or more precisely whether there is
         a chain of length >=0 from A to B where each element of the
         chain appears in the args or needs entry of its successor.
         Currently A and B must be operations, though this may change. *)
      fun inorder(NodeA:DagNode,NodeB:DagNode)=
      let
         val N(_,Operation (opA))=NodeA
         val N(_,Operation (opB))=NodeB
         val idA= #id(opA)
         val idB= #id(opB)
      in
         if idA<=idB then
            if idA=idB then true
                       else false
         else
         (* We do a depth-first search, keeping a visited array with
            indices idB+1 . . idA. *)
         let
            val visited=BoolArray.array(idA-idB,false)
            fun is_visited(id)=BoolArray.sub(visited,id-(idB+1))
            fun visit(id)=BoolArray.update(visited,id-(idB+1),true)
            fun dfs(N(_,parts))=case parts of
               Arg _ => false
            |  Constant _ => false
            |  Thrown => false
            |  Operation opO =>

            let
               val idO= #id(opO)
            in
               if idO<=idB then
                  if idO=idB then true
                             else false
               else
                  (* If O is not marked as visited, visit it, and then
                     recurse on its descendants. *)
                  if is_visited(idO) then false
                     (* there can't be anything down here as we've already
                        looked *)
                  else
                     let
                        val _ = visit(idO)
                        val dfs_list = List.exists dfs
                     in
                        dfs_list(#args(opO))
                           orelse
                        dfs_list(#needs(opO))
                     end
            end (* of definition of dfs *)
         in
            dfs(NodeA)
         end
      end (* end of inorder *)

   fun inorder2 (A,B) =
   let
      (* We construct an array as in inorder.  B is only used for finding the
         minimum index. *)
      fun n_id(N(_,Operation{id,...}))=id

      val maxA=
         List.foldl
            (fn(n,sf)=>Int.max(n_id n,sf))
            0
            A
      (* result is false for node with id>maxA *)
      val minB=
         List.foldl
            (fn(n,sf)=>Int.min(n_id n,sf))
            (maxA+1)
            B
      (* we compute all nodes <= some in A with id in [minB,maxA].  Thus if B is empty the
         array has length 0. *)
      val arrlen=maxA-minB+1

      val isbefore=BoolArray.array(arrlen,false)
   
      fun is_visited(id)=BoolArray.sub(isbefore,id-minB)
      fun visit(id)=BoolArray.update(isbefore,id-minB,true)

      fun dfs(N(_,parts))=
         (case parts of
            Arg _ => {}
         |  Constant _ => {}
         |  Thrown => {}
         |  Operation opO =>
            let
               val idO= #id(opO)
            in
               if idO<minB orelse is_visited idO
               then 
                  {}
               else
               let
                  val () = visit idO
                  val () = List.app dfs (#args opO)
                  val () = List.app dfs (#needs opO)
               in
                  {}
               end
            end
         ) 
      (* of definition of dfs *)

      val () = List.app dfs A
   in
      fn(N(_,Operation{id,...})) =>
         if id>maxA 
         then 
            false
         else
            is_visited id
   end
            
   fun descendants(A:DagNode)=
   (* descendants returns the list of Operation DagNodes, including A
      (which should be an Operation DagNode), which can be
      reached from A through args and needs lists.  The DagNodes are
      returned in descending order of id.

      descendants could be speeded up by moving the recursion, since it
      deconstructs DagNodes twice. *)
   let
      val N(_,Operation OpA)=A
      val node_array=Array.array(#id OpA + 1,NONE:DagNode option)
      (* We use depth-first-search to find all the descendants of A;
         they are stored in node_array. *)
      fun dfs(node as N(_,Operation Op))=
         (case Array.sub(node_array,#id Op) of
            SOME _ => {} (* we've been here before! *)
         |  NONE  => (* visit this node and recurse on descendants *)
               let
         	  val _ = Array.update(node_array,#id Op,SOME node)
         	  val this_uses= #args Op @ #needs Op
         	  val children=
         	     List.filter
         		(fn   N(_,Operation _)=>true
         		   |  _ =>false
         		   )
         		this_uses
               in List.app dfs children
               end
         )
      val _=dfs A
   in
      Array.foldl
         (fn  (NONE,list_so_far)=>list_so_far
            | (SOME node,list_so_far)=>node::list_so_far
            )
         []
         node_array
   end

(* the XXX_toString functions are intended for debugging purposes only and
   should not be used in production code or in any other way relied on. *)
   local
      fun dbid(DagB{id,...})=id

      (* the describe_XXX functions produce a brief string used in arguments etcetera
         describing nodes. *)
      fun describe_arg(N(t,Arg i))=
         "A"^Int.toString(i)^" "^Types.java_void_type_toString(t)^"\n"
      fun describe_const(N(t,Constant c))=
      (* we avoid displaying the type if it matches the type of the
         constant (otherwise there is a bug!) *)
      let
         val ctype=Constants.typeOf c
         val match=
            (case t of
               SOME jt => Types.java_type_equal(jt,ctype)
            |  NONE => false
            )
      in
         if match
         then Constants.constant_toString c ^ "\n"
         else "***** Mismatched constant!!!!!\n"
         (* this message will get more detailed if it occurs. .  *)
      end

      fun describe_op(N(_,Operation Op))=Int.toString(#id Op)^"\n"
      (* the type of Op and other information will get listed when it is. *)

      fun describe_thrown(N(_,Thrown))="Thrown\n"

      val describe=
         (fn a as N(_,Arg _)=>describe_arg a
         |   a as N(_,Constant _)=>describe_const a
         |   a as N(_,Operation _)=>describe_op a
         |   a as N(_,Thrown)=>describe_thrown a
         )
   in

      val DagNode_brief_toString=describe

      fun DagNode_toString(Node:DagNode)=
      (* This produces a string describing Node and all its
         descendants. Node should be an Operation node. *)
      let
         fun full_op(N(t,Operation Op))=
         (* this does the complete description of the operation *)
         let
            val os=Operations.operation_toString(#comp Op)
         in
            Int.toString(#id Op) ^ " " ^
            Types.java_void_type_toString(t) ^ " " ^ os
         end
            ^"\n" ^
            "Arg list:\n" ^
            (String.concat
               (List.map
                   describe
                   (#args Op)
                   )
                ) ^
            (case #needs Op of
               []=>""
            |  Needs=>
                  "Also needs: "^
                  (String.concat
                     (List.map
                        (fn N(_,Operation needed)=>
                           Int.toString(#id needed)^" ")
                        Needs
                        )
                     ) ^ "\n"
               )
      in String.concat
            (List.map
               full_op
               (descendants Node)
               )
      end


      fun DagLabel_toString(dboref,arglist,_)=
      let
         val SOME db= !dboref
      in
         "to block#"^Int.toString(dbid db)^" with args\n"^
            (String.concat
               (List.map
                   describe
                   arglist
                   )
                )
      end

      fun DagExit_toString Ex=
         (case Ex of
            cond {test,yes,no}=>
               "if "^Tests.toString test^"\n"^
               "yes:"^DagLabel_toString yes^
               "no :"^DagLabel_toString no
         |  cond0 {test,yes,no}=>
               "if0 "^Tests.toString test^"\n"^
               "yes:"^DagLabel_toString yes^
               "no :"^DagLabel_toString no
         |  goto lab=>"goto "^ DagLabel_toString lab
         |  athrow=>"athrow\n"
         |  return=>"return\n"
         |  lookupswitch{lookuptable,default}=>
               "lookupswitch\n"^
               String.concat(List.map
                   (fn (ji,lab)=>"if"^JavaInt.toString ji^" "^DagLabel_toString lab)
                   lookuptable
                   )^
               ("default "^DagLabel_toString default)
         |  tableswitch{jumptable,low,not_in_interval} =>
               "tableswitch from "^JavaInt.toString low^"\n"^
               String.concat(List.map
                  (fn lab=>DagLabel_toString lab)
                  jumptable)^
               ("not in interval "^DagLabel_toString not_in_interval)
         )

      fun DagExcep_toString(DagE(cho,lab))=
         "Catch "^
         (case cho of
            SOME c => JavaString.toMLString(ClassHandle.name c)
         |  NONE => "everything"
         ) ^ " " ^ DagLabel_toString lab

      fun descriptor_toString(Operations.D{input,output})=
         "input:"^
             String.concat(List.map
                 (fn jt=>" "^Types.java_type_toString jt)
                 input
                 )^"\n"^
         "output:"^
             String.concat(List.map
                 (fn jt=>" "^Types.java_type_toString jt)
                 output
                 )^"\n"

      fun DagBlock_toString(DagB {id,vale,descriptor,exit,exceptions,is_handler})=
         "Block #"^Int.toString id^(if is_handler then " (handles exceptions)\n" else "\n")^
         descriptor_toString descriptor^
         String.concat(List.map
            (fn ex=>DagExcep_toString ex)
            exceptions)^
         DagNode_toString vale ^ "\n *** Exit:\n"^
         DagExit_toString exit ^ "\n\n"
   end
end
