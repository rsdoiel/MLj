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

(* Precomp:PRECOMP does the "preliminary compilation" of a DagBlock into backend instructions.  That is
   to say, it produces the backend instructions which do the computations in the block (with the exception
   of any involved in the exit instruction itself - I am not yet sure how this can be dealt with best),
   but the local numbers output are unlikely to be the ones in the final code (indeed they may
   not even work; longs and doubles will not use two locals as they do in real life).  Precomp attempts
   no optimisation of locals at all; it uses a new local number each time it has a new value to store,
   with the single exception of when it uses the iinc instruction and the local in question is no
   longer needed. *)
structure Precomp:>PRECOMP=
struct
   local
      open Operations
      open Dag
      open Types
     
      structure DFSnode=DFS(SplaySetFn(Dag.NodeKey))
      structure Sortnode=Sort(Dag.NodeKey)
      structure Mapnode=SplayMapFn(Dag.NodeKey)

      val int_type=F(0,INT)

      (* typeOf returns the type of a node *)
      fun typeOf(N(jvt,_))=jvt


      structure IBM=IntBinaryMap

   in
      fun precomp{first_local=first_local,db=Dag.DagB
         { vale=vale_node,descriptor=Operations.D{input=input,...},id=db_id,
           is_handler,... }}=
      let
         val N(_,vale_op as Operation vale_data)=vale_node
         val n_ids= #id vale_data + 1
         (* The ids allocated go from 0 to #id vop, so we can index DagNodes for this block by
            allocating arrays of size n_ids *)
         val n_args=List.length input
         val n_args'=n_args+
            (if is_handler then 1 else 0)
         (* n_args'  is like n_args but includes an extra local for the thrown object,
            if any. *)
         val n_all=n_args'+n_ids

         (* There is no point in pretending otherwise; the algorithm used for turning DagNodes into
            instructions is very imperative indeed!
            We compile the instructions in the order they are to be output and as we go we maintain the
            following variables: *)

         (* We use a number of arrays in this function.  Many of them are indexed on the union of
            Arg and Operation DagNodes.  We use the following numbering: *)
         val number_Thrown=n_args
         fun number(N(_,Arg i))=i
         |   number(N(_,Thrown))=number_Thrown
         |   number(N(_,Operation Op))=n_args' + #id Op
         |   number(N(_,Constant _))= ~1
         (* We also use number occasionally for comparison of nodes; hence constants go to -1 so they
            effectively compare equal *)

         fun is_constant(N(_,Constant _))=true
         |   is_constant _=false

         val node_array=Array.array(n_all,NONE:DagNode option)
            (* node_array inverts number *)
         (* use DFS to construct node_array *)
         val _=DFSnode.simple_dfs
            (fn N(_,Operation Op)=>
               (* Neighbourhood function.  It's important that we don't return Constant nodes
                  as neighbours *)
                (List.filter (fn node=>not(is_constant node)) (#args Op))
                @ #needs Op (* needed nodes can't be constants *)
            |   _=>[]
            )
            (fn (node,{})=>Array.update(node_array,number node,SOME node))
            {}
            vale_node


         datatype node_status_type=TODO|DONE|LOCAL of int
         val locals_for_nodes=Array.tabulate(n_all,
            fn i=>if i<n_args' then LOCAL (first_local+i) else TODO)
         fun status node=Array.sub(locals_for_nodes,number node)
         (* status returns TODO if the node has yet to be computed, DONE if it has been computed
            but it wasn't necessary to store it in a local variable, and LOCAL i if it has been
            computed and stored in local variable number i *)

         val nodes_for_locals=Array.tabulate(n_all,
            fn i=>if i<n_args' then Array.sub(node_array,i) else NONE)
         (* nodes_for_locals contains the nodes corresponding to given locals *)
         fun node_of local_no=valOf(Array.sub(nodes_for_locals,local_no-first_local))

         fun set_status(node,new_status)=
            (Array.update(locals_for_nodes,number node,new_status);
               (case new_status of
                   LOCAL local_no => Array.update(nodes_for_locals,local_no-first_local,SOME node)
               |   _  => {}
               ))

         (* Clarification - for block arguments which are never used (this occurs for example if
            the block is the first block of a method which ignores one of its argument)
            node_array[i] will be NONE (because the depth-first search never reached that
            node), nodes_for_locals[i] will be NONE (as it's copied from node_array) but
            locals_for_nodes will be first_node +i.  This hardly matters really, because when producing
            the code instructions we will never look at such nodes anyway. *)

         val next_local=ref (first_local+n_args')
         (* The next local number to use. *)

         datatype usage_type=
         (* The usage of a node describes how often its result is used, in the sense of being an
            argument to something.  *)
            UNUSED             (* Result of node is unused *)
         |  USEDONCE of DagNode (* Result of node is used just once, in one argument position
                                   of this DagNode *)
         |  USEDMANY of int (* Result of node is used N times, where N>=2 is the argument *)
         val node_usage=Array.array(n_all,UNUSED)
         (* node_usage will contain usage types for all nodes *)
         val _=
            Array.appi
               (fn (_,NONE) => {}
               |  (_,SOME(node as N(_,Operation {args,...})))=>
                     List.app
                        (fn argnode=>
                           if is_constant argnode then {}
                           else
                              let
                                 val n=number argnode
                              in
                                 Array.update(node_usage,n,
                                    case Array.sub(node_usage,n) of
                                       UNUSED => USEDONCE node
                                    |  USEDONCE _ => USEDMANY 2
                                    |  USEDMANY n => USEDMANY (n+1)
                                    )
                              end
                           )
                        args
                )
               (node_array,n_args',NONE)

         fun usage node=Array.sub(node_usage,number node)
(*
         fun node_is_live(node,except_id)=
            (* true if there are any nodes not yet done for which node is an argument,
               apart from the one with id except_id which does use it.  This function
               is fairly slow as it checks all the nodes; it could be speeded up by keeping a count
               of remaining uses.  But this does not seem necessary as I only plan to use
               node_is_live
               when deciding whether to stamp on an existing local using iinc. *)
         (case usage node of UNUSED => false | USEDONCE _ => false | USEDMANY _ =>
            let
               val node_n=number node
               exception IsLive
            in
               ((Array.app
                  (fn SOME(possible_parent as N(_,Operation Op))=>
                  let
                     val yeshere=
                        if status possible_parent=TODO andalso #id Op <> except_id then
                           List.exists
                              (fn argnode=>(number argnode=node_n))
                              (#args Op)
                        else false
                  in
                     if yeshere then raise IsLive else {}
                  end
                  )
                  node_array
                  );false) handle IsLive => true
            end
            )
*)

         (* We can now proceed to compute the instructions. *)
         val instructions=ref ([]:Code.instruction list)
         (* The instructions themselves.  They are added at the front of the list, and so this list
            will need to be reversed before this function returns.

            We do some peephole optimisation before returning these instructions
            *)

         val stack_words=ref (if is_handler then 1 else 0)
         (* stack_words is the current stack size. *)


         (* We insert stack_size instructions to meet the following criteria:
            1) There is a stack_size instruction immediately before every load and
               immediately after every store.  The argument to the stack_size instruction
               should be the number of words on the stack at that point.
            2) Any segment of code between a store to a local variable and a load from the
               same local variable should satisfy the property that the minimum and maximum
               stack sizes attained before, after, or during the segment should be the minimum
               and maximum argument to stack_size in that segment.  For example, the add(int)
               instruction, which replaces values A/B by a value C where C=A+B, is deemed to
               attain three stack sizes: that with both A & B on the stack; that with
               C on the stack, and that after A and B have been removed from the stack but before
               C has been put on the stack.
            3) The segment of code from the beginning to the first store should satisfy the
               property that the maximum stack size attained before, during, or after the
               segment should be the maximum argument to stack_size in that segment.
            *)
         (* We achieve this and try to avoid putting too many stack_size instructions in
            using some more state: *)
         local
            val lowest_sofar=ref(!stack_words)
            val highest_sofar=ref(!stack_words)
            (* [lowest_sofar,highest_sofar] is the range of stack attained so far since the
               last store or since the code began. *)
            fun add_ss i= instructions:= Code.stack_size i :: !instructions
         in
            (* record_absolute_stack records that the stack size has reached sw in an instruction
               near this (we don't need the exact location so long as we don't cross load/stores).
               record_stack uses the current stack size and record_stack' uses the current stack
               size plus an offset. *)

            fun record_absolute_stack sw=
               if sw> !highest_sofar
               then
                 (add_ss sw;
                  highest_sofar:= sw)
               else
                 if sw< !lowest_sofar
                 then
                    (add_ss sw;
                     lowest_sofar:= sw)
                 else {}
          
            fun record_stack()=record_absolute_stack(!stack_words)

            fun record_stack' i=record_absolute_stack(!stack_words + i)

            fun after_store ()=
            let
               val sw= !stack_words
            in
              (add_ss sw;
               lowest_sofar:= sw;
               highest_sofar:= sw
               )
            end

            fun before_load ()=
               add_ss(!stack_words)
         end

         (* The convention is that operations always ensure they have inserted appropriate
            stack_size operations for the input to the operation and for any intermediate steps
            in the operation.  They do not need to do it for the result of the operation. *)


         fun do_load i=
         (* load local variable number i *)
         let
            val jt=valOf(typeOf(node_of i))
         in
           (before_load();
            instructions:= Code.load(widen jt,i):: !instructions ;
            stack_words:= !stack_words + Types.java_type_size jt
            )
         end

         fun do_store(node,i)=
         (* store the top of stack, which contains the value corresponding to node, in
            local variable i *)
         let
            val jt=valOf(typeOf node)
         in
           (record_stack();
            instructions:= Code.store(widen jt,i):: !instructions ;
            stack_words:= !stack_words - Types.java_type_size jt;
            after_store();
            set_status(node,LOCAL i)
            )
         end

         fun do_const const=
         (* push the supplied constant *)
           (instructions:= Code.push const:: !instructions ;
            stack_words:= !stack_words + Types.java_type_size(Constants.typeOf const)
            )

         val _=
            if is_handler
            then
            (* If the exception object is used store it in its local variable; otherwise
               pop it off the stack *)

               (case Array.sub(node_usage,number_Thrown) of
                  UNUSED =>
                    (instructions:= Code.pop:: !instructions;
                     stack_words:= 0
                     )
               |  _ => do_store(valOf(Array.sub(node_array,number_Thrown)),number_Thrown+first_local)
               )
            else {}

         (* The translation is done by three functions.  Most of the work is done by
            do_push, which finds a value needed now on the stack, and do_need, which computes a value
            which isn't needed on the stack.  These two functions are mutually recursive.  There is
            also do_vale which sets them off going down the tree of DagNodes. *)

         fun do_push(N(_,Constant const),_)=do_const const
         |   do_push(node,storeit)=
(* do_push(node,storeit) pushes the node.  If storeit is true, it also stores it in a local
   variable if the node's usage is USEDMANY (IE used more than once) and it hasn't already
   been stored.  (Otherwise, do_push is being called from do_needs, which does its own
   storing.) *)
            (case status node of
               LOCAL i => do_load i
            |  DONE => raise Fail
"Bug in basic block code - DONE for DagNode to be pushed!"
            |  TODO =>
            let
               val N(jvt,Operation {args=args,needs=needs,comp=comp,...})=node

               val initial_stack_words= !stack_words
               val _=record_stack()

               fun do_nlist{needed_nodes:Dag.DagNode list,is_barred:Dag.DagNode->bool}=
               let
                  (* do_nlist ensures that needed_nodes are done.  If Variables.moveup_needs
                     is set (which it usually is) it attempts to replace nodes by parents
                     unless is_barred parent is true.
                     *)
                  val revised_needed=
                  if !Variables.moveup_needs then
                  let
                     fun replace_parent needed_node=
                     (* SOME parent if needed_node is USEDONCE by parent which can be done before
                     the node we are pushing; NONE otherwise *)
                        (case usage needed_node of
                           USEDONCE parent =>
                              if is_barred parent
                              then NONE
                              else SOME parent
                        |  _ => NONE
                        )

                     fun ancestor node=
                     (case replace_parent node of
                        SOME parent => ancestor parent
                     |  NONE => node
                     )  (* This must terminate since eventually we must come to the vale instruction *)
                  
                     val unsorted_revised=List.map ancestor needed_nodes
                  in
                     List.rev(Sortnode.sort unsorted_revised)
                  end
                  else
                     needed_nodes

                  val () = List.app do_needs revised_needed
                  (* NB that it does no harm if do_needs is called more than once for the
                     same operation. *)
               in
                  ()
               end
 
               (* Divide needs list into those nodes which can be done before
                  any of the arguments, and the rest.  We do the needs in the first group 
                  before anything else, to avoid gratuitous use of stack.
           
                  This algorithm is fairly slow but can be improved if necessary.
                  *)
               val op_args=
                  List.filter
                     (fn N(_,Operation _) => true
                     |   _ => false
                     )
                     args

               val args'=
                  (case op_args of
                     [] => [node]
                  |   _ => op_args
                  )

               fun is_afterargs node=
               let
                  val afun=Dag.inorder2([node],args')
               in
                  List.exists afun args'
               end 

               val (needs_after,needs_before)=List.partition is_afterargs needs

               (* Now do the needs_before arguments *)
               val ()=do_nlist{needed_nodes=needs_before,is_barred=is_afterargs}

               datatype comp_type=(* Gives information about operators which require special
                  treatment *)
                  NO (* Operator does not carry a constant and isn't new *)
               |  NEW (* operator is new *)
               |  YES of Constants.constant*bool
               (* Operator does carry this constant.  The bool is TRUE if the operator is
                  also commutative *)
               fun get_comp_type comp=
                  (case comp of
                     add_constant c => YES(c,true)
                  |  mul_constant c => YES(c,true)
                  |  divide_constant c => YES(c,false)
                  |  rem_constant c => YES(c,false)
                  |  new _ => NEW
                  |  _ => NO
                  )

               fun nulfun _={}
               val (do_before,do_after)=
(* do_before and do_after are functions to be done before and after to implement special treatment. *)
                  (case get_comp_type comp of
                      NO => (nulfun,nulfun)
                  |   YES(c,false) => (nulfun,fn ()=>do_const c)
                  |   YES(c,true) =>
                         if !Variables.push_constants_before
                         then
                            (fn ()=>do_const c,nulfun)
                         else
                            (nulfun,fn ()=>do_const c)
                  |   NEW =>
                         (* We create a new object of this class and dup it (the latter is
                            unnecessary if we create a new object just to call the initialisation
                            method on it but hopefully that is rare). *)
                         (fn ()=>
                         let
                            val SOME(F(0,CLASS c))=jvt
                            val _= stack_words:= !stack_words+2
                         in
                            instructions:=
                               List.revAppend(
                                  [Code.new c,
                                   Code.dup
                                   ],!instructions
                                   )
                         end,
                         nulfun)
                  )

               val ()=do_before()

               (* Push the args *)
               val ()= List.app (fn arg=>do_push(arg,true)) args

               (* Do the remaining needs. *)
               fun is_barred node'=Dag.inorder(node',node)
               val ()=do_nlist{needed_nodes=needs_after,is_barred=is_barred}                

               val ()=do_after()

               (* OK - the arguments are now in place on the stack.  Perform the operation! *)

               (* first some abbreviations *)
               fun a()=valOf jvt
               fun w()=widen(valOf jvt)
               fun getjtin()=let val N(SOME jtin,_):: _ = args in jtin end
               fun drop_dim(Types.F(n,bt))=Types.F(n-1,bt)

               val _=record_stack()
               val _=record_absolute_stack initial_stack_words

               val new_instructions =
               (* new_instructions are the instructions to be done.  There are not reversed (like
                  instructions) but in the order in which they are to be done.  Where we push required
                  constants onto the stack we *)
               (case comp of
                  add => [Code.add(w())]
               |  sub => [Code.sub(w())]
               |  neg => [Code.neg(w())]
               |  mul => [Code.mul(w())]
               |  divide _ => [Code.divide(w())]
               |  rem _ => [Code.rem(w())]
               |  add_constant _ =>[Code.add(w())]
               |  mul_constant _ =>[Code.mul(w())]
               |  divide_constant _ =>[Code.divide(w())]
               |  rem_constant _ =>[Code.rem(w())]
               |  bitwise_or =>[Code.bitwise_or(w())]
               |  bitwise_and=>[Code.bitwise_and(w())]
               |  bitwise_xor=>[Code.bitwise_xor(w())]
               |  shl=>[Code.shl(w())]
               |  shr=>[Code.shr(w())]
               |  ushr=>[Code.ushr(w())]
               |  convert _=>
                     (* This is the first tricky one - we need to find out the type of the argument *)
                  let
                     val jtin=getjtin()
                     val argin=jtin
                     val argout=a()
                  in
                     if Types.java_type_equal(widen argin,argout)
                     then [] (* Conversion is not necessary *)
                     else
                        (case (issmall argin,issmall argout) of
                           (false,false) => [Code.convert(argin,argout)]
                        |  (false,true) =>
                              if isint argin
                              then [Code.convert(argin,argout)]
                              else [Code.convert(argin,int_type),
                                    Code.convert(int_type,argout)]
                        |  (true,false) =>
                               [Code.convert(int_type,argout)]
                        |  (true,true) =>
(* we implement the equivalent of sections 5.1.2/5.1.3 of the
   Java language specification.  Widening conversions cost nothing
   and can be ignored. *)
                           let
                              fun widen()=[Code.convert(int_type,argout)]
                              fun narrow()=[]
                           in
                              (case (argin,argout) of
                                 (F(0,CHAR),F(0,BYTE)) => widen()
                              |  (F(0,CHAR),F(0,SHORT)) => widen()
                              |  (F(0,SHORT),F(0,BYTE)) => widen()
                              |  (F(0,SHORT),F(0,CHAR)) => widen()
                              |  _ => narrow()
                              )
                           end
                        )
                  end
               |  sign_extend =>
                  let
                     val jtin=getjtin()
                  in
                     [Code.convert(int_type,jtin)]
                  end
               |  aload _ => [Code.aload(a())]
               |  astore _ => [Code.astore(drop_dim(getjtin()))]
               |  cmp b =>
                  let
                     val argin=getjtin()
                  in
                     if isint argin then
                     let
                        val _=record_stack' 2
                        val L1=Labels.new_label()
                        val L2=Labels.new_label()
                        val L3=Labels.new_label()
                        fun p i=Code.push(Constants.INT(JavaInt.fromInt i))
                     in
                        [
                        Code.dup2,
                        Code.cond(Tests.lt,int_type,L2),
                        Code.cond(Tests.eq,int_type,L3),
                        p 1,
                        Code.goto L1,

                        Code.L L3,
                        p 0,
                        Code.goto L1,

                        Code.L L2,
                        Code.pop2,
                        p ~1,
                        Code.L L1
                        ]
                     end
                     else
                        [Code.cmp(argin,b)]
                  end

               |  new m => [Code.invoke_special m] 
               (* The actual newing and dupping should have been done in do_before *)
               |  newarray(jt as Types.F(array_dim,_),_)=>
                  let
                     val n_args= List.length args
                  in
                     if array_dim=1
                     then [Code.anewarray jt]
                     else
                        if n_args=1 andalso !Variables.do_1dim_as_newarray then
                           [Code.anewarray jt]
                        else
                           [Code.multianewarray(jt,n_args)]
                  end
               |  arraylength _ => [Code.arraylength]
               |  getfield (f,_) => [Code.getfield f]
               |  putfield(f,_) => [Code.putfield f]
               |  getstatic(f,_) => [Code.getstatic f]
               |  putstatic(f,_) => [Code.putstatic f]
               |  invoke_interface m =>
                  let
                     val total_arg_size=
                        List.foldl
                           op+
                           0
                           (List.map
                              (fn arg=>Types.java_type_size(valOf(typeOf arg)))
                              args
                              )
                  in
                     [Code.invoke_interface(m,total_arg_size)]
                  end
               |  invoke_special m => [Code.invoke_special m]
               |  invoke_static m => [Code.invoke_static m]
               |  invoke_virtual m => [Code.invoke_virtual m]
               |  checkcast (h,_) => [Code.checkcast h]
               |  instanceof h => [Code.instanceof h]
               |  monitorenter _ =>
                     if !Variables.do_monitors
                     then [Code.monitorenter]
                     else raise Fail
"Ordering not yet defined for monitors.  If you must then set Variables.do_monitors at your own risk."
               |  monitorexit _ =>
                     if !Variables.do_monitors
                     then [Code.monitorexit]
                     else raise Fail
"Ordering not yet defined for monitors.  If you must then set Variables.do_monitors at your own risk."
               |  vale _ => raise Fail "vale in do_push!!"
               )

               val _ = instructions:= List.revAppend(new_instructions,!instructions)

(* Operation is now done.  The stack size must be that when this function was called plus
   the type of the result, assuming that the arguments to everything were of the right type
   *)
               val res_size=Types.java_void_type_size jvt
               val _ = stack_words:= initial_stack_words+res_size
               val _ = record_stack()

               val _ = set_status(node,DONE)
(* Now take a copy of this node if storeit and it is used more than once. *)
               val _ =
                  if storeit then
                     (case usage node of
                        USEDMANY _ =>
                          (* dup the result and put it in local number !next_local  *)
                          (stack_words:= !stack_words+res_size ;
                           instructions:=
                             (case res_size of
                                 1 => Code.dup
                             |   2 => Code.dup2
                             |   _ => raise Fail
"Bug - USEDMANY for void node"
                             ):: !instructions ;
                           do_store(node,!next_local) ; (* this sets the node's status *)
                           next_local:= !next_local +1
                           )
                     |  _ => {}
                     )   
                  else {}  
            in     
               {}
            end) (* end of do_push! *)

         and do_needs node=
            (* If the node has not been done, do it, and store the result in a local variable if
               necessary. *)
         (case status node of
            TODO =>
               (do_push (node,false);
               (case usage node of
                  UNUSED =>
                     (case Types.java_void_type_size(typeOf node) of
                        0 => {}
                     |  1 =>
                       (instructions:= Code.pop:: !instructions;
                        stack_words:= !stack_words-1
                        )
                     |  2 =>
                       (instructions:= Code.pop2:: !instructions;
                        stack_words:= !stack_words-2
                        )
                     )
               |  _ =>
                     (case typeOf node of
                        NONE => {}
                     |  SOME _ =>
                       (do_store(node,!next_local);
                        next_local:= !next_local+1
                        )
                     )
               ))
         |  _ => {}
         ) (* end of do_needs *)

         fun do_vale node=
         let
            (* For now we simply merge the label args with the needs list and do_needs them all. *)
            val N(_,Operation {comp=vale {n_args=n},needs=needs,args=args,...})=node
            val stack_args=List.take(args,n)
            val lab_args=List.drop(args,n)
            val all_to_needs=List.rev(Sortnode.sort(lab_args@needs))
            val _=List.app (fn arg=>do_push(arg,true)) stack_args
            val _=List.app do_needs all_to_needs
            val _=record_stack()
         in
            {}
         end

         (* Now do the translation *)
         val _=do_vale vale_node

         (* Everything is now computed except the data for getargs and gettype *)

         (* types are stored in typearray *)
         val typearray=
            Array.tabulate
               (!next_local - first_local,
               (fn i=>
                  (case Array.sub(nodes_for_locals,i) of
                     SOME node => valOf(typeOf node)
                  |  NONE =>
(* This can happen when there is a block argument or a thrown object which is either unused, or else
   only used in an exception label. *)
                        if i<n_args
                        then (* type of argument i *)
                           List.nth(input,i)
                        else
                           if i>n_args orelse not is_handler then
                              raise Fail
"???? Bug in basic block code; unused local"
                        else
(* this is a thrown object which is unused or only used in an exception label.  Set its type to object *)
                           Types.F(0,Types.CLASS(ClassHandle.object))

                  )))

         fun gettype n=Array.sub(typearray,n-first_local)
         (* Now for args to blocks in labels *)
         val N(_,Operation{comp=vale{n_args=n},args=args,...})=vale_node
         val block_args=List.drop(args,n)
(* block_args should contain all the operations referred to in the vale instruction (but not
   arguments or thrown objects which are only referred to in exception labels). *)
         val argmap=
(* Construct a map containing the operations referred to in block_args *)
            List.foldl
               (fn (node as N(_,Operation _),map_so_far)=>
               let
                  val LOCAL i=status node
               in
                  Mapnode.insert(map_so_far,node,i)
               end
               |   (_,map_so_far) => map_so_far
               )
               Mapnode.empty
               block_args
         fun getargs node=
            (case node of
               N(_,Operation _) => valOf(Mapnode.find(argmap,node))
            |  N(_,Arg i) => first_local+i
            |  N(_,Thrown) => first_local+n_args
            )

         val instructions1=List.rev(!instructions)

         (* If Variables.do_elision, remove redundant load store pairs in instructions1.  
            A load store pair is redundant when
            1) instructions1 contains store j stack_size i . . . stack_size i load j
               (NB that all stores are followed by a stack_size and all loads preceded by one)
            2) j is not loaded anywhere else (in particular this means that j must be
               USEDONCE or USEDMANY 2).
            3) There is no stack_size instruction between the store and the load with an
               argument < i.
            4) The local variable is not needed after the vale instruction
            *)

         (* We compute this is a somewhat crude way, using a map f from ints to ints.
            If f(i) exists it means we have encountered a store for local i and it is not
            yet eliminated from consideration for elision.  If f(i)>=0 it means the stack size
            for the store was f(i).  If f(i)<0 it means we have encountered a load for i
            with matching stack size, with no intervening smaller stack, and the only problem
            is that there might be another load to come.  At the end, the stores and loads to
            be elided are those for which f(i)<0.

            We also maintain max_stack_so_far, the maximum value of f.
            *)
         val change_map=
            if !Variables.do_elision then
            let
               fun recompute_max map=
                  IBM.foldl
                     Int.max
                     ~1
                     map
   
               fun do_ss(m as (map_so_far,max_stack_so_far),ss)=
               (* handle a stack_size instruction with argument ss *)
                  if ss>=max_stack_so_far
                  then m
                  else
                     let
                        val new_map=
                           IBM.filter
                              (fn s => s<=ss)
                              map_so_far
                        val new_max=recompute_max new_map
                     in (new_map,new_max)
                     end
   
               fun compute_local_map(m1 as (map1,max1),remaining_instructions)=
                  (case remaining_instructions of
                     Code.store(ar,j)::Code.stack_size s::rest =>
                     let
                        val m2 as (map2,max2)=do_ss(m1,s)
                        val is_possible=
                           (case usage(node_of j) of
                              USEDMANY 2 => true
                           |  USEDONCE _ => true
                           |  _ => false
                           )
                        val m3=
                           if not(is_possible) then m2
                           else
                              (IBM.insert(map2,j,s),Int.max(s,max2))
                     in
                        compute_local_map(m3,rest)
                     end
                  |  Code.stack_size s::Code.load(ar,j)::rest =>
                     let
                        val m2 as (map2,max2)=do_ss(m1,s)
                        val m3 =
                           (case IBM.find(map2,j) of
                               NONE =>
                                  m2
                           |   SOME s2 =>
                                  if s=s2
                                  then
                                  let
                                     val new_map=IBM.insert(map2,j,~1)
                                  in
                                     (new_map,if s=max2 then recompute_max new_map else max2)
                                  end
                                  else
                                  let
                                     val (new_map,_)=IBM.remove(map2,j)
                                  in
                                     (new_map,if s=max2 then recompute_max new_map else max2)
                                  end
                           )
                     in
                        compute_local_map(m3,rest)
                     end
                  |  Code.ret j::rest => raise Fail "ret not yet sorted out"
   
                  |  Code.stack_size s::rest =>
                        compute_local_map(do_ss(m1,s),rest)
                  |  _ ::rest =>
                        compute_local_map(m1,rest)
                  |  [] => map1
                  )
   
               val change_map1= compute_local_map((IBM.empty,~1),instructions1)
               (* We come to the code which removes locals from the change map if they occur
                  as an argument to the needs list *)
               val N(_,Operation {comp=vale {n_args=n},args=vale_args,...})=vale_node
               val vale_locs=List.drop(vale_args,n)
               fun remove_arg(arg,map_so_far)=
                  (case status arg of
                     LOCAL i =>
                        #1(IBM.remove(map_so_far,i)) handle LibBase.NotFound => map_so_far
   
                  |   _ => map_so_far
                     )
                val change_map2=
                   List.foldl
                      remove_arg
                      change_map1
                      vale_locs
            in
               change_map2
            end
            else IBM.empty

         fun can_elide loc=
            (case
               IBM.find(change_map,loc) of
                  SOME s => (s<0)
               |  NONE => false
               )

         (* Now transform the instructions, applying the changes.  We also need to compute the
            maximum and final number of stack words required. *)
         val final_stack_words= !stack_words (* this is unchanged by the elisions *)
         val (rev_instructions2,offset,max_stack_words)=
            List.foldl
               (fn (ins,(i_sf,o_sf,m_sf)) =>
                  (case ins of
                     Code.stack_size s => (i_sf,o_sf,Int.max(m_sf,s+o_sf))
                  |  Code.load(ar,loc) =>
                        if can_elide loc
                        then
                           (i_sf,o_sf-java_type_size ar,m_sf)
                        else
                           (ins::i_sf,o_sf,m_sf)
                  |  Code.store(ar,loc) =>
                        if can_elide loc
                        then
                           (i_sf,o_sf+java_type_size ar,m_sf)
                        else
                           (ins::i_sf,o_sf,m_sf)
                  |  _ => (ins::i_sf,o_sf,m_sf)
                  )
               )
               ([],0,0)
               instructions1

         val () = if offset<>0 then raise Fail "Mysterious offset" else {}
         val instructions2=List.rev rev_instructions2
      in
        {instructions=instructions2,
         max_stack_words=max_stack_words,
         final_stack_words=final_stack_words,
         first_local=first_local,
         next_local= !next_local,
         getargs=getargs,
         gettype=gettype
         }
      end (* precomp *)

      fun move_locals(
         move_list:{from:int,to:int,java_type:Types.java_type} list,
         const_list:{from:Constants.constant,to:int} list,
         initial_stack_size)=
      let
(* At the moment we do this naively, by pushing everything onto the stack and taking it off again.
   There are more sophisticated strategies but they require care with double-word quantities. *)
         val move_instructions=
            List.revAppend(
               List.map (fn ml=>Code.load(widen(#java_type ml),#from ml))
               move_list
               ,
               List.map (fn ml=>Code.store(widen(#java_type ml),#to ml))
               move_list
               )

         val const_instructions=
            List.foldl
               (fn (cl,i_so_far)=>
                  Code.push(#from cl)::Code.store(widen(Constants.typeOf(#from cl)),#to cl)::
                     i_so_far)
               []
               const_list

         val max_stack_words=
            initial_stack_size+
               Int.max(
                  List.foldl
                     (fn (ml,sum_so_far)=>sum_so_far+Types.java_type_size(#java_type ml))
                     0
                     move_list,
                  List.foldl
                     (fn (cl,max_so_far)=>
                        Int.max(Types.java_type_size(Constants.typeOf(#from cl)),max_so_far))
                     0
                     const_list
                  )
      in
         {instructions=move_instructions@const_instructions,max_stack_words=max_stack_words}
      end (* move_locals *)

      fun do_exit{block=Dag.DagB{exit=exit,vale=N(_,Operation{args=args,comp=vale{n_args=n},...}),...},
         getlabel}=
      let
         fun getid((_,_,id):Dag.DagLabel)=id
         val classify=getlabel o getid

         fun argtypes()=List.map (widen o valOf o typeOf) (List.take(args,n))
            (* types of the arguments on the stack *)
         fun i1 instructions={instructions=instructions,max_stack_words=0} (* common case *)

         fun do_inner_exit exit=
            (case exit of
               goto lab=>
                  (case classify lab of
                     SOME label => i1[Code.goto label]
                  |  NONE => i1[]
                  )
            |  athrow => i1[Code.athrow]
            |  return =>
                  (case argtypes() of
                     [] => i1[Code.return_void]
                  |  [res] => i1[Code.return res]
                  )
            |  lookupswitch {lookuptable: (JavaInt.t*DagLabel) list,default:DagLabel}=>
               let
                  val after_label=Labels.new_label()
                  (* We put this after the instructions, as we have to supply a label anyway *)
                  fun must_get lab=getOpt(classify lab,after_label)
               in
                  i1[Code.lookupswitch
                    {lookuptable=
                        List.map
                           (fn (ji,lab)=>(ji,must_get lab))
                           lookuptable,
                     default=must_get default
                     },
                     Code.L after_label
                     ]
               end
            |  tableswitch {jumptable:DagLabel list,not_in_interval:DagLabel,low:JavaInt.t}=>
               let
                  val after_label=Labels.new_label()
                  (* We put this after the instructions, as we have to supply a label anyway *)
                  fun must_get lab=getOpt(classify lab,after_label)
               in
                  i1[
                     Code.tableswitch
                       {low=low,
                        high=JavaInt.numops.add(low,JavaInt.fromInt(List.length jumptable - 1)),
                        jumptable=
                           List.map must_get jumptable,
                        not_in_interval=must_get not_in_interval
                        },
                     Code.L after_label
                     ]
               end
            |  cond {test,yes,no}=>
               let
                  fun test_and_jump{test:Tests.test,lab:Labels.label,yes_if_nan:bool}=
                  (* returns instructions and max_stack_words for
                     the operation (if test then goto lab).  If yes_if_nan, we also go to
                     the label if either of the arguments is a NaN *)
                  let
                     val atype= hd(argtypes())
                  in
                     if isint atype orelse isref atype then
                        i1[Code.cond(test,atype,lab)]
                     else if islong atype then
                        i1[Code.cmp(atype,true),Code.cond0(test,lab)]
                     else if test=Tests.eq orelse test=Tests.ne then
                        (* Yikes!  Floating point equality! *)
                        if (test=Tests.ne andalso yes_if_nan) orelse
                           (test=Tests.eq andalso not(yes_if_nan))
                           then i1[Code.cmp(atype,true),Code.cond0(test,lab)]
                        else (* I give up *)
                           raise Fail
   "Sorry - testing for floating point equality with NaNs counting as equal is not implemented."
                      else (* floating point comparisons which don't involve equality *)
                           i1[Code.cmp(atype,
                              yes_if_nan <> (test=Tests.le orelse test=Tests.lt)
                              ),Code.cond0(test,lab)]
                  end
               in
                  (case (classify yes,classify no) of
                     (NONE,NONE)=>
                         (* comparison has null effect; why? *)
                        i1(case java_type_size(hd(argtypes())) of
                              1 => [Code.pop2]
                           |  2 => [Code.pop2,Code.pop2]
                           )
                  |  (SOME lab,NONE)=>test_and_jump{test=test,lab=lab,yes_if_nan=false}
                  |  (NONE,SOME lab)=>test_and_jump{test=Tests.negate test,lab=lab,yes_if_nan=true}
                  |  (SOME laby,SOME labn)=>
                     let
                        val {instructions=instructions,max_stack_words=max_stack_words}=
                           test_and_jump{test=test,lab=laby,yes_if_nan=false}
                     in
                        {instructions=instructions@ [Code.goto labn],max_stack_words=max_stack_words}
                     end
                  )
               end
            |  cond0 {test,yes,no} => (* The comparison should be of an integer or a reference *)
               let
                  fun cond0ref(test,lab)=
                     if test=Tests.eq then Code.ifnull lab
                     else if test=Tests.ne then Code.ifnonnull lab
                     else raise Fail "Invalid comparison of reference arguments"
                  val atype=hd(argtypes())
                  val cond0_to_use=
                     if isint atype 
                     then Code.cond0
                     else if isref atype then cond0ref               
                     else raise Fail "Cond0 for arg not integer or ref"
               in
                  (case (classify yes,classify no) of
                     (NONE,NONE)=>i1[Code.pop]
                  |  (SOME lab,NONE)=>i1[cond0_to_use(test,lab)]
                  |  (NONE,SOME lab)=>i1[cond0_to_use(Tests.negate test,lab)]
                  |  (SOME laby,SOME labn)=>
                                      i1[cond0_to_use(test,laby),Code.goto labn]
                  )
               end
            )
      in
         do_inner_exit exit
      end (* do_exit *)
   end (* local *)
end (* Precomp *)
