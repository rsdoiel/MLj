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

(* Wire:>WIRE constructs and manipulates connections between basic block
   Dags. *)
structure Wire:>WIRE=
struct
   local
(*
(* DEBUG *)
      fun say s= #say(!Compiler.Control.Print.out) s
*)
      open Dag
  
      structure IIPIS=IntIntervalPartialIntSet
      
      structure OP=OrthogonalPartition(IIPIS)

      (* We have some structures which are to be used in the MapInt functor for
         finding intersections of local copy and const lists as input to
         Precomp.move_locals. In each case the ordering is on the to field. *)
      structure CopyKey:>ORD_KEY 
         where type ord_key={from:int,to:int,java_type:Types.java_type}=
      struct
         type ord_key={from:int,to:int,java_type:Types.java_type}
         fun compare(x:ord_key,y:ord_key)=Int.compare(#to x,#to y)
      end
      structure CopyMapInt=MapInt(structure K=CopyKey)

      structure IBM=IntBinaryMap

      structure ConstKey:>ORD_KEY 
         where type ord_key={from:Constants.constant,to:int} =
      struct
         type ord_key={from:Constants.constant,to:int}
         fun compare(x:ord_key,y:ord_key)=Int.compare(#to x,#to y)
      end
      structure ConstMapInt=MapInt(structure K=ConstKey)

      structure Matrix=Sparse (* we use sparse matrices *)
   in
      type DBvec=Dag.DagBlock Vector.vector * int
      (* all the DagBlocks in a method & the highest label number *)

      fun block_id(Operations.B(i,_))=i
      fun dblock_id(Dag.DagB{id=id,...})=id

      structure block_key:ORD_KEY=
      struct
         type ord_key=Operations.block
         fun compare(b1,b2)=Int.compare(block_id b1,block_id b2)
      end

      structure BlockDFS=DFS(SplaySetFn(block_key))
      structure BlockMap=SplayMapFn(block_key)
      structure IBS=IntBinarySet
      structure IDFS=DFS(IBS)

      fun wire(block)=
      (* Strategy - use MakeDag.make_dag to do a depth-first search on the
         blocks, keeping a map from blocks to compiled blocks. *)
      let
         val label_counter=ref 0

         fun dfsfun(blk,(map_so_far,patches,id_so_far,list_dbs))=
         let
            val (new_db,new_patches)=MakeDag.make_dag(id_so_far,blk,label_counter)
            val new_map=BlockMap.insert(map_so_far,blk,new_db)
            val neighbours=List.map #1 new_patches
         in
            (neighbours,(new_map,new_patches::patches,id_so_far+1,new_db::list_dbs))
         end
         val (map,patches,_,list_dbs)=BlockDFS.dfs_combined dfsfun (BlockMap.empty,[],0,[]) block
         (* Do the patches.  list_dbs is the list of Dag blocks, in reverse order of numbering  *)
         val _ =
            List.app
               (fn (blk,dboref) => dboref:= BlockMap.find(map,blk))
               (List.concat patches)
      in
         (Vector.fromList(List.rev list_dbs),!label_counter)
      end

      fun db_toString((vec,_),i)=
         Dag.DagBlock_toString(Vector.sub(vec,i))

      type label_data= (* this is the data returned by find_children for each label of a block *)
        {destination:Dag.DagBlock,
         kind:Variables.label_kind,
         arguments:Dag.DagNode list, (* arguments supplied to destination, in order *)
         id:int (* Number of the label *)
         }

      fun find_children(Dag.DagB {exit=exit,exceptions=exceptions,...})=
      let
         val exit_list=
            (case exit of
               cond {test,yes,no} =>
                  [(yes,Variables.TEST test),(no,Variables.TEST (Tests.negate test))]
            |  cond0 {test,yes,no} =>
                  [(yes,Variables.TEST test),(no,Variables.TEST (Tests.negate test))]
            |  goto lab =>
                  [(lab,Variables.GOTO)]
            |  lookupswitch {lookuptable,default} =>
               let
                  val tablelen=List.length lookuptable
               in
                  (default,Variables.SWITCH(tablelen,false))::
                  List.map
                     (fn (_,lab)=>(lab,Variables.SWITCH(tablelen,true)))
                     lookuptable
               end
            |  tableswitch {jumptable,not_in_interval,...} =>
               let
                  val tablelen=List.length jumptable
               in
                  (not_in_interval,Variables.SWITCH(tablelen,false))::
                  List.map
                     (fn lab => (lab,Variables.SWITCH(tablelen,true)))
                     jumptable
               end
            |  athrow => []
            |  return => []
            )
         val exception_list=
            List.map
               (fn Dag.DagE(_,lab)=>(lab,Variables.EXCEPTION))
               exceptions

      in
         List.map
            (fn ((dbref,arguments,id),kind)=>
              {destination=valOf(!dbref),
               kind=kind,
               arguments=arguments,
               id=id})
            (exit_list @ exception_list)
      end


      (* find_paths and path_from are used for determining whether there is a path from one block
         to another.  The idea is to construct one path_data object for the whole method; then
         path_from(A,B) should compute rapidly if there is a control path from A to B.  path_from returns
         TRUE if A=B.
         *)
      (* find_paths currently finds the set of descendants of each block.  This is far from optimal. *)
      type path_data=IBS.set Vector.vector

      fun find_paths vec=
      let
         fun descendants blkid=
            IDFS.simple_dfs
               (fn i=>List.map (dblock_id o #destination) (find_children(Vector.sub(vec,i))))
               (fn (i,set_so_far)=>IBS.add(set_so_far,i))
               IBS.empty
               blkid
      in
         Vector.tabulate(Vector.length vec,fn i=>descendants i)
      end

      fun path_from vec (A,B)=IBS.member(Vector.sub(vec,dblock_id A),dblock_id B)

      fun find_freqs(vec,_)=
      let
         (* Strategy.  For each DagBlock we use find_children and Variables.weightlabel
            to estimate relative probabilities of going to other blocks.  We then normalise these
            real numbers to sum to (1-!Variables.leakage) and it is one column of a matrix A.  We
            use matrix.invertonevec to find (I-A)^{-1}(e_1); this is the answer.  There are two
            reasons for leakage: (1) it corresponds to the probability of leaving the block by some
            other method; eg by an exception; (2) we need it anyway so that the matrix I-A is actually
            invertible and not very ill-conditioned.  If there was a loop in the blocks with no
            escape, and we normalised to 1, the answers would naturally be infinite and we can't allow
            this kind of thing.  As it is, we know that the l1 norm of A is at most 1-leakage, therefore
            the l1 norm of I-A is at least leakage, and so (with 64-bit floats) it seems reasonable to
            hope if not believe that the answers will not be too crazy. *)

         val nblocks=Vector.length vec
         val matrix=Matrix.create(nblocks,nblocks)
         val pd=find_paths vec

         fun compute_exits block=let
            (* compute_exits returns a list of pairs (real,label_data) for the block,
               where the reals are normalised. *)
            val unnormalised_exits=
               List.map
                  (fn child=>
                    ((!Variables.weightlabel)(#kind child,path_from pd (#destination child,block)),
                      child
                      ))
                  (find_children block)
            val sum=
               List.foldl
                  (fn ((weight,_),sum_so_far)=>sum_so_far+weight)
                  0.0
                  unnormalised_exits
            val multiplier= (1.0- !Variables.leakage)/sum
         in
            List.map
               (fn (weight,lab_data)=>(weight*multiplier,lab_data))
               unnormalised_exits
         end


         val _=
            Vector.appi
               (fn(j,db)=>(* compute column j of I-A *)
                  let
                     val weighted_exits=compute_exits db

                     val _=Matrix.update(matrix,j,j,1.0) (* row j of I *)
                     val _=
(* Subtract entries in weighted_exits. *)
                        List.app
                           (fn (weight,lab)=>let
                              val i=dblock_id(#destination lab)
                           in
                              Matrix.update(matrix,i,j,~weight)
                           end)
                           weighted_exits
                  in
                     {}
                  end
                  )
               (vec,0,NONE)

            val freqvec=Matrix.invertonevec matrix (* this is what takes the time! *)
            fun f1 block=Matrix.vector_sub(freqvec,dblock_id block)
            fun f2 block=compute_exits
         in
            (fn block=>Matrix.vector_sub(freqvec,dblock_id block),compute_exits)
         end (* find_freqs *)

      type internal_compiled=
        {instructions:Code.instruction list Vector.vector,
         max_locals:int,
         max_stack_words:int,
         common_move_instructions:Code.instruction list Vector.vector,
         label_move_instructions:Code.instruction list Vector.vector
         }


      fun do_internal_compile
         {blocks=(blockvec,nlabels),block_freq_fun=block_freq_fun,label_freq_fun=label_freq_fun}:
         internal_compiled =
      let
         val nblocks=Vector.length blockvec
         (* first we compute the compiled code with the virtual local numbers put in by
            precomp. *)
         val virtual_local=ref 0 (* this is where virtual local numbers start at *)
         val precomped=
            Vector.tabulate(nblocks,
               fn i =>
               let
                  val p=Precomp.precomp
                    {db=Vector.sub(blockvec,i),
                     first_local= !virtual_local}
                  val _= virtual_local:= #next_local p
               in
                  p
               end
               )
         val nlocals= !virtual_local
         (* We construct an array mapping locals to the index of the corresponding block in
            precomped *)
         val blocks_for_locals=Array.array(nlocals,~1)
         val _=
            Vector.appi
               (fn (blockno,pc)=>
                  Array.modifyi
                     (fn _ =>blockno)
                     (blocks_for_locals,#first_local pc,SOME(#next_local pc - #first_local pc))
                  )
               (precomped,0,NONE)

         fun local_gettype n= #gettype(Vector.sub(precomped,Array.sub(blocks_for_locals,n))) n
         (* For each local, compute from & to such that for all integers i in [from,to), the 
            local has been written to or is an argument or thrown, and has yet to be used for the
            last time, at the point immediately after instruction i, numbering instructions from 0.

            We check that every local is stored at most once.

            iincs do not affect liveness and therefore are ignored.

            There are two wrinkles to beware of.

            Firstly, method arguments which are unused nevertheless require a
            separate local (block arguments which are unused should only occur where they head a
            method).  Secondly, some locals created by Precomp.precomp are actually not used
            at all (because they are elided).  We deem method arguments to be written at
            instruction number ~2, so that from is ~2 for them, and other block arguments to be
            written at instruction number ~1, so that from is ~1 for them.  In fact the
            default value of from for everything apart from method arguments is ~1, though it will
            be altered later on for other things when there is a store instruction.  The default value
            of to is always ~1, but is set to the number of the last read instruction after that.  So
            for unused method arguments [from,to) will be [~2,~1), which is non-empty so they will
            not get clobbered, but for unused block arguments [from,to) will be [~1,1), which is empty,
            so they may get clobbered.  

            Secondly, arguments in exits or exceptions from the block must not be clobbered.  So we deem
            these to be read as if by an instruction added at the end of the block.
            *)
         val is_live_from_array=Array.array(nlocals,~1)
         (* set is_live_from_array[i] to ~2 for arguments to block 0. *)
         val Dag.DagB{descriptor=Operations.D{input,...},...}=Vector.sub(blockvec,0)
         val _=
            Array.modifyi
               (fn _ => ~2)
               (is_live_from_array,0,SOME(length input))

         val is_live_to_array=Array.array(nlocals,~1)

         fun note_write(lno,i)=(* instruction number i (counting from 0) writes to the local *)
           if Array.sub(is_live_from_array,lno)<> ~1
           then
              raise Fail "multiple stores in a local"
           else
              Array.update(is_live_from_array,lno,i)

         fun note_read(lno,i)= (* instruction number i (counting from 0) reads from the local *)
            Array.update(is_live_to_array,lno,i)

         val _=
            Vector.appi
               (fn (i,pc) =>
                  let
                     val n_ins=
                        List.foldl
                           (fn (ins,ins_no)=>
                              ((case ins of
                                 Code.load(_,lno) => note_read(lno,ins_no)
                              |  Code.ret(lno) => note_read(lno,ins_no)
                              |  Code.store(_,lno) => note_write(lno,ins_no)
                              |  _ => {}
                              );
                              ins_no+1))
                           0
                           (#instructions pc)
                     (* We must ensure that all locals which are used as arguments are live at the
                        end of the block. *)
                     val _=
                        List.app
                           (fn {arguments,...} =>
                              List.app
                                 (fn (N(_,Constant _))=>{}
                                 |   arg =>
                                     note_read(#getargs pc arg,n_ins)
                                 )
                                 arguments
                              )
                           (find_children(Vector.sub(blockvec,i)))

                  in
                     {}
                  end
                  )
               (precomped,0,NONE)

         fun is_unused i= (* True if i is an unused block argument which isn't a method argument *)
            Array.sub(is_live_from_array,i)= ~1 andalso Array.sub(is_live_to_array,i)= ~1

(*
(* DEBUG *)
val () = say(String.concat(List.tabulate(nlocals,
   fn i => "Local "^Int.toString i^" from "^ Int.toString (Array.sub(is_live_from_array,i)) ^ " to " ^
      Int.toString(Array.sub(is_live_to_array,i)) ^ "\n" )))
*)

         (* precomped contains the data for each DagBlock.  We
            to minimise the amount of copying that is required.  We do this using the
            OP (OrthogonalPartition(IIPIS)) structure. *)
         (* 1.  Construct the node list.  Nodes are labelled with the local number *)
         val node_vec=Vector.tabulate(nlocals,fn i=>Graph.newNode())
         val nodes=List.tabulate(nlocals,fn i=>{node=Vector.sub(node_vec,i),label=i})
         (* 2.  Construct the function passed as the second argument to OP.prefind which
            finds the intitem corresponding to the given node label - in this case the node number.
            *)
         fun get_intitem i=
           (Array.sub(blocks_for_locals,i),
              {from=Array.sub(is_live_from_array,i),
               to=Array.sub(is_live_to_array,i)
               })

         (* 3.  Construct the arcs, with their weightings. *)
         fun arcs_for_ldata start weight ({destination=destination,arguments=arguments,...}:
            label_data)=
         (* The arcs list for one label, given the node=start where the label starts at, and
            the real weight attached to this label *)
         let
            val start_id=dblock_id start
            val dest_id=dblock_id destination
            val dest_local_start= #first_local(Vector.sub(precomped,dest_id))
            val start_getargs= #getargs(Vector.sub(precomped,start_id))

            val (arcs,_)=
               List.foldl
                  (fn (N(_,Constant _),(arcs_sf,argno_sf)) => (arcs_sf,argno_sf+1)
                  |  (argnode,(arcs_sf,argno_sf)) =>
(*
( (* DEBUG *)
  say(
  "\n\nBlock "^Int.toString start_id^" label to "^Int.toString dest_id^
  "\nArgument number "^Int.toString argno_sf ^
  "\nNode is "^Dag.DagNode_brief_toString argnode^
  "Stored in "^Int.toString(start_getargs argnode) ^
  "\nMust go to "^Int.toString(dest_local_start+argno_sf));
*)
                     (({from=Vector.sub(node_vec,start_getargs argnode),
                        to=Vector.sub(node_vec,dest_local_start+argno_sf)},
                       weight)::arcs_sf,argno_sf+1))
(*
)
*)
                  ([],0)
                  arguments
         in
            arcs
         end

         fun arcs_for_db db=
         (* The arcs list for one DagBlock *)
            List.concat
               (List.map
                  (fn (weight,ld)=>arcs_for_ldata db (weight+ !Variables.code_copy_cost) ld)
                  (label_freq_fun db))

         val arcs= (* the complete arcs list *)
            List.concat
               (List.tabulate(nblocks,fn bno=>arcs_for_db(Vector.sub(blockvec,bno))))

         (* Now find the partition *)
         local
            val nodeGraph=Graph.makeGraph(Graph.pre{nodes=nodes,arcs=[]})
         in
            val variables:(int list * IIPIS.intset) list =
            (* each list contains a group of locals to be amalgamated into one variable.  
               The intset contains the set of points at which the variable is live. *)
               List.map 
                  (fn (nlist,intset) =>
                     (List.map
                        (fn node => valOf(Graph.nodeLabel(nodeGraph,node)))
                        nlist
                     ,
                     intset
                     ))
                  (OP.prefind(Graph.pre{nodes=nodes,arcs=arcs},get_intitem))

            val nvars=List.length variables
         end

         (* Now do register colouring to allocate the locals *)
         val local_map=Array.array(nlocals,~1)
         (* local_map[i] will contain what local number i gets sent to *)
         val maxvars=2*nvars (* upper bound on the number of local slots required *)
         val usage=Array.array(maxvars,IIPIS.empty)
         (* usage contains the set at which this local is already occupied. *)

         fun v_is2 l=Types.is2(local_gettype l)
         fun l_is2(l:: _)=v_is2 l 
         (* true if variable with these locals has doubleword type *)
         
         fun allocate(lno,(nlist,intset))=

         (* allocate the variable (nlist,intset) to local number lno *)
         let
            val ()=
               List.app
                  (fn loc => Array.update(local_map,loc,lno))
                  nlist
            fun mark_used lno=
               Array.update(usage,lno,
                  IIPIS.union(Array.sub(usage,lno),intset))

            val ()=mark_used lno
            val is2=l_is2 nlist
            val ()=if is2 then mark_used(lno+1) else ()
         in
            ()
         end  
         
         local   
            val DagB {descriptor=Operations.D {input=method_types,...},...}=Vector.sub(blockvec,0)
         in
            val n_method_args=List.length method_types
         end

         val (method_args,internal_variables)=
         (* divide the variables into method arguments and the rest *)
            List.partition
               (fn(list,_) => List.exists( fn argno => argno<n_method_args) list)
               variables

         (* allocate method arguments *)
         local
            val m_array=Array.array(n_method_args,~1)
            (* m_array[i] is set to the local number for the ith method argument *)
            fun do_args(ano,next_local)=
               if ano>=n_method_args
               then
                  next_local
               else
               let
                  val ()=Array.update(m_array,ano,next_local)
                  val next_local=next_local+(if v_is2 ano then 2 else 1)
                  val ano=ano+1
               in
                  do_args(ano,next_local)
               end
            val next_avail=do_args(0,0)

            fun get_ano(l::rest)=if l<n_method_args then l else get_ano rest
         
            val ()=
               List.app
                  (fn variable as (nlist,_)=>
                      allocate(Array.sub(m_array,get_ano nlist),variable))
                  method_args
         in
            val next_avail=next_avail
         end

         
         val strategy= !Variables.local_merge_strategy

         val prio_variables= 
         (* Variables paired with a number; the lower the number, the earlier it should be
            allocated *)
         (case strategy of
            Variables.NEVER => 
               List.map (fn v as (nlist,_) => (~(List.length nlist),v)) internal_variables
         |  Variables.ALWAYS =>
         (* Compute the degree for each internal variable.
            We cost clashes as follows: 
               single-single costs 1
               single-double costs 2
               double-double costs 3
            *)
               IIPIS.intersects_table{
                  contents=internal_variables,
                  eval=(fn (nlist,iset) => (l_is2 nlist,iset)),
                  cost=
                     (fn (b1,b2) => 
                        (case(b1,b2) of 
                           (false,false)=>1 | 
                           (false,true)=>2  | 
                           (true,false)=>2  | 
                           (true,true)=>3)
                        )
                  }
         |  Variables.SAMETYPE =>
            (* Only clashes with other variables of the same type matter *)
               IIPIS.intersects_table{
                  contents=internal_variables,
                  eval=(fn (nlist,iset) => (local_gettype(hd nlist),iset)),
                  cost=
                     (fn (jt1,jt2) => if Types.java_type_equal(jt1,jt2) then 1 else 0)
                  }
         )

         val variables_inorder=List.map #2 (IntSort.sort_pairs prio_variables)

         (* Now do the allocation *)
         val alloc=
         (case strategy of
            Variables.ALWAYS =>
           (fn (variable as (nlist,iset)) =>
            (* this function allocates the variable to the lowest available degree number *)
            let
               val is2=l_is2 nlist
               fun get_lno i=
               (* returns least available lno >= i *)
                  if IIPIS.intersects(Array.sub(usage,i),iset) 
                  then
                     (* i not available *)
                     get_lno(i+1)
                  else if is2 andalso IIPIS.intersects(Array.sub(usage,i+1),iset)
                  then
                     (* This is a double word quantity and i+1 isn't available either *)
                     get_lno(i+2)
                  else
                     i
   
            in
               allocate(get_lno 0,variable)
            end
            )
         |  Variables.NEVER =>
            let
               val counter=ref next_avail
            in
               fn (variable as (nlist,_)) =>
               let
                  val is_2=l_is2 nlist
                  val () = allocate(!counter,variable)
                  val () = counter:= !counter + (if is_2 then 2 else 1)
               in
                  ()
               end
            end
         |  Variables.SAMETYPE =>
            let
               datatype local_type=
                  T of Types.java_type 
               |  F (* Second word of a double word *)
               |  N (* No type allocated *)

               val typearr=Array.array(maxvars,N)
               fun can_merge(jt:Types.java_type,l:local_type)=
               (case l of 
                  N => true
               |  F => false
               |  T jt2 => Types.java_type_equal(jt,jt2)
               )

               fun set_type(lno,jt)=
               let
                  val is2=Types.is2 jt
                  val ()=Array.update(typearr,lno,T jt)
                  val ()=if is2 then Array.update(typearr,lno+1,F) else ()
               in
                  {}
               end

               (* set type for method args *)
               fun set_marg i=
               if i=0
               then 
                  ()
               else        
                  let
                     val next_i=i-1
                     val ()=set_type(Array.sub(local_map,next_i),local_gettype next_i)
                  in
                     set_marg(i-1)
                  end
               val ()=set_marg n_method_args
            in 
               (fn (variable as (nlist,iset)) =>
               (* this function allocates the variable to the lowest available degree number *)
               let
                  val jt=local_gettype(hd nlist)
                  val is2=Types.is2 jt
                  
                  fun get_lno i=
                  (* returns least available lno >= i *)
                     if not(can_merge(jt,Array.sub(typearr,i))) orelse
                        IIPIS.intersects(Array.sub(usage,i),iset) 
                     then
                        (* i not available *)
                        get_lno(i+1)
                     else if is2 andalso IIPIS.intersects(Array.sub(usage,i+1),iset)
                     then
                        (* This is a double word quantity and i+1 isn't available either *)
                        get_lno(i+2)
                     else
                        i
 
                  val lno=get_lno 0
                  val () = allocate(lno,variable)
                  val () = set_type(lno,jt)
               in
                  ()
               end
               )
            end
         )

         (* Now allocate all the variables *)
         val ()=List.app alloc variables_inorder
    
         fun new_lno old_lno=Array.sub(local_map,old_lno)
         (* Compute the total number of locals used *)
         val max_locals=
         let
            fun downfrom n=
            let
               val new_n=n-1
            in
               if IIPIS.is_empty(Array.sub(usage,new_n))
               then
                  downfrom new_n
               else
                  n
            end
         in
            downfrom(Array.length usage) 
               handle Subscript => 0 (* no locals used at all! *)
         end     

         (* Now compute the modified instructions *)
         val instructions=
            Vector.tabulate(nblocks,
               fn i =>
                  List.map
                     (fn Code.load(a,lno)=>Code.load(a,new_lno lno)
                     |   Code.store(a,lno)=>Code.store(a,new_lno lno)
                     |   Code.iinc(lno,ji)=>Code.iinc(new_lno lno,ji)
                     |   Code.ret lno=>Code.ret(new_lno lno)
                     |   other=>other
                     )
                     (#instructions(Vector.sub(precomped,i)))
               )

         (* Do peepholing *)
         val instructions=
            if !Variables.do_peephole
            then
               Vector.map Peephole.peephole instructions
            else 
               instructions

         (* Compute prelim_max_stack_words (NB - the maximum stack
            size may have to increase after we've compiled the exit and copying code) *)
         val prelim_max_stack_words=
            Vector.foldl
               (fn (p,max_so_far)=>Int.max(#max_stack_words p,max_so_far))
               0
               precomped

         fun move_locals_args_for_lab db (ldata:label_data)=
         (* ldata is label data for a label out of db.  Compute move_locals arguments for it.
            (See Precomp.move_locals *)
         let
            val pc_source=Vector.sub(precomped,dblock_id db)
            val pc_dest=Vector.sub(precomped,dblock_id(#destination ldata))
            val (move_list,const_list,_)=
               List.foldl
                  (fn (node,(moves_so_far,consts_so_far,arg_counter))=>
                  let
                     val old_to=arg_counter+ #first_local pc_dest
                  in
                     if is_unused old_to
                     then
                        (moves_so_far,consts_so_far,arg_counter+1)
                     else
                     let
                        val to=new_lno old_to
                     in
                        (case node of
                           N(_,Constant const) =>
                             (moves_so_far,{from=const,to=to}::consts_so_far,arg_counter+1)
                        |  N(SOME jt,_) =>
                             let
                                val from=new_lno(#getargs pc_source node)
                             in
                                (if from=to then moves_so_far else
                                    {from=new_lno(#getargs pc_source node),to=to,java_type=jt} ::
                                    moves_so_far,
                                 consts_so_far,
                                 arg_counter+1)
                             end
                        )
                     end
                  end
                  )
                  ([],[],0)
                  (#arguments ldata)
         in
            (move_list,const_list)
         end

         (* We scan through the labels finding move_locals_args_for_lab for each, and also finding
            which ones are exceptions, putting the results of this in arrays. *)

         (* local_moves_array and local_consts_array will contain the move and const instructions
            to be done for each label.  However to minimise these we do moves and consts which
            are common to all non-exception exits before the exit, and we do this by putting them in
            common_moves_array and common_consts_array. *)

         val local_moves_array=Array.array(nlabels,[])
         val local_consts_array=Array.array(nlabels,[])

         val common_moves_array=Array.array(nblocks,[])
         val common_consts_array=Array.array(nblocks,[])

         (* label_is_exception[i] will be true if label i is an exception label *)
         val label_is_exception=BoolArray.array(nlabels,false)

         val _ =
            Vector.app
               (fn db =>
               let
                  val (local_moves,consts,label_ids)=
                  (* Compute the lists of non-exceptional
                     local moves, constants to push and label ids
                     (IE 3 lists, of the same length, with corresponding entries)
                     Also construct the label_is_exception array and write exception
                     moves/locals (which we don't optimise at all) directly into
                     local_moves_array and local_consts_array.
                     *)
                     List.foldl
                        (fn (ldata,so_far as (moves_so_far,consts_so_far,ids_so_far)) =>
                        let
                           val (moves,consts)=move_locals_args_for_lab db ldata
                        in
                           (case #kind ldata of
                              Variables.EXCEPTION =>
                              let
                                 val _=BoolArray.update(label_is_exception,#id ldata,true)
                                 val _=Array.update(local_moves_array,#id ldata,moves)
                                 val _=Array.update(local_consts_array,#id ldata,consts)
                              in
                                 so_far
                              end
                           |  _ => (moves::moves_so_far,consts::consts_so_far,(#id ldata)::ids_so_far)
                           )
                        end
                        )
                        ([],[],[])
                        (find_children db)
                  (* Use CopyMapInt and ConstMapInt to find the intersections of the
                     local moves and constant push lists *)
                  val (common_local_moves,other_local_moves)=
                     CopyMapInt.multi_intersect(local_moves,fn (x,y)=> #from x = #from y)
                  (* bug fix.  Unfortunately this isn't quite good enough; we must also
                     ensure that no #from of other_local_moves is a #to of the common_local_moves,
                     as otherwise the common move will clobber the other move.  It's taken so long
                     (must be nearly a year) for this to emerge that hopefully this will not happen
                     very often. *)
                  val common_moves_map=
                     List.foldl
                        (fn (clm as {to,...},sf) => IBM.insert(sf,to,clm))
                        IBM.empty
                        common_local_moves
                  (* Commons indexed by to for fast access.  NB tos must all be different *)

                  fun bad_common_moves([],sf as (l_sf,m_sf)) = sf 
                  (* bad common moves and map of still-valid common moves given this list of 
                     local moves *)
                  |   bad_common_moves(h::t,sf as (l_sf,m_sf)) = 
                      let
                         val {from=index,to= _,java_type= _}=h
                         val next=
                           (case IBM.find(m_sf,index) of
                              NONE => sf
                           |  SOME clm => (clm::l_sf,#1(IBM.remove(m_sf,index)))
                           )
                      in
                         bad_common_moves(t,next)
                      end

                  val sf as (bcm,cm_remaining)=
                     List.foldl
                        bad_common_moves
                        ([],common_moves_map)
                        other_local_moves
                  (* bcm contains bad common moves because of locals in other_local_moves.
                     We may now have to take out move common moves because of the new other_local_moves
                     recently added. *)
                  fun iterate (recent_bcms,sf as (bcm,cm_remaining))=
                  (case recent_bcms of
                     [] => sf
                  |  _ =>
                     let
                        val (now_recent,next_map)=bad_common_moves(recent_bcms,([],cm_remaining))
                     in
                        iterate(now_recent,(List.revAppend(now_recent,bcm),next_map))
                     end
                  )

                  val (bad_common_moves,remaining_common_moves)=iterate(bcm,sf)
                  val common_local_moves=IBM.listItems remaining_common_moves
                  val other_local_moves=
                     List.map
                        (fn old_lm => List.revAppend(bad_common_moves,old_lm))
                        other_local_moves
                      

                  val (common_consts,other_consts)=
                     ConstMapInt.multi_intersect(consts,fn (x,y)=> Constants.equal(#from x,#from y,true))
                     (* the "true" means that we ignore the distinctions between ints/booleans/
                        chars/bytes/shorts. *)
                  val _=
                     ListPair.app
                        (fn (moves,id)=>Array.update(local_moves_array,id,moves))
                        (other_local_moves,label_ids)
                  val _=
                     ListPair.app
                        (fn (const,id)=>Array.update(local_consts_array,id,const))
                        (other_consts,label_ids)
                  val _= Array.update(common_moves_array,dblock_id db,common_local_moves)
                  val _= Array.update(common_consts_array,dblock_id db,common_consts)
               in
                  {}
               end
                  )
               blockvec

         val max_stack_so_far=ref prelim_max_stack_words

         val label_move_instructions=
            Vector.tabulate(nlabels,
               (fn i=>
                  let
                     val {instructions=instructions,max_stack_words=max_stack_words}=
                        Precomp.move_locals(
                           Array.sub(local_moves_array,i),
                           Array.sub(local_consts_array,i),
                           if BoolArray.sub(label_is_exception,i) then 1 else 0)
                     val _ =
                        if !max_stack_so_far<max_stack_words
                        then max_stack_so_far:=max_stack_words
                        else {}
                  in
                     instructions
                  end))

         val common_move_instructions=
            Vector.tabulate
               (nblocks,
                   (fn i=>
                   let
                      val {instructions=common_instructions,max_stack_words=common_max_stack_words}=
                         Precomp.move_locals(
                            Array.sub(common_moves_array,i),
                            Array.sub(common_consts_array,i),
                            #final_stack_words(Vector.sub(precomped,i)))
                      val _ =
                         if !max_stack_so_far<common_max_stack_words
                         then max_stack_so_far:=common_max_stack_words
                         else {}
                   in
                      common_instructions
                   end
                   ))
      in
        {instructions=instructions,
         max_locals=max_locals,
         max_stack_words= !max_stack_so_far,
         common_move_instructions=common_move_instructions,
         label_move_instructions=label_move_instructions
         }
      end (* do_internal_compile *)

      datatype borlabid=
         BLOCK of int
      |  LABM of int

      fun resolve_code_motion {blocks=(blockvec,nlabels),label_freq_fun,label_move_instructions}=
      let
         (* We use the HPATH structure *)
         val nblocks=Vector.length blockvec
         (* Construct node list *)
         val blocknode_vec=Vector.tabulate(nblocks,fn _ => Graph.newNode())
         fun getblockn i=Vector.sub(blocknode_vec,i)

         val labnode_array=Array.array(nlabels,NONE)
         fun getlabn i=valOf(Array.sub(labnode_array,i))

         (* Will get set to a node for each label with move instructions attached *)

         val node_list=
            Vector.foldli
               (fn (i,l,nl_so_far)=>
                  (case l of
                     [] => nl_so_far
                  |  _ =>
                     let
                        val node=Graph.newNode()
                        val _ = Array.update(labnode_array,i,SOME node)
                     in
                        {node=node,label=LABM i}::nl_so_far
                     end
                  ))
               (List.tabulate(nblocks,fn i=>{node=getblockn i,label=BLOCK i}))
               (label_move_instructions,0,NONE)

         (* construct an empty graph with these nodes for looking up labels of nodes *)
         val EG=Graph.makeGraph(Graph.pre{nodes=node_list,arcs=[]})
         fun getlab n=valOf(Graph.nodeLabel(EG,n))

         val arc_list=
            Vector.foldli
               (fn (db_id,db,arcs_so_far)=>
                  let
                     val labfreq:(real*label_data) list=label_freq_fun db
                  in
                     List.foldl
                        (fn ((freq,{destination,id,kind,...}),arcs_so_far)=>
                           (case Vector.sub(label_move_instructions,id) of
                              [] => ({from=getblockn db_id,to=getblockn(dblock_id destination)},
                                     freq* (!Variables.jump_cost) kind)::arcs_so_far
                           |  _  => ({from=getblockn db_id,to=getlabn id},
                                       freq*(!Variables.jump_cost) kind)::
                                    ({from=getlabn id,to=getblockn(dblock_id destination)},
                                        freq* !Variables.jump_cost_added)::arcs_so_far
                           ))
                        arcs_so_far
                        labfreq
                  end
                  )
               []
               (blockvec,0,NONE)

      in
         List.map
            (fn node=>getlab node)
            (HPath.prefind(Graph.pre {nodes=node_list,arcs=arc_list},getblockn 0))
      end (* do_resolve_code_motion *)

      fun final_push {blocks=(blockvec,nlabels),
                      internal_compiled={max_locals,max_stack_words=prelim_max_stack_words,
                         instructions,common_move_instructions,label_move_instructions},
                      order} =
      let
         val nblocks=Vector.length blockvec


         
         (* block_pre, block_post, labm_pre contain the label and exception instructions to be
            put before blocks, after blocks, and before
            label move code.  The order of these
            does not matter.  labm_pre[i] is ignored if label i has no movement code. *)
         val block_pre=Array.array(nblocks,[])
         val block_post=Array.array(nblocks,[])
         val labm_pre=Array.array(nlabels,[])

         fun bpre(index,ins)=Array.update(block_pre,index,ins::Array.sub(block_pre,index))
         fun bpost(index,ins)=Array.update(block_post,index,ins::Array.sub(block_post,index))
         fun lpre(index,ins)=Array.update(labm_pre,index,ins::Array.sub(labm_pre,index))


         (* We put some extra code here for dealing with the broken JIT for
            Windows, JDK1.2, which seems not to like exception handlers before
            the clause they handle.  Since we hope this will not be necessary
            for long, we use the following simple strategy: where a handler
            currently is before the end of the block, add an extra
            goto instruction at the end of the method and jump to that instead.
            *)
         val symantec= !Variables.symantec_bug
         (* If true we put the fix in. *)

         (* method_post contains instructions to be put at the end of the method, in
            reverse order.  This feature is only used currently for the Symantec bugfix. *)
         val method_post=ref ([]:Code.instruction list)
         fun mpost ins=(method_post := ins:: (!method_post))

         (* We need to know the order in which things are *)
         val is_before_or_eq=
            if symantec
            then
            let
               val labpos=Array.array(nlabels,0)
               val blockpos=Array.array(nblocks,0)
               fun insert(i,left)=
               (case left of
                  [] => ()
               |  LABM j::left => (Array.update(labpos,j,i);insert(i+1,left))
               |  BLOCK j::left => (Array.update(blockpos,j,i);insert(i+1,left))
               )
               val ()=insert(0,order)
               fun index(BLOCK i)=Array.sub(blockpos,i)
               |   index(LABM i)=Array.sub(labpos,i)
            in
               fn (x,y) => (index x <= index y)
            end
            else
               fn _ => raise (Fail "Wire.is_before_or_eq not available")
         
         (* Set up block and label movement labels *)
         val block_labs=Vector.tabulate(nblocks,fn _ => Labels.new_label())
         val _=
            Vector.appi
               (fn (i,lab)=>bpre(i,Code.L lab))
               (block_labs,0,NONE)

         val lab_labs=Vector.tabulate(nlabels,fn i=>
            (case Vector.sub(label_move_instructions,i) of
               [] => NONE
            |  _ => SOME(Labels.new_label())
            ))

         val _=
            Vector.appi
               (fn (i,labo)=>
                 (case labo of
                    NONE => {}
                 |  SOME lab => lpre(i,Code.L lab)
                 ))
               (lab_labs,0,NONE)

         val label_dest=Array.array(nlabels,~1)
         (* label_dest[i] is about to be set to the block id of the destination of label i *)
         val _=
            Vector.app
               (fn db=>
                  List.app
                     (fn ldata=>Array.update(label_dest,#id ldata,dblock_id(#destination ldata)))
                     (find_children db)
               )
               blockvec

         fun dlid_to_bor dlid=
            (case Vector.sub(lab_labs,dlid) of
               NONE => BLOCK(Array.sub(label_dest,dlid))
            |  SOME _=> LABM dlid
            )
         fun dl_to_bor(_,_,dlid)=dlid_to_bor dlid
         (* given DagLabel, corresponding block or label id to jump to *)

         fun bor_to_lab bor=
         (* given a block or label id, the actual label to jump to *)
            (case bor of
               BLOCK b => Vector.sub(block_labs,b)
            |  LABM l => valOf(Vector.sub(lab_labs,l))
            )

         fun bor_pre(bor,ins)= (* insert ins in preamble of block or label *)
            (case bor of BLOCK i=>bpre(i,ins) | LABM i =>lpre(i,ins))

         (* Do exceptions *)
         val _=
            Vector.app
               (fn DagB{id,exceptions,...}=>
                  ignore(List.foldl
                     (fn (DagE (copt,dlab),priority)=>
                        let
                           val newex=TryCatch.new_exception'(copt,priority)
                           val ()=bpre(id,Code.begin_catch newex)
                           val ()=bpost(id,Code.end_catch newex)
                           val handler=dl_to_bor dlab
                           val ()=
                              if symantec andalso (is_before_or_eq(handler,BLOCK id))
                              then
                              (* do the patch *)
                              let
                                 val extra_label=Labels.new_label()
                              in
                                (mpost(Code.goto extra_label);
                                 mpost(Code.handle_catch newex);
                                 bor_pre(handler,Code.L extra_label)
                                 )
                              end
                              else
                                 bor_pre(handler,Code.handle_catch newex)
                        in
                           priority-1
                        end
                        )
                     0
                     exceptions
                     )
                  )
               blockvec

         val max_stack_words=ref prelim_max_stack_words
         (* It is still possible that max_stack_words may have to go up if some of the exit code
            uses extra stack (at the time of typing though, this is never true) *)

         fun mapnext f l=
         (* mapnext is like map except that it also supplies the next element in the list, if any *)
         let
            fun mn(sofar,l)=
               (case l of
                  [last] => f(last,NONE)::sofar
               | this::(remainder as next::_)=>
                  mn(f(this,SOME next)::sofar,remainder)
               )
         in
            List.rev(mn([],l))
         end

         (* And now, if you please, we'll proceed *)
         val compiled_instructions'= (* Actually a list of lists *)
            mapnext
               (fn (this,nexto) =>
                  (case this of
                     BLOCK i =>
                     (Array.sub(block_pre,i)) @
                     (Vector.sub(instructions,i)) @
                     (Vector.sub(common_move_instructions,i)) @
                     let
                        (* exit code *)
                        fun getlabel i=
                        let
                           val bori=dlid_to_bor i
                        in
                           if SOME bori=nexto then NONE else SOME(bor_to_lab bori)
                        end
                        val {instructions=exit_instructions,max_stack_words=exit_max_stack_words}=
                           Precomp.do_exit{block=Vector.sub(blockvec,i),getlabel=getlabel}
                        val _=
                           if !max_stack_words<exit_max_stack_words
                           then max_stack_words:=exit_max_stack_words
                           else {}
                     in
                        exit_instructions
                     end @
                     (Array.sub(block_post,i))
                  |  LABM i =>
                     let
                        val lcode=(Array.sub(labm_pre,i))@(Vector.sub(label_move_instructions,i))
                        val destblock=Array.sub(label_dest,i)
                     in
                        if SOME(BLOCK destblock)=nexto
                        then lcode
                        else lcode@[Code.goto(Vector.sub(block_labs,destblock))]
                     end
                  ))
               order
          
          val compiled_instructions' =
             (case !method_post of
                [] => compiled_instructions' (* nearly always true *)
             |  post => compiled_instructions' @ [post]
             )

          val compiled_instructions=List.concat compiled_instructions'
          val fixed_instructions=
             if !Variables.fix_netscape3_bug1 then
                (case List.last compiled_instructions of
                   Code.end_catch _ => compiled_instructions @ [Code.nop]
                |  _ => compiled_instructions
                ) 
             else
                compiled_instructions
      in
        {instructions=fixed_instructions,
         max_stack_words=SOME(!max_stack_words),
         max_locals=SOME(max_locals)
         }
      end (* final_push *)
   end (* local *)
end (* struct *)


