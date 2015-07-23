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


structure MakeDag:>MAKEDAG=
struct
   open Operations
   open Dag

   structure SortNode=Sort(NodeKey) (* used for sorting needs arrows *)

   (* We maintain a map from value numbers to DagNodes.  For
      this we use IntBinaryMap, which is a New Jersey extension.*)
   structure IBM=IntBinaryMap
   (* We use IntBinarySet for finding the required values for an
      exit *)
   structure IBS=IntBinarySet

   (* insert is used by map_args and make_dag to insert values in maps
      while checking that the value isn't already there. *)
   fun insert(valuemap,V v,n:DagNode)=
      (case IBM.find(valuemap,v) of
         NONE=>
            IBM.insert(valuemap,v,n)
      |   SOME _ =>
            raise Fail("Value "^Int.toString v ^"used twice in the same block")
      )
   |  insert(_,C _,_)=
        raise Fail("Attempt to write to constant value")

   fun map_args(B(_,(D{input=input_types,...},
      input_vals,_,_,_,_)))=
(* We use ListPair.foldl to go through the two lists, using an extra
   counter to keep track of the position of the argument *)
   let
      val _=if List.length(input_types)<>List.length(input_vals)
         then raise Fail(
"Different number of args in block descriptor to number of values supplied")
         else {}
   in
      #1(
         ListPair.foldl
            (fn (arg_type,arg_value,(map_so_far,i))=>
               (insert(map_so_far,arg_value,N(SOME arg_type,Arg(i))),i+1))
               (IBM.empty,0)
            (input_types,input_vals)
         )
   end

   fun make_dag(new_id:int,block as B(_,(block_desc as D{input=input_types,...},input_vals,orders,
      exit,exceptions,handler_info)),label_counter)=
   let
       val map_with_args=map_args(block)

       (* val2node converts a value to a DagNode, using the supplied
          map from value numbers to nodes *)
       fun val2node v_map (V n)=
          (case IBM.find(v_map,n) of
             SOME node => node
          |  NONE =>
                raise Fail("Instruction uses undefined value"^Int.toString n)
          )
       |  val2node v_map (C c)=N(SOME(Constants.typeOf c),Constant c)

       val map_with_args_ex= (* Map including a value for the handler if there is one *)
          (case handler_info of
             NONE => map_with_args
          |  SOME {thrown,thrown_class} =>
                insert(map_with_args,thrown,
                   N(SOME(Types.F(0,Types.CLASS(valOf thrown_class))),Thrown)))

(* We now have to translate the instructions in the basic block to
   DagNodes.

   This will be done using a foldl operation.  As well as the
   map taking values to DagNodes, we build up a map taking
   the instruction number (instructions are numbered from 0) to the
   corresponding DagNodes; we use this map in searching backwards for
   interfering instructions when we add the needs arrows.  We also maintain
   a counter.  So the state carried around by the foldl is:

   v_map :  the map taking values to DagNodes
   i_map :  the map taking instruction numbers to DagNodes
   arg0_init:
            if Arg 0 has been the subject of an initialisation method
            SOME (node of method), otherwise NONE.   
   i     :  the next instruction number
   *)
   (* instruction_to_dnode is the function to be passed to the foldl.

      This is a good place for doing any clever stuff.  In particular,
      common subexpression elimination, replacing arithmetic operators
      by bitwise ones, and
      computation of constant expressions.  For now I will keep it
      as simple as I can.  *)

      fun instruction_to_dnode((Op,{input=valsin,output=valsout}),
        {v_map=v_map:DagNode IBM.map,i_map=i_map:DagNode IBM.map,
         arg0_init=arg0_init:DagNode option,i=i:int})=
      let
         (* We need to find the following things for this operation:
            the type of the resulting value (if any)
            the comp, giving the operation to be done
            the args list
            the needs list
            and the id.
            *)

         (* reduce_strength may change the computation and the arguments,
            so we call the initial ones first_comp and first_args. *)
         val first_comp=Op
         val first_args=
            List.map (val2node v_map) valsin

         (* is_reducible tests if an operation is a candidate for
            reduction by reduce_strength *)
         fun is_reducible add=true
         |   is_reducible sub=true
         |   is_reducible mul=true
         |   is_reducible (divide _)=true
         |   is_reducible (rem _)=true
         |   is_reducible _=false

         fun reduce_strength(comp,args)=
         (* this computes a new comp and args when the operation is
            add, sub, mul, divide or rem, if possible.  Strategy:

            The argument types should be all numeric and all the
            same.

                If they are FLOAT or DOUBLE, make the AccessOrder flag
                OPTIONAL for divide and rem.  That's all we shall do
                because other assumptions are tricky; we don't know
                how floating point on the target machine will behave.

            Otherwise, if the type is INT or LONG:
            (1) if at least one of args is a constant and
                (if div) the second is constant, replace the
                comp by an appropriate XXX_constant.  (We are assuming
                addition and multiplication is commutative here.)
            (2) if for mul, divide or rem
                the constant is a power of 2, replace
                by an appropriate shift or bitwise and.


            There is more that can be done; in particular getting rid
               of multiplication by 0 and 1, and addition or subtraction
               of 0, and (for loops) replacing multiplication of a loop
               variable by a constant addition.
            *)
         if not(is_reducible comp) then (comp,args) else
         let
            val [arg1,arg2]=
               (case args of
                  [_,_]=>args
               |  _=>raise Fail("Arithmetic instruction has <>2 args")
               )
            val N(SOME(Types.F(0,type1)),part1)=arg1
            val N(SOME(Types.F(0,type2)),part2)=arg2
            (* A match failure here indicates that the types are array
               types *)
            val discrete=
               (case type1 of
                   Types.INT => true
                |  Types.LONG => true
                |  Types.FLOAT => false
                |  Types.DOUBLE => false
                |  _                 => raise Fail
"Arithmetic instruction has args not INT/LONG/FLOAT or DOUBLE."
                )
            fun addconst(const,arg)=(add_constant const,[arg])
(* We may later fold addconsts together. *)
         in
            if (not discrete) then
            let
               val newcomp=
                  (case comp of
                     divide(MUSTDO)=>divide(OPTIONAL)
                  |  rem(MUSTDO)=>rem(OPTIONAL)
                  |  _ => comp
                  )
            in
               (newcomp,args)
            end
            else
               (case comp of
                  add=>
                     (case (part1,part2) of
                        (Constant c,a) => addconst(c,arg2)
                     |  (a,Constant c) => addconst(c,arg1)
                     |  _              => (comp,args)
                     )
               |  sub=>
                     let
                        fun subconst(const,arg)=
                        let
                        (* Try to replace the constant by its negation
                           and the operation by an add_constant (the reason
                           for this rigmarole is to allow use of iinc later
                           on).
                           *)
                           val negconst=
                           (case const of
                              Constants.INT i => Constants.INT(JavaInt.numops.neg i)
                           |  Constants.LONG l => Constants.LONG(JavaLong.numops.neg l)
                           )
                        in
                           addconst(negconst,arg)
                        end

                     in
                        (case (part1,part2) of
                           (a,Constant c) => subconst(c,arg1)
                        |  _              => (comp,args)
                        )
                     end
               |  mul=>
                     let
                        fun mulconst(const,arg)=
                        let
                           val shifter=
                              (case type1 of
                                 Types.INT=>
                                    let val (Constants.INT i)=const
                                    in JavaInt.log2(i)
                                    end
                              |  Types.LONG=>
                                    let val (Constants.LONG l)=const
                                    in JavaLong.log2(l)
                                    end
                              )
                        in
                           (case shifter of
                              NONE  =>(mul_constant const,[arg])
                           |  SOME s=>(shl,
                                 [arg,
                                  N(SOME(Types.F(0,Types.INT)),Constant(
                                     Constants.INT(JavaInt.fromInt s)))])
                           )
                        end
                     in
                        (case (part1,part2) of
                           (Constant c,a) => mulconst(c,arg2)
                        |  (a,Constant c) => mulconst(c,arg1)
                        |  _              => (comp,args)
                        )
                     end
               |  _=> (* computation must be divide or rem *)
                     (case (part1,part2) of
                        (a,Constant c) =>
                           (case comp of
(* We can't replace divisions by shifts or rems by binary ands because
  the JVM "div" instruction is defined to round the quotient to 0. *)
                              divide _ => (divide_constant(c),[arg1])
                            | rem _    => (rem_constant(c),[arg1])
                            )
                        | _            => (comp,args)
                        )
               )
         end (* of reduce_strength *)

         val (comp,args)=reduce_strength(first_comp,first_args)

         val id=i

         (* We now compute the type of the result of the operation.  We
            could easily verify the types fairly comprehensively (using
            maybe_subclass) but we don't because in
            many cases I imagine it will be easier to locate any errors from
            the output of javap -verify. *)

         val result_type=
         let
            (* ht returns the type of the first element of the list passed
               it *)
            fun ht (N(t,_)::_)=t
            |   ht []=raise Fail "Mysterious empty argument list"
            (* adrop is like ht except that the type of the first element
               should be an array type and it decrements the number of
               dimensions involved by 1. *)
            fun adrop a=
            let
               val t=ht a
            in
               (case t of
                  SOME(Types.F(ndims,b))=>
                     if ndims>0 then
                        SOME(Types.F(ndims-1,b))
                     else raise Fail "adrop with non-array type"
               )

            end
            (* type_fref returns the type of a field, given the field
               reference. *)
            fun type_fref(Descriptors.fref{desc=d,...})=SOME d
               (* (fortunately field_descriptor is the same as java_type)
                  *)

            (* type_mref returns the return type of a method, given the
               method reference. *)
            fun type_mref(Descriptors.mref {desc=Descriptors.M(t,_),...})=t

            (* type_iref returns the return type of a method, given the
               interface method reference. *)
            fun type_iref(Descriptors.iref {desc=Descriptors.M(t,_),...})=t

            (* type_class returns the type of a class object for a method
               (this is used for new). *)
            fun type_class(Descriptors.mref {class=c,...})=Types.F(0,Types.CLASS c)
        in
            (case comp of
               add=>ht args
            |  sub=>ht args
            |  neg=>ht args
            |  mul=>ht args
            |  divide _=>ht args
            |  rem _=>ht args
            |  add_constant _=>ht args
            |  mul_constant _=>ht args
            |  divide_constant _=>ht args
            |  rem_constant _=>ht args
            |  bitwise_or=>ht args
            |  bitwise_and=>ht args
            |  bitwise_xor=>ht args
            |  shl=>ht args
            |  shr=>ht args
            |  ushr=>ht args
            |  aload _=>adrop args
            |  astore _=>NONE
            |  convert b=>SOME(Types.F(0,b))
            |  cmp _=>SOME(Types.F(0,Types.INT))
            |  sign_extend =>ht args
            |  new mr=>SOME(type_class mr)
            |  newarray (t,_)=>SOME t
            |  arraylength _ =>SOME(Types.F(0,Types.INT))
            |  getfield(fr,_)=>type_fref fr
            |  putfield(fr,_)=>NONE
            |  getstatic(fr,_)=>type_fref fr
            |  putstatic(fr,_)=>NONE
            |  invoke_interface mr=>type_iref mr
            |  invoke_special mr=>type_mref mr
            |  invoke_static mr=>type_mref mr
            |  invoke_virtual mr=>type_mref mr
            |  checkcast (jt,_)=>SOME jt
            |  instanceof _=>SOME(Types.F(0,Types.INT))
            |  monitorenter _=>NONE
            |  monitorexit _=>NONE
            |  vale _=>NONE
            )
         end (* of result_type *)

         (* We need to work out when two operations interfere with
            each other, so that we have to add a needs arrow between
            them.   To find this, we need to test when operations
            fall in the following classes.

            EVF:  something that can raise an exception; IE
                  is COMPULSORY, or is vale,
                  or is a function (actually functions can raise
                  exceptions).

            funct:  a function (this is a subclass of EVF).

            load: an operation that can load something from
                  memory; IE getfield or getstatic or aload.
                  Some load operations can also raise exceptions.
            store:an operation that can store something in memory;
                  IE putfield or putstatic or astore.  Some store
                  operations can also raise exceptions.
            mutable: for loads and stores, if the mutable flag is set.

            type4: an operation which might use one or more class objects
                  which might need needs arrows to initialisation methods
                  on those objects which are not implied by earlier rules.
            *)

         (* is_zero is used to test constants in divide instructions *)
         fun is_zero(Constants.INT i)=(i=JavaInt.fromInt 0)
         |   is_zero(Constants.LONG l)=(l=JavaLong.fromInt 0)

         fun is_EVF(divide(MUSTDO))=true
         |   is_EVF(rem(MUSTDO))=true
         |   is_EVF(divide_constant c)=is_zero c
         |   is_EVF(rem_constant c)=is_zero c
         |   is_EVF(aload(_,MUSTDO))=true
         |   is_EVF(astore(_,MUSTDO))=true
         |   is_EVF(newarray(_,MUSTDO))=true
         |   is_EVF(arraylength MUSTDO)=true
         |   is_EVF(getfield(_,(_,MUSTDO)))=true
         |   is_EVF(putfield(_,(_,MUSTDO)))=true
         |   is_EVF(getstatic(_,(_,MUSTDO)))=true
         |   is_EVF(putstatic(_,(_,MUSTDO)))=true
         |   is_EVF(new _)=true
         |   is_EVF(invoke_interface _)=true
         |   is_EVF(invoke_special _)=true
         |   is_EVF(invoke_static _)=true
         |   is_EVF(invoke_virtual _)=true
         |   is_EVF(checkcast(_,MUSTDO))=true
         |   is_EVF(monitorenter MUSTDO)=true
         |   is_EVF(monitorexit MUSTDO)=true
         |   is_EVF(vale _)=true
         |   is_EVF _ = false

         fun is_funct(new _)=true
         |   is_funct(invoke_interface _)=true
         |   is_funct(invoke_special _)=true
         |   is_funct(invoke_static _)=true
         |   is_funct(invoke_virtual _)=true
         |   is_funct _ =false

         fun is_load(aload _)=true
         |   is_load(getfield _)=true
         |   is_load(getstatic _)=true
         |   is_load _=false

         fun is_store(astore _)=true
         |   is_store(putfield _)=true
         |   is_store(putstatic _)=true
         |   is_store _ =false

         fun is_mutable(aload(MUTABLE,_))=true
         |   is_mutable(astore(MUTABLE,_))=true
         |   is_mutable(getfield(_,(MUTABLE,_)))=true
         |   is_mutable(putfield(_,(MUTABLE,_)))=true
         |   is_mutable(getstatic(_,(MUTABLE,_)))=true
         |   is_mutable(putstatic(_,(MUTABLE,_)))=true
         |   is_mutable _=false

         fun is_type4(putfield(_,(IMMUTABLE,OPTIONAL)))=true
         |   is_type4(putstatic(_,(IMMUTABLE,OPTIONAL)))=true
         |   is_type4(instanceof _)=true
         |   is_type4(checkcast(_,OPTIONAL))=true
         |   is_type4(monitorenter(OPTIONAL))=true
         |   is_type4(monitorexit(OPTIONAL))=true
         |   is_type4 _ = false


(* maybe_subclass(X,Y) tests to see whether X might be a subclass of
   Y.  If X is a subclass of Y, it will return true. *)
         local
            open ClassHandle
         in
            fun maybe_subclass(X,Y)=
               (case superclass Y of
                  NONE=>true
               |  SOME _ =>
                  let
                     fun is_subclass Z =
                        if equal(Z,Y)
                        then true
                        else
                           (case superclass Z of
                              NONE=>false
                           |  SOME new_Z=>is_subclass new_Z
                           )
                  in
                     is_subclass X
                  end
               )

            fun maybe_interface(X)=
               (case is_interface X of
                  NONE=>true
               |  SOME true=>true
               |  SOME false=>false
               )

         end

         fun is_compatible((Op,args),(Op',args'))=
(* is_compatible tests if two load or store operations could refer to the
  same storage cell.  It will return true if they do, though of course it
  may also return true in other cases.  The full explanation of the logic
  of this should be documented in the file compatible.txt.  This function
  may be refined later on by information passed down from the type
  analysis. *)
         let
            datatype lstype=
               FIELD of {f:Descriptors.field_ref,a:AccessOrder}
            |  STATIC of {f:Descriptors.field_ref,a:AccessOrder}
            |  ARRAY of AccessOrder
            fun classify(getfield(f,a))=FIELD {f=f,a=a}
            |   classify(putfield(f,a))=FIELD {f=f,a=a}
            |   classify(getstatic(f,a))=STATIC {f=f,a=a}
            |   classify(putstatic(f,a))=STATIC {f=f,a=a}
            |   classify(aload a)=ARRAY a
            |   classify(astore a)=ARRAY a
         (* this will cause a matching failure if we attempt to classify
            an operation which isn't a load/store *)

            fun name_and_desc_equal((n1,d1),(n2,d2))=
               JavaString.equal(n1,n2) andalso
               Descriptors.field_descriptor_equal(d1,d2)
         in
            (case (classify Op,classify Op') of

               (FIELD {f=Descriptors.fref f,a=a},
                FIELD {f=Descriptors.fref f',a=a'})=>
               let
                  open Types
                  (* We use variable names corresponding to those in
                     compatible.txt (except that we use FF and FF'
                     to avoid confusion with Types.F) *)
                  val FF=(#name f,#desc f)
                  val FF'=(#name f',#desc f')
                  val N(SOME(F(0,CLASS(O))),_)::_=args
                  val N(SOME(F(0,CLASS(O'))),_)::_=args'
                  (* a matching failure here indicates that the first
                     argument of a getfield or a putfield is not of type
                     class reference, as it should be *)
               in name_and_desc_equal(FF,FF') andalso a=a' andalso
                  (maybe_subclass(O,O') orelse maybe_subclass(O',O))
               end
            |  (STATIC {f=Descriptors.fref f,a=a},
                STATIC {f=Descriptors.fref f',a=a'})=>
               let
                  val F=(#name(f),#desc(f))
                  val F'=(#name(f'),#desc(f))
                  (* We use CC and CC' here to avoid confusion with
                     the constructor C *)
                  val CC= #class f
                  val CC'= #class f'
               in
                   name_and_desc_equal(F,F') andalso
                   ClassHandle.equal(CC,CC') andalso
                     (if a<>a' then raise Fail
"Static class accesses with different access flags??"
                               else true
                     )
               end
            |  (ARRAY a,ARRAY a') =>
               if (a<>a') then false
               else
               let
                  val (N(SOME(Types.F(n,X)),_))::_=args
                  val (N(SOME(Types.F(n',X')),_))::_=args'
               in
                  (n<n' andalso
                     (case X of
                        Types.CLASS c =>
                           ClassHandle.equal(c,ClassHandle.object) orelse
                           ClassHandle.equal(c,ClassHandle.cloneable)
                     |   _             => false
                     )
                  )
                  orelse
                  (n'<n andalso
                     (case X' of
                        Types.CLASS c' =>
                           ClassHandle.equal(c',ClassHandle.object) orelse
                           ClassHandle.equal(c',ClassHandle.cloneable)
                     |   _             => false
                     )
                  )
                  orelse
                  (n=n' andalso
                     (case (X,X') of
                        (Types.CLASS c,Types.CLASS c') =>
                            maybe_interface c orelse
                            maybe_interface c' orelse
                            maybe_subclass(c,c') orelse
                            maybe_subclass(c',c)

                     (* In other cases, the types must be equal *)
                     |  (t1,t2)=>Types.base_type_equal(t1,t2)
                     )
                  )
               end
            |  _ => false
            )
         end (* of is_compatible *)

         (* We are now ready to compute the needs list for the node.
            This is not tremendously easy.  To make optimisation of the
            DAG later on easier, we avoid inserting needs arrows which are
            implied anyway; IE when the operation must be done earlier because
            of some other chain of needs and argument arrows.  To optimise
            this, we go through the operations already done
            back to and including the
            last function call or the first operation of the basic block,
            whichever is earliest, inserting needs arrows where they
            are not already implied; this ensures that we don't get any
            unnecessary needs arrows.  We can stop at the last function
            because all operations needed by some other operation are needed
            by functions.
            *)

         fun do_before(i1 as (Op1,args1),i2 as (Op2,args2))=
         (* do_before returns true if Op1 needs to be done before Op2 according to rules 1 to 3;
            args1 and args2 are the corresponding argument lists. *)
            (is_EVF Op2 andalso
               (is_EVF Op1 orelse
               is_store Op1 orelse
               (is_funct Op2 andalso is_load Op1 andalso is_mutable Op1)))
            orelse
            (is_load Op2 andalso
                ((is_store Op1 andalso is_compatible(i1,i2)) orelse
                is_funct Op1))
            orelse
            (is_store Op2 andalso
            is_mutable Op2 andalso
               (is_EVF Op1 orelse
                  ((is_load Op1 orelse is_store Op1) andalso
                  is_compatible(i1,i2))))

         local
            (* We now find the id-number of the last function, or the first
               instruction in the basic block if there isn't one. *)
            fun last_funct_before n=
               if n=0 then 0
               else (* look at n-1 *)
                  let
                     val new_n=n-1
                     val SOME(N(_,Operation O))=IBM.find(i_map,new_n)
                     (* a matching failure here can't happen because
                        we should have computed operation new_n and put it in
                        i_map *)
                     val is_this_a_funct=is_funct(#comp O)
                  in
                     if is_this_a_funct
                     then new_n
                     else last_funct_before new_n
                  end
         in
            val last_funct=last_funct_before id
         end

         (* We now compute the needed nodes.  The first lot we will compute may
            contain some extra nodes.  To get rid of them, we put the needed nodes in
            descending order of id and add them to the partial order in that order, only adding
            ones which aren't already implied by the arrows.  *)

         (* Find the node needed by rule 4 if any.  This is an initialisation function
            and so can be assumed <= last_funct *)
         val type4_needs= (* NONE if we don't need the init function; SOME node otherwise *)
            if is_type4 comp then
               (case arg0_init of
                  NONE => NONE
               |  SOME node =>
                  (* we will need node if one of args is N(_,Arg 0) *)
                     if List.exists
                        (fn N(_,Arg 0) => true
                        |   _ => false
                        )
                        args
                     then SOME node
                     else NONE
               )
            else NONE

         val other_needs=
            (* Now compute the other needed arrows, also in descending order.  We only have to
               look back as far as last_funct *)
         let
            fun make_other_needs(other_needs_so_far,n)=
               if n=id
               then
                  other_needs_so_far
               else
                  let
                     val SOME(node_n as N(_,Operation O))=IBM.find(i_map,n)
                     val opargs_n=(#comp O,#args O)
                  in
                     if do_before(opargs_n,(comp,args))
                     then
                        make_other_needs(node_n::other_needs_so_far,n+1)
                     else
                        make_other_needs(other_needs_so_far,n+1)
                  end
         in
            make_other_needs([],last_funct)
         end

         val full_needs=
            (case type4_needs of
               NONE => other_needs 
            |  SOME node => other_needs@[node]
            )

         (* full_needs is now the complete list of needed nodes, in descending order.
            Now prune this list. *)
         val needs=
         (case full_needs of
            [] => []
         |   _ =>
            let
               val N(_,Operation {id=earliest_needed,...})=List.last full_needs
               (* The done_earlier array remembers whether a node is already forced to be
                  done before this one. *)
               val done_earlier=BoolArray.array(id-earliest_needed,false)

               fun lookup_earlier n=BoolArray.sub(done_earlier,n-earliest_needed)
               fun make_earlier n=BoolArray.update(done_earlier,n-earliest_needed,
                  true)

               fun visit (node as N(_,p))=
               (* visit  n adds the node and all its descendants to done_earlier,
                  returning true if this made any difference. *)
               (case p of
                  Operation O=>
                  let
                     val n= #id O
                  in
                     if n<earliest_needed orelse
                        lookup_earlier n
                     then false
                     else let
                        val _ =make_earlier n
                        (* visit all descendants *)
                        val _= List.app (ignore o visit) (#args O)
                        val _= List.app (ignore o visit) (#needs O)
                     in
                        true
                     end
                  end
               |  _ (* Constant or argument or thrown *) => false
               )

               (* Visit the arguments of the new operation *)
               val _= List.app (ignore o visit) args

               val needs=
                  List.filter
                     visit
                     full_needs
            in
               needs
            end
         )

         (* We are now in a position to construct the node. *)
         val new_node=N(result_type,Operation
            {comp=comp,args=args,needs=needs,id=id})

         (* We need to update the map from value numbers to nodes, if
            this operation has non-void type. *)
         val new_v_map=
            (case result_type of
               NONE => v_map
            |  SOME _ =>
               let (* find the value number to assign to *)
                  val [valout]=valsout
                  (* in this case the output list should have length 1 *)
               in
                  insert(v_map,valout,new_node)
               end
               )

         (* update i_map *)
         val new_i_map=IBM.insert(i_map,id,new_node)

         (* update arg0_init *)
         val new_arg0_init=
            (case comp of
               invoke_special mref =>
                  if Descriptors.is_init(mref)
                  then
                  let
                     val _ =
                        (case args of
                           N(_,Arg 0):: _ => {}
                        |  _ => raise Fail "invoke_special <init> used for argument not arg 0."
                        )
                  in
                     SOME new_node
                  end
                  else
                     arg0_init
            |  _ => arg0_init
            )

      in (* this is the in (at last!) of the instruction_to_dnode function.
            *)
         {v_map=new_v_map,i_map=new_i_map,arg0_init=new_arg0_init,i=id+1}
         (* assign maps and increment counter *)
      end (* instruction_to_dnode *)

      (* We need to construct a pseudo-order for the vale instruction,
         to go at the end of the order list.  The only tricky bit of
         this is working out what input values to supply.  It's not
         clear how to handle values which must be computed as they are
         needed by blocks referred to in labels.  Really they need to
         go into a third list, since we don't know at this stage whether
         they are going to go on the stack or be stored in local variables.

         The convention to be used is, however, that described in
         Operations.sml, where the argument list is constructed to contain
         the n_args actual stack arguments, followed by the union of the
         arguments required for the labels we might jump to, and we specify
         n_args with the vale instruction.  As n_args is generally
         small (0,1 or 2, unless Andrew is feeding us incorrect input)
         this means that both types of argument are easily accessible.
         *)

      fun reduce_exit exit=
      (* However before we do anything else, we make the exit easier *)
         (case exit of
            Operations.cond {input,test,yes,no} =>
               (case input of
                  [C const,value]=>
                     if Constants.is_zero const
                     then
                        Operations.cond0 {input=[value],test=Tests.reverse test,yes=yes,no=no}
                     else exit
               |  [value,C const]=>
                     if Constants.is_zero const
                     then
                        Operations.cond0 {input=[value],test=test,yes=yes,no=no}
                     else exit
               |  _ => exit
               )
         |  Operations.lookupswitch 
               {lookuptable=unpruned_lookuptable,default,input} =>
               let
                  (* Check that the lookuptable is sorted *)
                  (* is_sorted is true if the argument is a 
                     javaint*'a list whose java_ints are 
                     strictly increasing. *)
                  fun is_sorted([])=true
                  |   is_sorted(l as (hd::tl))=
                     ListPair.all
                        (fn ((ji1,_),(ji2,_))=>JavaInt.numops.le(ji1,ji2))
                        (l,tl)
                  val _ = 
                     if is_sorted unpruned_lookuptable 
                     then {}
                     else raise Fail "Lookuptable is not sorted!"
        
                  (* Compute lookuptable in which labels equal to default have been removed. *)
                  fun values_are_equal (value1,value2)=
                  (case (value1,value2) of
                     (V n1,V n2) => (n1=n2)
                  |  (C c1,C c2) => Constants.equal(c1,c2,false)
                  |  _ => false
                  )

                  fun labels_are_equal((bor1,args1):label,(bor2,args2))=
                  let
                     val B(b1,_)=valOf(!bor1)
                     val B(b2,_)=valOf(!bor2)
                  in
                     if b1=b2
                     then
                        ListPair.all
                           values_are_equal
                           (args1,args2)
(* two labels will compare the same if they point to the same block and one argument list is the
   prefix of the other.  However if two labels point to the same block and their argument lists are
   of different lengths, this is an error. *) 
                     else
                        false
                  end
                  
                  val lookuptable=
                     List.filter
                        (fn (ji,lab)=>not(labels_are_equal(lab,default)))
                        unpruned_lookuptable 
               in
                  (case lookuptable of
                     [] => (* How odd *)
                        Operations.goto default
                  |  [(ji,lab)] =>
                        if ji=JavaInt.fromInt 0 
                        then
                           Operations.cond0 {input=input,test=Tests.eq,yes=lab,no=default}
                        else
                           Operations.cond 
                             {input=C(Constants.INT ji)::input,test=Tests.eq,yes=lab,no=default}
                  |  _ =>
                  let
                     val length=List.length lookuptable
                     val lowest= #1(hd lookuptable)
                     val highest= #1(List.last lookuptable)
                     val rangeopt=JavaInt.toInt(JavaInt.numops.sub(highest,lowest))
                  in
                     if rangeopt<>NONE andalso
                        (!Variables.do_table) {nentries=length,range=valOf rangeopt}
                     then 
                     let
                     (* compile to a tableswitch *)
                        val range=valOf rangeopt
                        val jumptable_arr=Array.array(range+1,default)
                        fun offset ji=valOf(JavaInt.toInt(JavaInt.numops.sub(ji,lowest)))
                        val _=
                           List.app
                              (fn (ji,lab) =>
                                 Array.update(jumptable_arr,offset ji,lab))
                                 lookuptable
                     in
                        Operations.tableswitch {
                           low=lowest,
                           not_in_interval=default,
                           jumptable=List.tabulate(range+1,fn i=>Array.sub(jumptable_arr,i)),
                           input=input
                           }
                     end
                     else
                        (* leave it *)
                        Operations.lookupswitch {
                           lookuptable=lookuptable,
                           default=default,
                           input=input
                           }
                  end     
                  )
               end                      
         |  _  => exit
         )

      val reduced_exit=reduce_exit exit

      val (stack_args,label_list)=
(* compute the actual stack arguments and the labels *)
         (case reduced_exit of
            Operations.cond c=> (#input c,[#yes c,#no c])
         |  Operations.cond0 c=> (#input c,[#yes c,#no c])
         |  Operations.goto L=> ([],[L])
         |  Operations.athrow vl=> (vl,[])
         |  Operations.lookupswitch L=>(#input L,#default L::
               (#2(ListPair.unzip(#lookuptable L))))
         |  Operations.tableswitch L=>(#input L,#not_in_interval L:: #jumptable L)
         |  Operations.return vl=> (vl,[])
         )
(* Compute the values in the label_list *)
      fun lab2vals (l:label):value list= #2 l
      val label_value_list=List.concat (List.map lab2vals label_list)
(* Compute the set of value numbers in this list. *)
      val label_set=
         List.foldl
         (fn (V n,set_so_far)=>IBS.add(set_so_far,n)
         |   (C _,set_so_far)=>set_so_far
         (* We do not include constants required by labels as vale args *)
         )
         IBS.empty
         label_value_list
      val vale_label_values=List.map V (IBS.listItems label_set)
         (* vale_label_nodes should now be the list of computed values
            required by this vale, with no duplications.
            Now construct the order! *)
      val vale_order=
         (vale{n_args=List.length stack_args},
            {input=stack_args@vale_label_values,
            output=[]})

      (* Now do the conversion *)

      val {i_map=i_map,v_map=v_map,i=n_ops,arg0_init=_}=
         List.foldl
            instruction_to_dnode
            {v_map=map_with_args_ex,i_map=IBM.empty,arg0_init=NONE,i=0}
            (orders@[vale_order])

      (* It remains to construct the DagBlock itself, plus the
         (block id,ref) pairs *)
      (* We already have the identifier of the new block and the block_descriptor *)
      val vale_node=valOf(IBM.find(i_map,n_ops-1))

      (* we add DagBlocks to be patched to patch_list *)
      val patch_list:(Operations.block*Dag.DagBlock option ref) list ref =
         ref []

      fun new_label_no()=
      let
         val new_no= !label_counter
         val _= label_counter:= !label_counter+1
      in
         new_no
      end


      fun convert_label(bo_ref,args)=
      (* convert the args to DagNodes and add the referenced block to
         patch_list *)
      let
         val node_args=List.map (val2node v_map) args
         val db_ref=ref NONE
         val _=
            patch_list := (valOf(!bo_ref),db_ref):: !patch_list
      in
         (db_ref,node_args,new_label_no())
      end

      val converted_exit=
         (case reduced_exit of
            Operations.cond {test=t,yes=laby,no=labn,...} =>
               Dag.cond
                 {test=t,
                  yes=convert_label(laby),
                  no=convert_label(labn)
                  }
         |  Operations.cond0 {test=t,yes=laby,no=labn,...} =>
               Dag.cond0
                 {test=t,
                  yes=convert_label(laby),
                  no=convert_label(labn)
                  }
         |  Operations.goto lab => Dag.goto(convert_label lab)
         |  Operations.athrow _ => Dag.athrow
         |  Operations.lookupswitch {lookuptable=table,default=lab,...} =>
               Dag.lookupswitch {
                  lookuptable=
                     List.map
                        (fn (i,lab)=>(i,convert_label lab))
                        table,
                  default=convert_label lab
                  }
         |  Operations.tableswitch {low,not_in_interval,jumptable,...} =>
               Dag.tableswitch {
                  low=low,
                  not_in_interval=convert_label not_in_interval,
                  jumptable=List.map convert_label jumptable
                  }
         |  Operations.return _ => Dag.return
         )

      val converted_exceptions=
         List.map
            (fn Operations.E(cro,lab)=>Dag.DagE(cro,convert_label lab))
            exceptions

   in
      (Dag.DagB
        {id=new_id,
         vale=vale_node,
         descriptor=block_desc,
         exit=converted_exit,
         exceptions=converted_exceptions,
         is_handler=isSome handler_info
         },
       !patch_list
       )
   end (* of make_dag *)
end (* of structure MakeDag *)
