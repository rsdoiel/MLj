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

(* Naive is an alternative compiler for basic block code that is supposed to
   be unoptimised but simple.  We try to avoid sharing code with the optimised basic block code.
   Exceptions are
      1) EasyLocals
      2) the datatypes that define basic blocks themselves
      3) the make_block function (all this does is assign an identification number to the block)
      4) the code for doing integer cmp.
      5) the code for computing the number of stack words for invoke_interface.
   *)
structure Naive=
struct
   open Operations
   open Types
   open Tests

   structure IBM=IntBinaryMap

   fun compile_block(block)=
   let
   (* Each block is compiled separately.  The arguments to the blocks are passed in local
      variables 0 .., just like the method itself. *)

   (* State we accumulate through the method. *)
      val block_label_map:Labels.label IBM.map ref= ref IBM.empty
      (* Map from block numbers to the label beginning the block *)

      val handler_map:TryCatch.t list IBM.map ref= ref IBM.empty
      (* handler_map maps block numbers to the list of exceptions that block handles. *)

      fun find(refmap,i)=IBM.find(!refmap,i)

      fun must_sub(refmap,i)=
         (case find(refmap,i) of
            SOME x => x
         |  NONE => raise Fail "Bad sub: "
         )

      fun update(refmap,i,n)= refmap:=IBM.insert(!refmap,i,n)

      local
         val compiled_instructions:Code.instruction list ref=ref []
         (* compiled_instructions contains the instructions for the method.  It is in reverse order! *)
      in
          fun do_ins ins=compiled_instructions:= ins:: !compiled_instructions
          fun do_lins l=compiled_instructions:= List.revAppend(l,!compiled_instructions)
          fun output_ins()= List.rev(!compiled_instructions)
      end

      val max_locals:int ref=ref 0
      val max_stack_words:int ref=ref 0
      (* max_locals and max_stack_words are the largest local number/stack size so far. *)

      fun check_max i=if !max_stack_words<i then max_stack_words:=i else {}

      fun do_one_block(B(bnumber,(D{input,...},args,instructions,exit,exceptions,hdata)))=
      (* This block has not been compiled.  Compile it, and all uncompiled child blocks.  Associate the
         label of this block with this bnumber in block_label_map, and return that label. *)
      let
         val blabel=Labels.new_label()

         val _= do_ins(Code.L blabel)
         val _= update(block_label_map,bnumber,blabel)

         val exception_handles=
            List.map
               (fn E(c,_)=>TryCatch.new_exception c)
               exceptions
         (* begin_catches *)
         val _=
            List.app
               (fn e=>do_ins(Code.begin_catch e))
               exception_handles

         val local_map:int IBM.map ref=ref IBM.empty
         (* local_map contains the map from value numbers to the locals they appear in *)
         val type_map:Types.java_type IBM.map ref=ref IBM.empty
         (* type_map contains the map from value numbers to their type. *)

         fun typeOf(C c)=Constants.typeOf c
         |   typeOf(V n)=must_sub(type_map,n)

         val (lpu,lnos)=EasyLocals.new_localpool input
         val lp=ref lpu
         fun get_local jt=
         let
            val (new_pool,new_lno)=EasyLocals.new_local(!lp,jt)
            val _= lp:=new_pool
         in
            new_lno
         end

         (* Add argument locals to the map *)
         val _=
            ListPair.app
               (fn(V a,lno)=>update(local_map,a,lno))
               (args,lnos)
         val _=
            ListPair.app
               (fn(V a,jt)=>update(type_map,a,jt))
               (args,input)

         fun is_defined(C _)=true
         |   is_defined(V n)=
           (case IBM.find(!local_map,n) of
               SOME _ => true
           |   NONE => false
           )

         fun get_vnum(V n)=n
         |   get_vnum(C _)=raise Fail "Attempt to store to a constant"

         fun store(value,jt)=
         (* store the top of the stack, of type jt, in a new local *)
         let
            val _= if is_defined value then raise Fail "Value multiply defined" else {}
            val vnum=get_vnum value
            val lno=get_local jt
            val _= do_ins(Code.store(widen jt,lno))
            val _= update(local_map,vnum,lno)
            val _= update(type_map,vnum,jt)
         in
            {}
         end

         fun load value=
         (* load the given value, returning its size *)
         let
            val _= if not(is_defined value) then raise Fail "Attempt to use an undefined value" else {}
         in
            Types.java_type_size(case value of
               C c =>
                  (do_ins(Code.push c);
                   Constants.typeOf c
                   )
            |  V vnum =>
                  let
                     val jt=must_sub(type_map,vnum)
                     val lno=must_sub(local_map,vnum)
                  in
                     (do_ins(Code.load(widen jt,lno));
                     jt)
                  end
            )
         end

         fun load_list input=
         (* load a list of values, checking the total stack size *)
            check_max(List.foldl
               op+
               0
               (List.map load input)
               )

         fun load_list'(input,extra_stack_words)=
         (* load a list of values, checking the total stack size, where there is already
            stuff on the stack *)
            check_max(List.foldl
               op+
               extra_stack_words
               (List.map load input)
               )

         (* Put the thrown object in a local *)
         val _=
            (case hdata of
               (SOME {thrown,thrown_class}) =>
               let
                  val tc=
                     (case thrown_class of
                        SOME c => c
                     |  NONE => ClassHandle.object
                     )
                  val jt=Types.F(0,Types.CLASS tc)
                  val _= check_max 1
                  val _= store(thrown,jt)
               in
                  {}
               end
            |  NONE => {}
            )

         fun do_instruction(comp,{input,output})=
         let
         (* do one instruction *)
            (* new needs special treatment; we need first to create the uninitialised
               object, then dup it, then push the arguments, then call the initialisation
               method *)
            fun method_class(Descriptors.mref{class=c,...})=c
            val extra_stack_words = 
               (case comp of
                  new mr =>
                  let
                     val _ = do_ins(Code.new(method_class mr))
                     val _ = do_ins(Code.dup)
                  in
                     2
                  end
               |  _ => 0
               )

            (* push all its arguments *)
            val _ = load_list'(input,extra_stack_words)
            (* Now do the operations.  For each operation we need to find
               the corresponding Code instruction or instructions, and the
               result type if any. *)
            val (ilist,res_type)=
            let
            (* We define abbreviations for a number of common cases *)
               fun ht()=typeOf(hd input)
               fun ar comp= (* arithmetic *)
               let
                  val res_type=ht()
               in
                  ([comp(res_type)],SOME res_type)
               end

               fun tv b=SOME(Types.F(0,b))
               val jvtint=tv Types.INT

               fun vd comp=(* computation passes straight through and returns VOID *)
                  ([comp],NONE)
               fun intg comp=(* computation passes straight through and returns INT *)
                  ([comp],jvtint)
               fun gf comp (fr as Descriptors.fref f)=
                  (* computation applied to the field ref, returning contents of the field *)
                  ([comp fr],SOME(#desc f))
               fun invm comp (mr as Descriptors.mref{desc=Descriptors.M(jvt,_),...})=
                  ([comp mr],jvt)
               fun invi (ir as Descriptors.iref{desc=Descriptors.M(jvt,_),...},i)=
                  ([Code.invoke_interface(ir,i)],jvt)
            in
               (case comp of
                  add => ar Code.add
               |  sub => ar Code.sub
               |  neg => ar Code.neg
               |  mul => ar Code.mul
               |  divide _ => ar Code.divide
               |  rem _ => ar Code.rem
               |  bitwise_or => ar Code.bitwise_or
               |  bitwise_and => ar Code.bitwise_and
               |  bitwise_xor => ar Code.bitwise_xor
               |  shl => ar Code.shl
               |  shr => ar Code.shr
               |  ushr => ar Code.ushr

               |  aload _ =>
                  let
                     val Types.F(n,bt)=ht()
                     val restype=Types.F(n-1,bt)
                     val _= if n<=0 then raise Fail "array load from non-array" else {}
                  in
                     ([Code.aload(restype)],SOME restype)
                  end
               |  astore _ =>
                  let
                     val Types.F(n,bt)=ht()
                     val eltype=Types.F(n-1,bt)
                     val _= if n<=0 then raise Fail "array store to non-array" else {}
                  in
                     vd(Code.astore(eltype))
                  end
               |  convert btout =>
                  let
                     val jtin as Types.F(0,btin)=ht()
                     val jtout=Types.F(0,btout)
                     val ain=jtin
                     val aout=jtout
                     fun oconv(a1,a2)=if java_type_equal(a1,a2) then [] else [Code.convert(a1,a2)]
                  in
                     ((case (issmall ain,issmall aout) of
                        (true,true) => 
                           let
                              fun widen()=[Code.convert(F(0,INT),aout)]
                              fun narrow()=[]
                           in
                              (case (btin,btout) of
                                 (CHAR,BYTE) => widen()
                              |  (CHAR,SHORT) => widen()
                              |  (SHORT,BYTE) => widen()
                              |  (SHORT,CHAR) => widen()
                              |  _ => narrow()
                              )
                           end
                     |  (true,false) =>
                            oconv(F(0,INT),aout)
                     |  (false,true) =>
                            oconv(ain,F(0,INT))@[Code.convert(F(0,INT),aout)]
                     |  (false,false) =>
                           oconv(ain,aout)
                     ),SOME jtout)
                  end
               |  sign_extend =>
                  let
                     val jtin=ht()
                     val _ =
                        if issmall jtin then {} 
                        else
                           raise Fail "sign_extend used for type not byte, char or short"
                  in
                     ([Code.convert(F(0,INT),jtin)],SOME jtin)
                  end
               |  cmp b =>
                     ((case ht() of
                        F(0,INT) =>
                        let
                           val _=check_max 4
                           val L1=Labels.new_label()
                           val L2=Labels.new_label()
                           val L3=Labels.new_label()
                           fun p i=Code.push(Constants.INT(JavaInt.fromInt i))
                        in
                           [
                           Code.dup2,
                           Code.cond(lt,F(0,INT),L2),
                           Code.cond(eq,F(0,INT),L3),
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
                     |  other => [Code.cmp(other,b)]
                     ),jvtint)
               |  new mr => ([Code.invoke_special mr],tv(Types.CLASS(method_class mr)))
               |  newarray (jt,_) => ([Code.multianewarray(jt,List.length input)],SOME jt)
               |  arraylength _ => intg Code.arraylength
               |  getfield(f,_)=> gf Code.getfield f
               |  putfield(f,_) => vd(Code.putfield f)
               |  getstatic(f,_) => gf Code.getstatic f
               |  putstatic(f,_) => vd(Code.putstatic f)
               |  invoke_interface ir =>
                  let
                     val total_arg_size=
                        List.foldl
                           op+
                           0
                           (List.map
                              (fn ival=>Types.java_type_size(typeOf ival))
                              input
                              )
                  in
                     invi(ir,total_arg_size-1)
                  end
               |  invoke_special mr => invm (Code.invoke_special) mr
               |  invoke_static mr => invm (Code.invoke_static) mr
               |  invoke_virtual mr => invm (Code.invoke_virtual) mr
               |  checkcast(jt,_)=>
                     ([Code.checkcast jt],SOME jt)
               |  instanceof jt => intg(Code.instanceof jt)
               |  monitorenter _ => vd Code.monitorenter
               |  monitorexit _ => vd Code.monitorexit
               )
            end

            val _= do_lins ilist
            val _=
               (case (output,res_type) of
                  ([value],SOME jt) => 
                     (store(value,jt);
                      check_max(Types.java_type_size jt))
               |  ([],NONE) => {}
               |  _ => raise Fail "Output of operation <> whether there is somewhere to put it"
               )
         in
            {}
         end (* of do_instruction *)

         (* do all instructions *)
         val _=
            List.app
               do_instruction
               instructions

         (* end_catches *)
         val _=
            List.app
               (fn e=>do_ins(Code.end_catch e))
               exception_handles

         val lab_list:(Code.instruction*label) list ref=ref
            (ListPair.map
               (fn (E(_,lab),eh) => (Code.handle_catch eh,lab))
               (exceptions,exception_handles)
            )
         (* lab_list contains a list of pairs, one for each label we've found so far, giving
            first either a exception-catch or label which is the target of the relevant exit,
            We start with the exceptions. *)
         fun getl label=
         let
            val nl=Labels.new_label()
            val _= lab_list:=(Code.L nl,label):: !lab_list
         in
            nl
         end
         (* Now do the exits *)

         val _ =
            do_lins(case exit of
               cond {test,yes,no,input} =>
               let
                  val _= load_list input
                  val jt=typeOf(hd input)
               in
                  if isint jt orelse issmall jt 
                  then
                     [Code.cond(test,widen jt,getl yes),
                      Code.goto(getl no)
                      ]
                  else if isref jt then
                     [Code.cond(test,jt,getl yes),
                      Code.goto(getl no)
                      ]
                  else
                  let
                     val _ =
                        if isreal jt andalso test=ne
                        then
                           raise Fail "Sorry - testing reals for inequality is not implemented"
                        else {}
                  in
                     [Code.cmp(jt,test=le orelse test=lt),
                      Code.cond0(test,getl yes),
                      Code.goto(getl no)
                      ]
                  end
                  
               end
            |  goto lab => [Code.goto(getl lab)]
            |  athrow input =>
               let
                  val _ =load_list input
               in
                  [Code.athrow]
               end
            |  lookupswitch {lookuptable,default,input} =>
               let
                  val _ =load_list input
               in
                  [Code.lookupswitch
                    {lookuptable=List.map (fn (ji,lab)=>(ji,getl lab)) lookuptable,
                     default=getl default
                     }]
               end
            |  return input =>
               let
                  val _=load_list input
               in
                  (case input of
                     [] => [Code.return_void]
                  |  [a] => [Code.return(widen(typeOf a))]
                  )
               end
            )

         (* That is the end of the basic block.  But we still need to compile extra code to
            handle the labels in lab list; in the process of doing this we shall also make sure that
            the child blocks are compiled. *)
         val _=
            List.app
               (fn (ins,(boref,input))=>
                  let
                     val SOME(blk as Operations.B(id,_))= !boref
                     val lab =
                        (case find(block_label_map,id) of
                            SOME lab => lab
                        |   NONE => do_one_block blk
                        )
                     (* Now add the block *)
                     val _=do_ins ins
                     (* work out whether this is an exception label or not *)
                     val extra_stack_words=
                        (case ins of
                           Code.L _ => 0
                        |  Code.handle_catch _ => 1
                        )

                     val _=load_list'(List.rev input,extra_stack_words)
                     val lcounter=ref 0 (* counts locals to write to *)
                     val _=
                        List.app (* Store contents of the stack in locals from 0 *)
                          (fn value =>
                          let
                             val jt=typeOf value
                             val _=do_ins(Code.store(widen jt,!lcounter))
                             val _=lcounter:= !lcounter+Types.java_type_size jt
                          in
                             {}
                          end
                          )
                          input

                     val _=do_ins(Code.goto lab)
                  in
                     {}
                  end
                  )
               (!lab_list)

         val _= (* last task - update max_locals! *)
            let
               val nlocals=EasyLocals.max_locals(!lp)
            in
               if nlocals> !max_locals then max_locals:=nlocals else {}
            end
      in
         blabel
      end (* do_one_block *)

      val _= do_one_block block
   in
     {instructions=output_ins(),
      max_locals= SOME(!max_locals),
      max_stack_words= SOME(!max_stack_words)
      }
   end
end
