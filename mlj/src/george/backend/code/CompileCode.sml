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

(* The CompileCode structure contains the functions which translate
   values of type "Code.instruction list" to bytecode.

   There are currently 5 passes, plus another function (clear_labels)
   which should be done at the end, before another method is compiled:

   optimise_pass changes load to load0, store to store0, and push_XXX to
   im0 or im1 or im2 where appropriate.  (optimise_pass is optional)

   check_pass actually does nothing except make various checks on the
   validity of an instruction list.  If it discovers an error it raises
   Fail with an explanatory message beginning "check_pass: ".  It should
   be done before any of the other passes (because it will fail the 
   internal instructions that optimise_pass and pool_pass insert).  Warning - 
   check_pass does not currently do any modelling of
   the stack or the contents of local variables, since such errors should
   not cause errors in the other passes, and can be detected using javap
   on the output.  check_pass only looks at each individual instruction.
   (check_pass is optional).

   pool_pass puts entries into the constant pool, taking
   an instruction list to another instruction list paired with an
   TryCatch.t list, to be added to the exceptions for the containing
   code attribute.  pool_pass also processes the begin_catch, end_catch and
   handle_catch, replacing them by labels, and outputting an TryCatch.t
   list corresponding to them.

   label_pass finds the value of all the labels.  It also computes
   how wide all the instructions are going to be, so it takes
   the instruction list to a list of items of type width_data*int.
   (the instructions themselves are unchanged), where width_data contains
   information about where the instruction is short or long (2 byte
   operands or 1 byte operands) and, if its a table instruction, how many
   padding bytes to put in; and the int contains the length of the
   instruction.  Also label_pass lists all labels, outputting this as type
   u; this should then be fed to clear_labels.

   bytecode_pass outputs the bytecode, taking a pair of
   the instruction list (from pool_pass) and width_data list (from
   label_pass) to a Word8Vector.vector.

   clear_labels unsets all labels.
   This means that the same label can be used in two different
   methods without problems, provided that
   no other label_pass, bytecode_pass or clear_labels
   intervenes between this method's
   label_pass and clear_labels.
   Another advantage is that the same class can be compiled twice.

   There are other constraints on order which are relevant:
   1) For a given class, pool_pass should be done for ALL methods before
      label_pass is done for any method, and the pool should be
      resolved after the last pool_pass and before the first label_pass.
   2) The exception list should not be compiled for a method until the
      label_pass has been done.

   If passes are done in the wrong order, the effect is undefined, but
   normally incorrect output will not result; instead there will be
   matching failures or uncaught exceptions.

   *)

structure CompileCode:>COMPILECODE=
struct
   local
      open Code
      open Numbers
      open Types
      open Labels
      open Tests

      open AllPools
      open W8
   in
      fun check_pass(C:instruction list)=
      let
         fun assert(test:bool,mess:string)=
            if not test then
               raise Fail(String.concat["check_pass: ",mess])
            else {}

         val isreturnable=isloadable

(* is_sorted_nonempty is true if the argument is a nonempty
   JavaInts *'a list whose JavaInts  are strictly increasing. *)
         fun is_sorted_nonempty([])=false
         |   is_sorted_nonempty(l as (hd::tl))=
            ListPair.all
               (fn ((ji1,_),(ji2,_))=>JavaInt.numops.le(ji1,ji2))
               (l,tl)


         fun parg(F(0,INT))="F(0,INT)"
         |   parg(F(0,LONG))="F(0,LONG)"
         |   parg(F(0,FLOAT))="F(0,FLOAT)"
         |   parg(F(0,DOUBLE))="F(0,DOUBLE)"
         |   parg(F(0,BOOLEAN))="F(0,BOOLEAN)"
         |   parg(F(0,BYTE))="F(0,BYTE)"
         |   parg(F(0,SHORT))="F(0,SHORT)"
         |   parg(F(0,CHAR))="F(0,CHAR)"
         |   parg(F(0,CLASS _))="F(0,CLASS _)"
         |   parg(_)="Array"

         fun ci order=case order of
(* this case statement should be exhaustive! *)

(* checks on the internal-only opcodes: ie load0, store0, internal_XXX, im0/1/2
   could be removed as these are harmless; however at the moment they are
   banned as they are not needed and probably indicate an error. *)

            L _=>{}
         |  begin_catch _=>{}
         |  end_catch _=>{}
         |  handle_catch _=>{}

         |  nop=>{}
         |  add(a)=>assert(isnum(a),"add on "^parg(a))
         |  sub(a)=>assert(isnum(a),"sub on "^parg(a))
         |  neg(a)=>assert(isnum(a),"neg on "^parg(a))
         |  mul(a)=>assert(isnum(a),"mul on "^parg(a))
         |  divide(a)=>assert(isnum(a),"divide on "^parg(a))
         |  rem(a)=>assert(isnum(a),"rem on "^parg(a))

         |  iinc(i,ji)=> assert(isu2(i) andalso JavaInt.isji2(ji),
                "iinc arguments don't fit in 2 bytes")
         |  bitwise_or a=>assert(islogical a,"bitwise_or on "^parg(a))
         |  bitwise_and a=>assert(islogical a,"bitwise_and on "^parg(a))
         |  bitwise_xor a=>assert(islogical a,"bitwise_xor on "^parg(a))
         |  shl a=>assert(islogical a,"shl on "^parg(a))
         |  shr a=>assert(islogical a,"shr on "^parg(a))
         |  ushr a=>assert(islogical a,"ushr on "^parg(a))

         |  convert(a,b)=>assert(
              (isnum(a) andalso isnum(b) andalso not(java_type_equal(a,b))) orelse
              (isint(a) andalso issmall(b)),
               "Wrong arguments to convert: "^parg(a)^" and "^parg(b))
         |  load(a,i)=>
            let
               val _=assert(isloadable(a),"load: can't do "^parg(a))
               val _=assert(isu2(i),"load: huge local variable number")
            in
               {}
            end
         |  store(a,i)=>
            let
               val _=assert(isloadable(a),"store: can't do "^parg(a))
               val _=assert(isu2(i),"store: huge local variable number")
            in
               {}
            end

         |  aload _ => {}
         |  astore _ => {}

         |  load0 _ =>assert(false,"load0: not permitted")
         |  store0 _=>assert(false,"store0: not permitted")

         |  cond(t,a,_)=>
              assert(isint a orelse 
                 (isref a andalso (t=eq orelse t=ne)),"cond: bad arguments")
         |  cond0 _=>{}
         |  cmp (a,_)=>assert(iscmp a,
              "cmp: only works for longs, floats and doubles"
              )
         |  ifnull _=>{}
         |  ifnonnull _=>{}
         |  push _ =>{}
         |  push_int _=>{}
         |  push_long _=>{}
         |  push_float _=>{}
         |  push_double _=>{}
         |  push_string _=>{}
         |  push_null =>{}

         |  internal_push_int _=>
               assert(false,"internal_push_int: not permitted")
         |  internal_push_long _=>
               assert(false,"internal_push_long: not permitted")
         |  internal_push_float _=>
               assert(false,"internal_push_float: not permitted")
         |  internal_push_double _=>
               assert(false,"internal_push_double: not permitted")
         |  internal_push_string _=>
               assert(false,"internal_push_string: not permitted")
         |  im0 _=>
               assert(false,"im0: not permitted")
         |  im1 _=>
               assert(false,"im1: not permitted")
         |  im2 _=>
               assert(false,"im2: not permitted")

         |  pop => {}
         |  pop2 => {}
         |  dup => {}
         |  dup_x1 => {}
         |  dup_x2 => {}
         |  dup2 => {}
         |  dup2_x1 => {}
         |  dup2_x2 => {}
         |  swap => {}
	
         |  goto _ => {}
  	
         |  athrow => {}
         |  jsr _ => {}
         |  ret i => assert(isi2(i),"ret: huge argument")

         |  tableswitch {low,high,jumptable,...} =>
               assert(JavaInt.toInt(JavaInt.numops.sub(high,low))=
                  SOME(List.length(jumptable)-1),
               "tableswitch: jumptable is wrong size"
                  )
         |  lookupswitch {lookuptable,...}=>
               assert(is_sorted_nonempty(lookuptable),
                 "lookupswitch: table is not sorted or empty"
(* I don't know if empty lookuptables are permitted but they are pointless
   anyway. *)
                  )
         |  return a=>assert(isreturnable(a),"return: bad argument")
         |  return_void=>{}

         |  new _=>{}
         |  internal_new _=>assert(false,"internal_new: not permitted")
         |  newarray a =>assert(not(isref a),
              "new_array: now obsolete; use anewarray instead")
         |  anewarray(Types.F(n,_)) =>assert(n<=255 andalso n>=1,
              "anew_array: bad number of dimensions")
         |  internal_anewarray _=>assert(false,
              "internal_anewarray: not permitted")
         |  multianewarray(Types.F(n,_),i)=>assert(n<=255 andalso n>=1 andalso i<=n,
              "multianewarray: bad number of dimensions")
         |  internal_multianewarray _=>assert(false,
              "internal_multianewarray: not permitted")
         |  arraylength=>{}
         |  getfield _=>{}
         |  putfield _=>{}
         |  getstatic _=>{}
         |  putstatic _=>{}

         |  internal_getfield _=>assert(false,
              "internal_getfield: not permitted")
         |  internal_putfield _=>assert(false,
              "internal_putfield: not permitted")
         |  internal_getstatic _=>assert(false,
              "internal_getstatic: not permitted")
         |  internal_putstatic _=>assert(false,
              "internal_putstatic: not permitted")
         |  invoke_interface (_,i)=>assert(i>=1 andalso i<=255,
              "invoke_interface: bad number of arguments")
         |  internal_invoke_interface _=>assert(false,
              "internal_invoke_interface: not permitted")
         |  invoke_special _=>{}
         |  internal_invoke_special _=>assert(false,
              "internal_invoke_special: not permitted")
         |  invoke_static _=>{}
         |  internal_invoke_static _=>assert(false,
              "internal_invoke_static: not permitted")
         |  invoke_virtual _ =>{}
         |  internal_invoke_virtual _=>assert(false,
              "internal_invoke_special: not permitted")

         |  checkcast h => assert(Types.isref h,
              "checkcast: non-reference argument type")
         |  instanceof h => assert(Types.isref h,
              "instanceof: non-reference argument type") 
         |  internal_checkcast _ =>assert(false,
              "internal_checkcast: not permitted")
         |  internal_instanceof _ =>assert(false,
              "internal_instanceof: not permitted")
         |  monitorenter => {}
         |  monitorexit  => {}

         |  stack_size _ => assert(false,
              "stack_size: not permitted")
      in
         List.app ci C
      end


      fun do_push(const:Constants.constant)=
(* returns appropriate push_int or whatever instruction equivalent to push c *)
      let
         open Constants
      in
         (case const of
            BOOLEAN i => push_int i
         |  BYTE    i => push_int i
         |  CHAR    i => push_int i
         |  SHORT   i => push_int i
         |  INT     i => push_int i
         |  LONG    l => push_long l
         |  FLOAT   f => push_float f
         |  DOUBLE  d => push_double d
         |  STRING  s => push_string s
         |  NULL      => push_null
         )
      end

      fun optimise_pass(C:instruction list)=let
      (* unless other optimisations are added, a simple map will do,
         since everything takes 1 instruction to 1 instruction. *)

(* isload0 and isstore0 are true if load0/store0 work on the relevant type *)
         val isload0=isloadable
         val isstore0=isloadable

         fun   opt(push const)=opt(do_push const)
            |  opt(c as push_int(j))=
           (case JavaInt.toInt(j) of
               NONE=>c
            |  SOME i=>
               if Numbers.isi2(i) then
                  if Numbers.isi1(i) then
                     if i>= ~1 andalso i<=5
                     then im0(F(0,INT),i)
                     else im1(i)
                  else im2(i)
               else c
               )
         | opt(c as push_long(l))=
           (case JavaLong.toInt(l) of
               NONE => c
            |  SOME i => if i=0 orelse i=1 then im0(F(0,LONG),i) else c
            )
         | opt(c as push_float(f))=
           (case JavaFloat.toInt(f) of
               NONE => c
            |  SOME i => if i>=0 andalso i<=2 then im0(F(0,FLOAT),i) else c
            )
         | opt(c as push_double(d))=
           (case JavaDouble.toInt(d) of
               NONE => c
            |  SOME i => if i=0 orelse i=1 then im0(F(0,DOUBLE),i) else c
            )
         | opt(c as load(a,i))=
            if i<=3 andalso isload0(a) then load0(a,i) else c
         | opt(c as store(a,i))=
            if i<=3 andalso isstore0(a) then store0(a,i) else c
         | opt(c)=c
      in
         List.map opt C
      end

      fun pool_pass(A:AllPools,C:instruction list)=
      let
         (* We use a ref list to accumulate the TryCatch.t's.  The
            exceptions are actually added *)
         val exceptions_so_far=ref([]:TryCatch.t list)

         fun r_class_or_array(A,jt as Types.F(n,bt))=
         (* put a class or array type in the constant pool, as
            used by checkcast or instanceof *)
            if n=0
            then
               (case bt of
                  Types.CLASS c => r_class(A,c)
               )
            else
               r_array(A,jt)

         (* Transformations in this pass only map 1 instruction to
            1 instruction, so we use map with the following instruction.
            *)
(* exception instructions *)
         fun d(begin_catch ex)=L (TryCatch.start_lab ex)
         |   d(end_catch ex)=L (TryCatch.end_lab ex)
         |   d(handle_catch ex)=let
                val _= (exceptions_so_far:=ex:: !exceptions_so_far)
             in
                L (TryCatch.handler_lab ex)
             end
(* pool instructions *)
         |   d(push c)=d(do_push c)

         |   d(push_int   (i))=internal_push_int   (r_int   (A,i))
         |   d(push_float (i))=internal_push_float (r_float (A,i))
         |   d(push_long  (i))=internal_push_long  (r_long  (A,i))
         |   d(push_double(i))=internal_push_double(r_double(A,i))
         |   d(push_string(i))=internal_push_string(r_string(A,i))
         |   d(new        (i))=internal_new        (r_class (A,i))
         |   d(anewarray  (jt as Types.F(n,bt)))=
                if n=1 then
                   (case bt of
                      Types.CLASS c => internal_anewarray(r_class(A,c))
                   |  Types.BOOLEAN => newarray(F(0,BOOLEAN))
                   |  Types.BYTE => newarray(F(0,BYTE))
                   |  Types.CHAR => newarray(F(0,CHAR))
                   |  Types.SHORT => newarray(F(0,SHORT))
                   |  Types.INT => newarray(F(0,INT))
                   |  Types.LONG => newarray(F(0,LONG))
                   |  Types.FLOAT => newarray(F(0,FLOAT))
                   |  Types.DOUBLE => newarray(F(0,DOUBLE))
                   )
                else
                   internal_anewarray(r_array(A,Types.F(n-1,bt)))
         |   d(multianewarray(jt,j))
                              =internal_multianewarray(r_array(A,jt),j)
         |   d(getfield   (i))=internal_getfield   (r_field (A,i))
         |   d(putfield   (i))=internal_putfield   (r_field (A,i))
         |   d(getstatic  (i))=internal_getstatic  (r_field (A,i))
         |   d(putstatic  (i))=internal_putstatic  (r_field (A,i))
         |   d(invoke_interface(i,j))
               =internal_invoke_interface(r_interface_method(A,i),j)
         |   d(invoke_special(i))
               =internal_invoke_special(r_method(A,i))
         |   d(invoke_static(i))
               =internal_invoke_static(r_method(A,i))
         |   d(invoke_virtual(i))
               =internal_invoke_virtual(r_method(A,i))
         |   d(checkcast  (i))=internal_checkcast  (r_class_or_array(A,i))
         |   d(instanceof (i))=internal_instanceof (r_class_or_array(A,i))
(* push_null *)
         |   d(push_null)=im0(F(0,CLASS ClassHandle.object),0)

         |   d(x)=x
      in
         (map d C,!exceptions_so_far) (* this depends evaluation order! *)
      end

      datatype width_data=
         FIXED (* instruction has fixed length *)
      |  IS_SHORT (* instruction can be long or short but is short here *)
      |  IS_LONG  (* ditto, long *)
      |  TABLE of int*int
               (* instruction is a tabular one with int1 padding
                  bytes and int2 entries (omitting the default one).
                  *)

      type t=(width_data*int) list
      type u=label list

      fun label_pass(C:instruction list)=let
         val p=ref 0 (* this keeps track of the bytecode *)
         (* wdat computes the width_data for the given instruction.  NB -
            p should be the offset of the start of the instruction when
            wdat is called, as it is needed if the instruction is
            lookupswitch or tableswitch. *)

         val labels=ref []:Labels.label list ref (* this lists all the labels *)


         fun wdat(tableswitch({jumptable,...}))=
            TABLE((~(!p)-1) mod 4,List.length(jumptable))
         |   wdat(lookupswitch({lookuptable,...}))=
            TABLE((~(!p)-1) mod 4,List.length(lookuptable))

         |  wdat(L _)=FIXED
         (* Label instructions get passed through but are translated to
            nothing by bytecode_pass *)

(* Other fixed instructions . .*)
|wdat(nop)=FIXED|wdat(add _)=FIXED|wdat(sub _)=FIXED|wdat(neg _)=FIXED
|wdat(mul _)=FIXED|wdat(divide _)=FIXED|wdat(rem _)=FIXED
|wdat(bitwise_or _)=FIXED
|wdat(bitwise_and _)=FIXED|wdat(bitwise_xor _)=FIXED|wdat(shl _)=FIXED
|wdat(shr _)=FIXED|wdat(ushr _)=FIXED|wdat(convert _)=FIXED
|wdat(cond _)=FIXED|wdat(cond0 _)=FIXED|wdat(cmp _)=FIXED
|wdat(ifnull _)=FIXED|wdat(ifnonnull _)=FIXED
|wdat(load0 _)=FIXED|wdat(store0 _)=FIXED|wdat(aload _)=FIXED
|wdat(astore _)=FIXED|wdat(im0 _)=FIXED|wdat(im1 _)=FIXED|wdat(im2 _)=FIXED
|wdat(pop)=FIXED|wdat(pop2)=FIXED|wdat(dup)=FIXED|wdat(dup_x1)=FIXED
|wdat(dup_x2)=FIXED|wdat(dup2)=FIXED|wdat(dup2_x1)=FIXED|wdat(dup2_x2)=FIXED
|wdat(swap)=FIXED|wdat(goto _)=FIXED|wdat(athrow)=FIXED|wdat(jsr _)=FIXED
|wdat(return _)=FIXED|wdat(return_void)=FIXED
|wdat(internal_new _)=FIXED|wdat(internal_anewarray _)=FIXED
|wdat(newarray _)=FIXED
|wdat(internal_multianewarray _)=FIXED|wdat(arraylength)=FIXED
|wdat(internal_getfield _)=FIXED|wdat(internal_putfield _)=FIXED
|wdat(internal_getstatic _)=FIXED|wdat(internal_putstatic _)=FIXED
|wdat(internal_invoke_interface _)=FIXED
|wdat(internal_invoke_special _)=FIXED
|wdat(internal_invoke_static _)=FIXED
|wdat(internal_invoke_virtual _)=FIXED
|wdat(internal_checkcast _)=FIXED
|wdat(internal_instanceof _)=FIXED
|wdat(monitorenter)=FIXED|wdat(monitorexit)=FIXED
|wdat(internal_push_long _)=FIXED|wdat(internal_push_double _)=FIXED

|wdat(internal_push_int h)=if Numbers.isu1(h{}) then IS_SHORT else IS_LONG
|wdat(internal_push_float h)=if Numbers.isu1(h{}) then IS_SHORT else IS_LONG
|wdat(internal_push_string h)=if Numbers.isu1(h{}) then IS_SHORT else IS_LONG

|wdat(iinc(var,inc))=if Numbers.isu1(var) andalso JavaInt.isji1(inc) then IS_SHORT
                      else IS_LONG
|wdat(load(_,var))=if Numbers.isu1(var) then IS_SHORT else IS_LONG
|wdat(store(_,var))=if Numbers.isu1(var) then IS_SHORT else IS_LONG
|wdat(ret var)=if Numbers.isu1(var) then IS_SHORT else IS_LONG
|wdat _ = raise Fail "In wdat"
(* the len function is given an instruction plus its wdat and computes
   the number of bytes it requires *)

(* Firstly, fixed instructions *)
         fun len(L _,_)=0
         |   len(nop,_)=1
         |   len(add _,_)=1
         |   len(sub _,_)=1
         |   len(neg _,_)=1
         |   len(mul _,_)=1
         |   len(divide _,_)=1
         |   len(rem _,_)=1
         |   len(bitwise_or _,_)=1
         |   len(bitwise_and _,_)=1
         |   len(bitwise_xor _,_)=1
         |   len(shl _,_)=1
         |   len(shr _,_)=1
         |   len(ushr _,_)=1
         |   len(convert _,_)=1
         |   len(cond _,_)=3
         |   len(cond0 _,_)=3
         |   len(cmp _,_)=1
         |   len(ifnull _,_)=3
         |   len(ifnonnull _,_)=3
         |   len(load0 _,_)=1
         |   len(store0 _,_)=1
         |   len(aload _,_)=1
         |   len(astore _,_)=1
         |   len(im0 _,_)=1
         |   len(im1 _,_)=2
         |   len(im2 _,_)=3
         |   len(pop,_)=1
         |   len(pop2,_)=1
         |   len(dup,_)=1
         |   len(dup_x1,_)=1
         |   len(dup_x2,_)=1
         |   len(dup2,_)=1
         |   len(dup2_x1,_)=1
         |   len(dup2_x2,_)=1
         |   len(swap,_)=1
         |   len(goto _,_)=3
         |   len(athrow,_)=1
         |   len(jsr _,_)=3
         |   len(return _,_)=1
         |   len(return_void,_)=1
         |   len(internal_new _,_)=3
         |   len(newarray _,_)=2
         |   len(internal_anewarray _,_)=3
         |   len(internal_multianewarray _,_)=4
         |   len(arraylength,_)=1
         |   len(internal_getfield _,_)=3
         |   len(internal_putfield _,_)=3
         |   len(internal_getstatic _,_)=3
         |   len(internal_putstatic _,_)=3
         |   len(internal_invoke_interface _,_)=5
         |   len(internal_invoke_special _,_)=3
         |   len(internal_invoke_virtual _,_)=3
         |   len(internal_invoke_static _,_)=3
         |   len(internal_checkcast _,_)=3
         |   len(internal_instanceof _,_)=3
         |   len(monitorenter,_)=1
         |   len(monitorexit,_)=1
         |   len(internal_push_long _,_)=3
         |   len(internal_push_double _,_)=3

(* now for variable length things. . *)
         (* things that can be wide'd *)
         |   len(iinc _,IS_SHORT)=3
         |   len(iinc _,IS_LONG)=6
         |   len(load _,IS_SHORT)=2
         |   len(load _,IS_LONG)=4
         |   len(store _,IS_SHORT)=2
         |   len(store _,IS_LONG)=4
         |   len(ret _,IS_SHORT)=2
         |   len(ret _,IS_LONG)=4

         (* things that will be ldc or ldc_w *)
         |   len(internal_push_int _,IS_SHORT)=2
         |   len(internal_push_int _,IS_LONG)=3
         |   len(internal_push_float _,IS_SHORT)=2
         |   len(internal_push_float _,IS_LONG)=3
         |   len(internal_push_string _,IS_SHORT)=2
         |   len(internal_push_string _,IS_LONG)=3

         |   len(tableswitch _,TABLE(pad,nentries))=
                13+pad+4*nentries
         |   len(lookupswitch _,TABLE(pad,nentries))=
                 9+pad+8*nentries
         |   len _ = raise Fail "in len"

      in (
   	    List.map (fn order=>let
   	       val _=
                  (case order of
   	 	     L lab=>
                       (Labels.set_label(lab,!p);
                        labels:= lab :: !labels
                        )
   	 	  |  _  => {}
                  )
   	
   	       val wdata=wdat(order)
   	       val l=len(order,wdata)
               val _=(p:= !p+l)
   	    in
   	       (wdata,l)
   	    end) (* end of lambda *)
   	    C
         ,
            !labels
         )
      end (* end of label_pass *)

      fun bytecode_pass(C:instruction list,W:(width_data*int) list)=let
(* we use ListPair.map! *)

         val p=(ref 0)
            (* this is the program counter.  We need to
               keep track of it because of all these jump instructions
               requiring offsets. *)
         open Numbers

(* Numbers contains various abbreviations for packing numbers into
   Word8Vectors.  j2 and j4 convert labels to Word8Vectors using
   !p as offset.  *)
         fun j2(l)=Labels.o2(l,!p)
         fun j4(l)=Labels.o4(l,!p)
         fun bc(c,(w,l))=let

         (* bc takes the instruction c, width_data w,
                            and length l
                            and computes a W8.vector encoding it *)
            val coded_instruction=case c of
(* before bc returns we check the length of this instruction and update
   p *)
         L _ =>fromvList []
      |  nop=>v1 0wx0

      |  add (F(0,INT))   =>v1 0wx60
      |  add (F(0,LONG))  =>v1 0wx61
      |  add (F(0,FLOAT)) =>v1 0wx62
      |  add (F(0,DOUBLE))=>v1 0wx63

      |  sub (F(0,INT))   =>v1 0wx64
      |  sub (F(0,LONG))  =>v1 0wx65
      |  sub (F(0,FLOAT)) =>v1 0wx66
      |  sub (F(0,DOUBLE))=>v1 0wx67

      |  neg (F(0,INT))   =>v1 0wx74
      |  neg (F(0,LONG))  =>v1 0wx75
      |  neg (F(0,FLOAT)) =>v1 0wx76
      |  neg (F(0,DOUBLE))=>v1 0wx77

      |  mul (F(0,INT))   =>v1 0wx68
      |  mul (F(0,LONG))  =>v1 0wx69
      |  mul (F(0,FLOAT)) =>v1 0wx6a
      |  mul (F(0,DOUBLE))=>v1 0wx6b

      |  divide (F(0,INT))=>v1 0wx6c
      |  divide (F(0,LONG))  =>v1 0wx6d
      |  divide (F(0,FLOAT)) =>v1 0wx6e
      |  divide (F(0,DOUBLE))=>v1 0wx6f

      |  rem (F(0,INT))=>v1 0wx70
      |  rem (F(0,LONG))=>v1 0wx71
      |  rem (F(0,FLOAT))=>v1 0wx72
      |  rem (F(0,DOUBLE))=>v1 0wx73

      |  bitwise_or (F(0,INT))=>v1 0wx80
      |  bitwise_or (F(0,LONG))=>v1 0wx81
      |  bitwise_and (F(0,INT))=>v1 0wx7e
      |  bitwise_and (F(0,LONG))=>v1 0wx7f
      |  bitwise_xor (F(0,INT))=>v1 0wx82
      |  bitwise_xor (F(0,LONG))=>v1 0wx83

      |  shl (F(0,INT))=>v1 0wx78
      |  shl (F(0,LONG))=>v1 0wx79
      |  shr (F(0,INT))=>v1 0wx7a
      |  shr (F(0,LONG))=>v1 0wx7b
      |  ushr (F(0,INT))=>v1 0wx7c
      |  ushr (F(0,LONG))=>v1 0wx7d

      |  convert(F(0,INT),F(0,LONG))=>v1 0wx85
      |  convert(F(0,INT),F(0,FLOAT))=>v1 0wx86
      |  convert(F(0,INT),F(0,DOUBLE))=>v1 0wx87
      |  convert(F(0,LONG),F(0,INT))=>v1 0wx88
      |  convert(F(0,LONG),F(0,FLOAT))=>v1 0wx89

      |  convert(F(0,LONG),F(0,DOUBLE))=>v1 0wx8a
      |  convert(F(0,FLOAT),F(0,INT))=>v1 0wx8b
      |  convert(F(0,FLOAT),F(0,LONG))=>v1 0wx8c
      |  convert(F(0,FLOAT),F(0,DOUBLE))=>v1 0wx8d
      |  convert(F(0,DOUBLE),F(0,INT))=>v1 0wx8e
      |  convert(F(0,DOUBLE),F(0,LONG))=>v1 0wx8f
      |  convert(F(0,DOUBLE),F(0,FLOAT))=>v1 0wx90
      |  convert(F(0,INT),F(0,BYTE))=>v1 0wx91
      |  convert(F(0,INT),F(0,CHAR))=>v1 0wx92
      |  convert(F(0,INT),F(0,SHORT))=>v1 0wx93

      |  cond(eq,F(0,INT),lab)=>v1l(0wx9f,[j2(lab)])
      |  cond(ne,F(0,INT),lab)=>v1l(0wxa0,[j2(lab)])
      |  cond(lt,F(0,INT),lab)=>v1l(0wxa1,[j2(lab)])
      |  cond(ge,F(0,INT),lab)=>v1l(0wxa2,[j2(lab)])
      |  cond(gt,F(0,INT),lab)=>v1l(0wxa3,[j2(lab)])
      |  cond(le,F(0,INT),lab)=>v1l(0wxa4,[j2(lab)])
      |  cond(eq,_,lab)=>v1l(0wxa5,[j2(lab)])
      |  cond(ne,_,lab)=>v1l(0wxa6,[j2(lab)])
      |  cond0(eq,lab)=>v1l(0wx99,[j2(lab)])
      |  cond0(ne,lab)=>v1l(0wx9a,[j2(lab)])
      |  cond0(lt,lab)=>v1l(0wx9b,[j2(lab)])
      |  cond0(ge,lab)=>v1l(0wx9c,[j2(lab)])
      |  cond0(gt,lab)=>v1l(0wx9d,[j2(lab)])
      |  cond0(le,lab)=>v1l(0wx9e,[j2(lab)])

      |  cmp(F(0,LONG),_)=>v1 0wx94
      |  cmp(F(0,FLOAT),false)=>v1 0wx95
      |  cmp(F(0,FLOAT),true)=>v1 0wx96
      |  cmp(F(0,DOUBLE),false)=>v1 0wx97
      |  cmp(F(0,DOUBLE),true)=>v1 0wx98

      |  ifnull(lab)=>v1l(0wxc6,[j2(lab)])
      |  ifnonnull(lab)=>v1l(0wxc7,[j2(lab)])

      |  load0(F(0,INT),0)=>v1 0wx1a
      |  load0(F(0,INT),1)=>v1 0wx1b
      |  load0(F(0,INT),2)=>v1 0wx1c
      |  load0(F(0,INT),3)=>v1 0wx1d
      |  load0(F(0,LONG),0)=>v1 0wx1e
      |  load0(F(0,LONG),1)=>v1 0wx1f
      |  load0(F(0,LONG),2)=>v1 0wx20
      |  load0(F(0,LONG),3)=>v1 0wx21
      |  load0(F(0,FLOAT),0)=>v1 0wx22
      |  load0(F(0,FLOAT),1)=>v1 0wx23
      |  load0(F(0,FLOAT),2)=>v1 0wx24
      |  load0(F(0,FLOAT),3)=>v1 0wx25
      |  load0(F(0,DOUBLE),0)=>v1 0wx26
      |  load0(F(0,DOUBLE),1)=>v1 0wx27
      |  load0(F(0,DOUBLE),2)=>v1 0wx28
      |  load0(F(0,DOUBLE),3)=>v1 0wx29
      |  load0(_,0)=>v1 0wx2a
      |  load0(_,1)=>v1 0wx2b
      |  load0(_,2)=>v1 0wx2c
      |  load0(_,3)=>v1 0wx2d

      |  store0(F(0,INT),0)=>v1 0wx3b
      |  store0(F(0,INT),1)=>v1 0wx3c
      |  store0(F(0,INT),2)=>v1 0wx3d
      |  store0(F(0,INT),3)=>v1 0wx3e
      |  store0(F(0,LONG),0)=>v1 0wx3f
      |  store0(F(0,LONG),1)=>v1 0wx40
      |  store0(F(0,LONG),2)=>v1 0wx41
      |  store0(F(0,LONG),3)=>v1 0wx42
      |  store0(F(0,FLOAT),0)=>v1 0wx43
      |  store0(F(0,FLOAT),1)=>v1 0wx44
      |  store0(F(0,FLOAT),2)=>v1 0wx45
      |  store0(F(0,FLOAT),3)=>v1 0wx46
      |  store0(F(0,DOUBLE),0)=>v1 0wx47
      |  store0(F(0,DOUBLE),1)=>v1 0wx48
      |  store0(F(0,DOUBLE),2)=>v1 0wx49
      |  store0(F(0,DOUBLE),3)=>v1 0wx4a
      |  store0(_,0)=>v1 0wx4b
      |  store0(_,1)=>v1 0wx4c
      |  store0(_,2)=>v1 0wx4d
      |  store0(_,3)=>v1 0wx4e

      |  aload(F(0,INT))=>v1 0wx2e
      |  aload(F(0,LONG))=>v1 0wx2f
      |  aload(F(0,FLOAT))=>v1 0wx30
      |  aload(F(0,DOUBLE))=>v1 0wx31
      |  aload(F(0,BYTE))=>v1 0wx33
      |  aload(F(0,BOOLEAN))=>v1 0wx33 (* these are the same *)
      |  aload(F(0,CHAR))=>v1 0wx34
      |  aload(F(0,SHORT))=>v1 0wx35
      |  aload(_)=>v1 0wx32

      |  astore(F(0,INT))=>v1 0wx4f
      |  astore(F(0,LONG))=>v1 0wx50
      |  astore(F(0,FLOAT))=>v1 0wx51
      |  astore(F(0,DOUBLE))=>v1 0wx52
      |  astore(F(0,BYTE))=>v1 0wx54
      |  astore(F(0,BOOLEAN))=>v1 0wx54
      |  astore(F(0,CHAR))=>v1 0wx55
      |  astore(F(0,SHORT))=>v1 0wx56
      |  astore(_)=>v1 0wx53

      |  im0(F(0,INT),~1)=>v1 0wx2

      |  im0(F(0,INT),0)=>v1 0wx3
      |  im0(F(0,INT),1)=>v1 0wx4
      |  im0(F(0,INT),2)=>v1 0wx5
      |  im0(F(0,INT),3)=>v1 0wx6
      |  im0(F(0,INT),4)=>v1 0wx7
      |  im0(F(0,INT),5)=>v1 0wx8
      |  im0(F(0,LONG),0)=>v1 0wx9
      |  im0(F(0,LONG),1)=>v1 0wxa
      |  im0(F(0,FLOAT),0)=>v1 0wxb
      |  im0(F(0,FLOAT),1)=>v1 0wxc
      |  im0(F(0,FLOAT),2)=>v1 0wxd
      |  im0(F(0,DOUBLE),0)=>v1 0wxe
      |  im0(F(0,DOUBLE),1)=>v1 0wxf
      |  im0(_,0)=>v1 0wx1

      |  im1(i)=>fromList [0wx10,i1(i)]
      |  im2(i)=>v1l(0wx11,[i2(i)])

      |  pop=>v1 0wx57
      |  pop2=>v1 0wx58
      |  dup=>v1 0wx59
      |  dup_x1=>v1 0wx5a
      |  dup_x2=>v1 0wx5b
      |  dup2=>v1 0wx5c
      |  dup2_x1=>v1 0wx5d
      |  dup2_x2=>v1 0wx5e
      |  swap=>v1 0wx5f

      |  goto(lab)=>v1l(0wxa7,[j2(lab)])
      |  athrow=>v1 0wxbf
      |  jsr(lab)=>v1l(0wxa8,[j2(lab)])
      |  return(F(0,INT))=>v1 0wxac
      |  return(F(0,LONG))=>v1 0wxad
      |  return(F(0,FLOAT))=>v1 0wxae
      |  return(F(0,DOUBLE))=>v1 0wxaf
      |  return(_)=>v1 0wxb0
      |  return_void=>v1 0wxb1

      |  internal_new h=>v1l(0wxbb,[h2(h)])

      |  newarray(F(0,BOOLEAN))=>fromList[0wxbc,0wx4]
      |  newarray(F(0,CHAR))=>fromList[0wxbc,0wx5]
      |  newarray(F(0,FLOAT))=>fromList[0wxbc,0wx6]
      |  newarray(F(0,DOUBLE))=>fromList[0wxbc,0wx7]
      |  newarray(F(0,BYTE))=>fromList[0wxbc,0wx8]
      |  newarray(F(0,SHORT))=>fromList[0wxbc,0wx9]
      |  newarray(F(0,INT))=>fromList[0wxbc,0wxa]
      |  newarray(F(0,LONG))=>fromList[0wxbc,0wxb]

      |  internal_anewarray h=>v1l(0wxbd,[h2(h)])
      |  internal_multianewarray(h,i)=>
            v1l(0wxc5,[h2(h),Word8Vector.fromList[u1(i)]])

      |  arraylength=>v1 0wxbe
      |  internal_getfield h=>v1l(0wxb4,[h2(h)])
      |  internal_putfield h=>v1l(0wxb5,[h2(h)])
      |  internal_getstatic h=>v1l(0wxb2,[h2(h)])
      |  internal_putstatic h=>v1l(0wxb3,[h2(h)])

      |  internal_invoke_interface (h,i)=>
         v1l(0wxb9,[h2(h),Word8Vector.fromList [u1(i),0w0]])
      |  internal_invoke_special h=>v1l(0wxb7,[h2(h)])
      |  internal_invoke_virtual h=>v1l(0wxb6,[h2(h)])
      |  internal_invoke_static h=>v1l(0wxb8,[h2(h)])
      |  internal_checkcast h=>v1l(0wxc0,[h2(h)])
      |  internal_instanceof h=>v1l(0wxc1,[h2(h)])
      |  monitorenter=>v1 0wxc2
      |  monitorexit=>v1 0wxc3

      |  internal_push_long h  =>v1l(0wx14,[h2(h)])
      |  internal_push_double h=>v1l(0wx14,[h2(h)])

      (* instructions that can be wide'd *)
      |  iinc(i,j)=>(case w of
            IS_LONG=>fromvList[
                Word8Vector.fromList[0wxc4,0wx84],
                u2(i),
                JavaInt.ji2(j)
                ]
         |  IS_SHORT=>fromList [0wx84,u1(i),JavaInt.ji1(j)]
         )
      |  load(arg,i)=>let
            val opcode=case arg of
               F(0,INT)=>0wx15
            |  F(0,LONG)=>0wx16
            |  F(0,FLOAT)=>0wx17
            |  F(0,DOUBLE)=>0wx18
            |  _=>0wx19
            in
         	 (case w of
         	    IS_LONG=>fromvList[Word8Vector.fromList[0wxc4,opcode],u2(i)]
         	 |  IS_SHORT=>fromList[opcode,u1(i)]
         	 )
            end

      |  store(arg,i)=>let
            val opcode=case arg of
               F(0,INT)=>0wx36
            |  F(0,LONG)=>0wx37
            |  F(0,FLOAT)=>0wx38
            |  F(0,DOUBLE)=>0wx39
            |  _=>0wx3a
            in
         	 (case w of
         	    IS_LONG=>fromvList[Word8Vector.fromList[0wxc4,opcode],u2(i)]
         	 |  IS_SHORT=>fromList[opcode,u1(i)]
         	 )
            end

      |  ret i=>(case w of
            IS_LONG=>concat [fromList[0wxc4,0wxa9],vv1(u2 i)]
         |  IS_SHORT=>fromList[0wxa9,i1(i)]
         )

      (* the branches for internal_push_int/float/string are identical *)
      |  internal_push_int h=>(
         case w of
            IS_LONG=>v1l(0wx13,[h2(h)])
         |  IS_SHORT=>fromList[0wx12,h1(h)]
         )

      |  internal_push_float h=>(
         case w of
            IS_LONG=>v1l(0wx13,[h2(h)])
         |  IS_SHORT=>fromList[0wx12,h1(h)]
         )

      |  internal_push_string h=>(
         case w of
            IS_LONG=>v1l(0wx13,[h2(h)])
         |  IS_SHORT=>fromList[0wx12,h1(h)]
         )

      |  tableswitch {low,high,jumptable,not_in_interval}=>let
            val TABLE(npad,_)=w
            val padding=Word8Vector.fromList(List.take([0wx0,0wx0,0wx0],npad))
         in
            v1l (
               0wxaa,
                 [padding,
                  j4(not_in_interval),
                  JavaInt.ji4(low),
                  JavaInt.ji4(high)]@
                  List.map (fn lab=>j4(lab)) jumptable
               )
         end

      |  lookupswitch {lookuptable,default}=>let
            val TABLE(npad,_)=w
            val padding=Word8Vector.fromList(List.take([0wx0,0wx0,0wx0],npad))
      (* the lookuptable should be sorted already *)
         in
            v1l(
               0wxab,
              [padding,
               j4(default),
               u4(List.length(lookuptable))] @
               List.concat(
                  List.map
                  (fn (ji,lab)=>[JavaInt.ji4(ji),j4(lab)])
                  lookuptable))
         end
        
      |  _ => raise Fail "in bc"
       
      
(* THAT'S THE END OF THE MONSTROUS bc CASE STATEMENT!!,
   starting with "val coded_instruction=". .  *)

(* the following test can be removed later on once the code is stable *)
            val flat_coded_instruction=W8.flatten(coded_instruction)
            val ll=W8.length(flat_coded_instruction)
            val _=if(ll<>l) then
               raise
         	  Fail("Length mismatch in bc, instruction=" ^
         	  Word8.toString(W8.first(coded_instruction))^
         	 "\n expected "^Int.toString(l)^
         	 "\n got "^Int.toString(ll))
         	else {}
            val _= (p:= !p+l) (* update counter *)
         in
            flat_coded_instruction
         end (* That's the end of the definition of bc!  Now to
                actually do the compilation *)

      in
         W8.concat(ListPair.map bc (C,W))
      end (* bytecode_pass *)

      fun clear_labels(labels:u)=List.app
         Labels.unset_label
         labels
   end (* local *)
end (* struct *)
