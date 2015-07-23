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

(* Constants:>CONSTANTS declares and manipulates various types
   of Java constant. *)
structure Constants:>CONSTANTS=
struct
   datatype constant= (* If a field has a constant value, it
                         must have the STATIC flag, and this value
                         must be consistent with its type *)
(* BYTE, CHAR, SHORT and BOOLEAN are all equivalent to INT.  However they
   are provided (1) so that the typeOf function works; (2) to assist with
   later retargetting.

   Recommendations: for BYTE use an unsigned byte, CHAR use a signed
       2-word (Unicode) number, SHORT use a signed 2-word number, BOOLEAN
       use 0 or 1. *)

       BOOLEAN of JavaInt.t
   |   BYTE of JavaInt.t
   |   CHAR of JavaInt.t
   |   SHORT of JavaInt.t
   |   INT of JavaInt.t
   |   LONG of JavaLong.t
   |   FLOAT of JavaFloat.t
   |   DOUBLE of JavaDouble.t
   |   STRING of JavaString.t
   |   NULL (* This corresponds to a null constant *)

   local
      fun widen(C:constant)=
      (case C of
         BOOLEAN ji => INT ji
      |  BYTE ji => INT ji
      |  CHAR ji => INT ji
      |  SHORT ji => INT ji
      |  _ => C
      )
   in
      fun equal(C1,C2,do_widen)=
      (case (if do_widen then (widen C1,widen C2) else (C1,C2)) of
          (BOOLEAN ji1,BOOLEAN ji2)=>(ji1=ji2)
      |   (BYTE ji1,BYTE ji2)=>(ji1=ji2)
      |   (CHAR ji1,CHAR ji2)=>(ji1=ji2)
      |   (SHORT ji1,SHORT ji2)=>(ji1=ji2)
      |   (INT ji1,INT ji2)=>(ji1=ji2)
      |   (LONG jl1,LONG jl2)=>(jl1=jl2)
      |   (STRING js1,STRING js2)=>JavaString.pack.equal(js1,js2)
      |   (FLOAT jf1,FLOAT jf2)=>JavaFloat.pack.equal(jf1,jf2)
      |   (DOUBLE jd1,DOUBLE jd2)=>JavaDouble.pack.equal(jd1,jd2)
      |   (NULL,NULL)=>true
      |   (_,_)=>false
      )
   end


   fun make_int(C:constant)=
   (case C of
      BOOLEAN ji => INT ji
   |  BYTE ji => INT ji
   |  CHAR ji => CHAR ji
   |  SHORT ji => SHORT ji
   |  _ => C
   )

   val j0=JavaInt.fromInt 0

   fun is_zero (C:constant)=
   (case C of
      BOOLEAN ji => ji=j0
   |  BYTE ji => ji=j0
   |  CHAR ji => ji=j0
   |  SHORT ji => ji=j0
   |  INT ji => ji=j0
   |  NULL => true
   |  _ => false
   )

   fun baseTypeOf(C:constant)=
   (* baseTypeOf is not currently in the signature but is used elsewhere
      in this file *)
      (case C of
         BOOLEAN _ => Types.BOOLEAN
      |  BYTE _    => Types.BYTE
      |  CHAR _    => Types.CHAR
      |  SHORT _   => Types.SHORT
      |  INT _     => Types.INT
      |  LONG _    => Types.LONG
      |  FLOAT _   => Types.FLOAT
      |  DOUBLE _  => Types.DOUBLE
      |  STRING _  => Types.CLASS(ClassHandle.string)
      |  NULL      => Types.CLASS(ClassHandle.object)
      )

   fun typeOf (C:constant)=Types.F(0,baseTypeOf C)

   val char_mask=JavaInt.fromInt 0xff
   val byte_shifter=JavaInt.fromInt 24
   val short_shifter=JavaInt.fromInt 16


   fun toInt(BOOLEAN ji |  BYTE ji | CHAR ji | SHORT ji | INT ji) = SOME ji
   |   toInt _ = NONE

   fun convert(C:constant,typeto:Types.base_type)=
   let
      fun is_hard t=
      (case t of
         Types.BYTE => true
      |  Types.CHAR => true
      |  Types.SHORT => true
      |  Types.INT => true
      |  Types.LONG => true
      |  _ => false 
      )

      fun is_small t=
      (case t of
         Types.BYTE => true
      |  Types.CHAR => true
      |  Types.SHORT => true
      |  _ => false
      )         

      val typefrom=baseTypeOf C
      val cando=
         is_hard typefrom andalso
         is_hard typeto

      (* We implement toJavaInt and fromJavaInt for each possible type. 
         We can take all conversions except ones where typeto=typefrom 
         via int. *)
 
      fun int_toJavaInt(INT ji)=ji
      fun char_toJavaInt(CHAR ji)=ji
      fun byte_toJavaInt(BYTE ji)=ji
      fun short_toJavaInt(SHORT ji)=ji
      fun long_toJavaInt(LONG jl)=JavaLong.toJavaInt jl
      
      fun int_fromJavaInt ji=INT ji
      fun char_fromJavaInt ji=
         CHAR(JavaInt.numops.andb(ji,char_mask))
      fun byte_fromJavaInt ji= 
         BYTE(JavaInt.numops.shr(JavaInt.numops.shl(ji,byte_shifter),byte_shifter))
      fun short_fromJavaInt ji=
         SHORT(JavaInt.numops.shr(JavaInt.numops.shl(ji,short_shifter),short_shifter))
      fun long_fromJavaInt ji=
         LONG(JavaLong.fromJavaInt ji)
   in
      if cando then SOME(
         if Types.base_type_equal(typefrom,typeto) 
         then C
         else
            (* If we can do it at all, we can do it via int *)
         let
            val toIntfun=
               (case typefrom of
                  Types.INT => int_toJavaInt
               |  Types.CHAR => char_toJavaInt
               |  Types.BYTE => byte_toJavaInt
               |  Types.SHORT => short_toJavaInt
               |  Types.LONG => long_toJavaInt
               )
            val fromIntfun=
               (case typeto of
                  Types.INT => int_fromJavaInt
               |  Types.CHAR => char_fromJavaInt
               |  Types.BYTE => byte_fromJavaInt
               |  Types.SHORT => short_fromJavaInt
               |  Types.LONG => long_fromJavaInt
               )
         in
            fromIntfun(toIntfun C)
         end    
         )   
      else NONE          
   end

   fun hashConst c = 0w29241 *
   (case c of
      BOOLEAN _ => 0w1
   |  BYTE _ => 0w2
   |  CHAR _ => 0w3
   |  SHORT _ => 0w4
   |  INT _ => 0w5
   |  LONG _ => 0w6
   |  FLOAT _ => 0w7
   |  DOUBLE _ => 0w8
   |  STRING _ => 0w9
   |  NULL => 0w10
   )


   val jim1=JavaInt.fromInt ~1
   val ji5=JavaInt.fromInt 5
   val jim32768=JavaInt.fromInt ~32768
   val ji32768=JavaInt.fromInt 32768
   val jim128=JavaInt.fromInt ~128
   val ji128=JavaInt.fromInt 128

   val jl0=JavaLong.fromInt 0
   val jl1=JavaLong.fromInt 1
   
   fun cost c=
   let
      (* Costs of constants that can be pushed by iconst_i and so on is clearly 1.
         Integer constants that are in [~32768,~128) or [128,32768) will be compiled
         to sipush and so cost is 3!
         Otherwise we assume 2 bytes for 1-word constants (which can be pushed by
         ldc) and 3 bytes for 2-word ones (which need ldc2_w).  Note that
         the backend prioritises the constant pool so that 1-word quantities come
         first, so unless there are 256 or more of them in a class file, that will
         be OK. *)
      fun cost_int ji=
         if JavaInt.numops.le(ji,ji5) andalso JavaInt.numops.le(jim1,ji)
         then 1
         else
            if (JavaInt.numops.le(jim32768,ji) andalso JavaInt.numops.lt(ji,jim128))
        orelse (JavaInt.numops.le(ji128,ji) andalso JavaInt.numops.lt(ji,ji32768))
            then 3
            else 2

      fun cost_long jl=
         if jl=jl0 orelse jl=jl1 then 1 else 3

      fun cost_float jf=
      (case JavaFloat.toInt jf of
         NONE => 2
      |  SOME i => if i>=0 andalso i<=2 then 1 else 2
      )

      fun cost_double jd=
      (case JavaDouble.toInt jd of
         NONE => 3
      |  SOME i => if i=0 orelse i=1 then 1 else 3
      )
   in
      (case c of
         BOOLEAN i => cost_int i
      |  BYTE i    => cost_int i
      |  CHAR i    => cost_int i
      |  SHORT i   => cost_int i
      |  INT i     => cost_int i
      |  LONG l    => cost_long l
      |  FLOAT f   => cost_float f
      |  DOUBLE d  => cost_double d
      |  STRING s  => 2
      |  NULL      => 1
      )
   end        
      
   (* the XXX_toString functions are intended for debugging purposes only and
      should not be used in production code or in any other way relied on. *)
   fun constant_toString(C:constant):string=
   let
      fun ji2s i=(case JavaInt.toInt i of SOME n=>Int.toString n
                                        | NONE=>"IIII")
      fun jl2s l=(case JavaLong.toInt l of SOME n=>Int.toString n
                                         | NONE=>"LLLL")
      val jf2s=JavaFloat.toString
      val jd2s=JavaDouble.toString
      val js2s=JavaString.toMLString
   in
      (case C of
         BOOLEAN i => "Bool "^ji2s i
      |  BYTE i    => "Byte "^ji2s i
      |  CHAR i    => "Char "^ji2s i
      |  SHORT i   => "Short "^ji2s i
      |  INT i     => "Int "^ji2s i
      |  LONG l    => "Long "^jl2s l
      |  FLOAT f   => "Float "^jf2s f
      |  DOUBLE d  => "Double "^jd2s d
      |  STRING s  => js2s s
      |  NULL      => "NULL"
      )
   end
end





