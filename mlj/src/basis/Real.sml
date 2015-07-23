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

(*======================================================================*)
(* Real structure.                                                      *)
(*======================================================================*)
structure Real:REAL =
struct
   type real = real

   local 
      open General
      open Option
      open Bool
      val op= = Prim.=
      val op<> = Prim.<>

      exception NumberFormatException = java.lang.NumberFormatException


      fun real2long(x:real):Int64.int =
         _pure(java.lang.Double.doubleToLongBits(x))
      fun long2real(x:Int64.int):real = 
         _pure(java.lang.Double.longBitsToDouble(x))

      (* lreal is a real with its bits unpacked. *)
      datatype lreal=R of
        {negative:bool,
         ebits:int,
         mbits:Int64.int
         } 

      (* While this hasn't been tested, we should be able
         to get a Real32 structure by changing references to doubles to 
         references to floats, replacing references to Int64s and longs
         to references to Ints and ints, adding a Math32 structure,
         and changing (msize,esize) to (23,8). *)
      val msize=52:int
      val esize=11:int
      val mhigh = Prim.shl(1:Int64.int,msize)
      val mmask = Int64.-(mhigh,1)
      val smask = Prim.shl(mhigh,esize)
      val emask_shifted = Int.-(Prim.shl(1:int,esize),1)

      val eoffset=Int.-(Prim.shl(1:int,Int.-(esize,1)),1)

      fun unpack(x:real):lreal=
      let
         val lx=real2long x
      in
         R {
           negative= Int64.<(lx,0),
           ebits= Prim.And(Prim.l2i (Prim.shr(lx,msize)), emask_shifted),
           mbits= Prim.And(lx, mmask)
           }
      end
 
      fun pack(R{negative,ebits:int,mbits:Int64.int})=
      let
         val ebshifted = Prim.shl(Prim.i2l ebits, msize)
         val absval = Prim.or(ebshifted,mbits)
         val resl=
            if negative then
               Prim.add(absval, smask)
            else
               absval
      in
         long2real resl
      end       

      val posInf = java.lang.Double.POSITIVE_INFINITY
      val negInf = java.lang.Double.NEGATIVE_INFINITY
      val minPos = 4.94e~324
      (* It would be nice to use java.lang.Double.MIN_VALUE but that seems to be
         wrong in jdk1.0.2.  3 digits is probably excessive; after all the two adjacent
         floats are 0 and 9e~324. *)
      val maxNeg = Prim.neg(minPos)

      val NaN= java.lang.Double.NaN


      fun isFinite (x:real) =
         Prim.lt(x,posInf) andalso Prim.gt(x,negInf)

      (* We rely on the fact that IEEE reals are stored so that comparisons on
         reals and the equivalent longs are almost exactly the same (modulo 
         signed zeros) *)   
      fun nextUp(r:real)=
         if isFinite r then long2real(Int64.+(real2long r,1)) else r
      (* If r is finite, smallest real greater in magnitude than r. *)

      fun nextDown(r:real)=
      (* If r is finite, largest real smaller in magnitude than r (We also have
         to cope with 0). *)
         if isFinite r 
         then
         let
            val rlong=real2long r
         in
            if not (Prim.eq(r, 0.0))
            then
               long2real(Int64.+(rlong,~1)) 
            else
               if rlong=0
               then
                  (* r= + 0.0 *)
                  maxNeg
               else
                  (* r= - 0.0 *)
                  minPos
         end   
         else r

      fun pow2 n=
         (* Assuming -eoffset<n<=eoffset, pow2 n is the n'th power of 2, 
            represented exactly *)
(*
      if Int.<=(n,Int.~ eoffset) orelse Int.>(n,eoffset) 
      then 
         raise Fail "Bad n in Real.pow2" 
      else
*)
         pack(R{negative=false,ebits=Int.+(n,eoffset),mbits=0})
         
           
      (* smereal is a real in mantissa/exponent form. *)
      datatype smereal=
         NAN
      |  INFINITE of bool (* For INFINITE and ZERO bool is true if negative *)
      |  ZERO of bool
      |  FINITE of
        {negative:bool,
         exponent:int,
         mantissa:Int64.int
         } 
      (* The real should have the form 
         (if negative then ~1 else 1) * mantissa * 2^exponent 

         A FINITE smereal should either have
            exponent-msize in (-eoffset,eoffset) and mantissa in
               [mhigh,2*mhigh)  
         or else have
            exponent-msize= -eoffset and mantissa in [1,2*mhigh)
         *)

      val meoffset=Int.+(eoffset,msize) (* exponent should be < this *)
      val meoffset'=Int.-(meoffset,1)

      fun sme_unpack r=
      let
         val R{negative,ebits,mbits}=unpack r
      in
         if ebits=emask_shifted then
            (* infinite or NaN *)
            if mbits=0 then INFINITE negative else NAN
         else if ebits=0 then
            (* 0 or subnormal *)
            if mbits=0 then ZERO negative else
               FINITE{negative=negative,exponent=Int.~ meoffset',
                  mantissa=mbits}
         else 
            (* normal *)
            FINITE{negative=negative,exponent=Int.-(ebits,meoffset),
               mantissa=Int64.+(mbits,mhigh)}
      end      

      fun sme_pack sme=pack(R
      (case sme of
         NAN => {negative=false,ebits=emask_shifted,mbits=mmask}
      |  INFINITE negative =>
                {negative=negative,ebits=emask_shifted,mbits=0}
      |  ZERO negative =>
                {negative=negative,ebits=0,mbits=0}
      |  FINITE {negative,exponent,mantissa} =>
         if Int64.>=(mantissa,mhigh) 
         then
            {negative=negative,ebits=Int.+(exponent,meoffset),
             mbits=Int64.-(mantissa,mhigh)}
         else
            {negative=negative,ebits=Int.+(eoffset,meoffset'),
             mbits=mantissa}
      ))         

      fun IntInfmulpow2(x,n:int)=
         if Int.>=(n,0)
         then
            IntInf.<<(x,Word.fromInt n)
         else
            IntInf.~>>(x,Word.fromInt(Int.~ n))

      val IntInfone=IntInf.fromInt 1
   in 
      val radix = 2 
      val precision = msize
      (* precision is the number of bits used to encode the mantissa. *)
   
      fun (x:real) < (y:real) = Prim.lt(x,y)
      fun (x:real) > (y:real) = Prim.gt(x,y)
      fun (x:real) <= (y:real) = Prim.le(x,y)
      fun (x:real) >= (y:real) = Prim.ge(x,y)

      fun x + y = Prim.add(x:real, y:real)
      fun x - y = Prim.sub(x:real, y:real)
      fun x * y = Prim.mul(x:real, y:real)
      fun x / y = Prim.div(x:real, y:real)

      fun *+ (a,b,c)= (a*b + c)
      fun *- (a,b,c)= (a*b - c)
    
      fun ~ x   = Prim.neg(x:real)
     
      val maxFinite = java.lang.Double.MAX_VALUE
 
      val posInf=posInf (* implemented in the declaration part of the local *)
      val negInf=negInf (* ditto *)
      val minPos=minPos (* ditto *)
 
      (* we avoid using long2real where the constants already exist as
         statics because the compiler won't be able to elide them. *)
      val minNormalPos= long2real(Prim.shl(1:Int64.int, msize))

      fun abs x = java.lang.Math.abs(x:real)
      fun min (x,y) = java.lang.Math.min(x:real,y:real)
      fun max (x,y) = java.lang.Math.max(x:real,y:real)

      fun sign x =
         let
            val s1=Prim.cmpl (x:real,0.0)
            (* s1=1 if x>0, 0 if x=0, -1 if x<0 or NaN *)
         in
            if Int.>=(s1,0) 
            then s1
            else (* Is x NaN? We could settle this with another comparison,
                    but we use isNaN in the hope that it gets inlined *)
               if java.lang.Double.isNaN(x:real)
               then raise General.Domain
               else ~1
         end
 
      fun signBit x= Int64.<(real2long x,0)

      fun sameSign(x,y)=(signBit x=signBit y)
      fun copySign(x,y)=
         if signBit x=signBit y
         then x
         else
            long2real(Prim.xor(real2long x, smask))

      fun compare(x:real,y:real)=
         (case Prim.cmpl(x,y) of
             1 => GREATER
         |   0 => EQUAL
         |   ~1 =>
            (if Int.<=(Prim.cmpg(x,y),0) 
             then LESS
             else raise IEEEReal.Unordered
             )
         ) 
      fun compareReal(x:real,y:real)=
         (case Prim.cmpl(x,y) of
             1 => IEEEReal.GREATER
         |   0 => IEEEReal.EQUAL
         |   ~1 =>
            (if Int.<=(Prim.cmpg(x,y),0) 
             then IEEEReal.LESS
             else IEEEReal.UNORDERED
             )
         ) 

      fun op == (x:real,y:real)=
         (Prim.cmpl(x,y) = 0)

      fun op != (x:real,y:real)=
         (Prim.cmpl(x,y) <> 0)
     
      fun isNan (x:real)=
         java.lang.Double.isNaN(x)

      fun op ?= (x:real,y:real)=
            Prim.eq(x,y)
         orelse
            isNan x
         orelse
            isNan y         

      fun unordered(x,y)=isNan (x+y)
      (* If (posInf,posInf) and (negInf,negInf) were unordered, as I feel
         they should be, we would replace x+y by x-y.  *)
 
      val isFinite=isFinite (* implemented in the declaration part
         of the local *)
        
      fun isNormal x=
      let
         val ax=abs x
      in
         ax>=minNormalPos andalso ax<posInf
      end

      fun class x=
      let
         val R {negative,ebits,mbits} = unpack x
         val sign=if negative then IEEEReal.NEG else IEEEReal.POS
      in
         if ebits=emask_shifted
         then
            if mbits=0 
            then IEEEReal.INF sign
            else 
               IEEEReal.NAN(
                  if mbits=mmask then IEEEReal.QUIET else IEEEReal.SIGNALLING)
         else if ebits=0
         then
            (if mbits=0 then IEEEReal.ZERO else IEEEReal.SUBNORMAL) sign
         else IEEEReal.NORMAL sign
      end    

      fun fromString mlstring=
      let
         (* NB - judging from the behaviour of SML/NJ and the specification 
            of Real.toString,
            "inf", "nan" and "~nan" are not supposed to work here *)
         (* get mlstring into Java format *)
         val jstring=
            String.implode
               (List.map
                  (fn #"~" => #"-"
                  |   #"e" => #"E"
                  )
                  (String.explode mlstring)
                  )
         val res=
            SOME (
              (Prim.unsafeValOf(java.lang.Double.valueOf(jstring))).#doubleValue())
            handle NumberFormatException => NONE
      in
         res
      end
      
      fun toManExp r=
      let
         val R{negative,ebits,mbits}=unpack r
         fun sR mbits=pack(R{negative=negative,ebits=0,mbits=mbits})
      in
         (* recognise odd cases *)
         if ebits=emask_shifted
         then
             (* infinity or NaN *)
             {man=r,exp=0}
         else
         if ebits=0
         then
             (* unnormalised number or 0 *)
             if mbits=0 
             then (* 0 *)
                {man=r,exp=0}
             else
             let
                fun shiftup(mbits:Int64.int,ebits)=
                let
                   val mbits'= Prim.shl(mbits,1:int)
                in
                   if Int64.>=(mbits',mhigh)
                   then 
                     {man=sR(Prim.And(mbits', mmask)),
                      exp=Int.-(ebits,eoffset)}
                   else
                      shiftup(mbits',Int.-(ebits,1))
                end
             in
                shiftup(mbits,ebits)
             end                      
         else
             (* normalised number *)
             {man=sR mbits,exp=Int.-(ebits,eoffset)}
      end    

      fun fromManExp{man,exp}=
      let
         val R{negative,mbits,ebits}=unpack man
      in
         if ebits=emask_shifted orelse (ebits=0 andalso mbits=0) orelse (exp=0) 
         then 
         (* infinity or NaN or 0 *) 
            man
         else
         let
            val mbits'=Int64.+(mbits,if Int.>(ebits,0) then mhigh else 0)
            val ebits'=Int.+(ebits,exp)
         in
            if Int.>=(ebits',emask_shifted)
            then
               (* Infinity *)
               if negative then negInf else posInf
            else if Int.<=(ebits',0) 
            then
               (* Underflow or 0.  Shift mbits' down 1-ebits' times.
                  We round to nearest with ties going up (for now). *)
            let
               val toshift=Int.-(1,ebits')
               val rounder=Prim.shl(1:Int64.int, (Int.-(toshift,1)))
            in
               pack(R{negative=negative,mbits=Prim.shr(Int64.+(mbits',rounder),
                  toshift),ebits=0})
            end
            else
               pack(R{negative=negative,mbits=Prim.And(mbits',
                  mmask),ebits=ebits'})
         end
      end  

      fun split r=
      let
         val sign=signBit r
         val signfn=fn x:real=>if sign then ~x else x
         val absr=abs r
         val {whole=whole',frac=frac'}=
            if java.lang.Double.isInfinite(absr)
            then {whole=absr,frac=0.0}
            else
            let
               val w = java.lang.Math.floor(absr)
               val f=absr-w
            in
               {whole=w,frac=f}
            end
      in
         {whole=signfn whole',frac=signfn frac'}
      end

      val realMod= #frac o split
      (* this is not just laziness; there doesn't appear to be any substantially easier way *)

      fun rem(x:real,y:real)=
      (* Careful examination of the various cases of the Java VM's drem instruction
         reveals that this has exactly the right semantics. *)
         Prim.rem(x,y)

      fun nextAfter(r,t)=
         (case compareReal(r,t) of
            IEEEReal.LESS => if signBit r then nextDown r else nextUp r
         |  IEEEReal.GREATER => if signBit r then nextUp r else nextDown r
         |  IEEEReal.EQUAL => t (* NB; r rather than t would be wrong, since r and t might be zeros
                                   of opposite sign. *)
         |  IEEEReal.UNORDERED => NaN
         )

      fun checkFloat x=
      if isFinite x then x
      else
         if isNan x 
         then raise Div
         else raise Overflow

      
      (* We provide 12 functions, of all possible pairs of
         ("floor" or "ceil" or "trunc" or "round") ("" or "Int64" or "Large").
         The Int64 functions and Large functions do not appear in the 
         signature but are used in Java.toInt64 and toLarge.
         *)
     
      (* First here are some constants *)
      val int_precision= valOf (Int.precision)
      val int_high=pow2 (Int.-(int_precision,1))
      val int_high_m1=int_high-1.0
      val int_high_half=int_high-0.5

      val int_low= ~int_high
      val int_low_m1=int_low-1.0
      val int_low_half=int_low-0.5

      fun checkNan x=if isNan x then raise General.Domain else raise 
         General.Overflow
      (* Used when x does not fall inside the required range for floor, 
         etcetera *)
      (* The checkFloor/checkCeil/checkTrunc/checkRound functions call checkNan
         (thus raising an exception) unless x can be floored/ceiled/
         trunced/rounded to within the range of int, when they do nothing. *)
      fun checkFloor x=
         if x>=int_low andalso x<int_high then {} else checkNan x
      fun checkCeil x=
         if x>int_low_m1 andalso x<=int_high_m1 then {} else checkNan x

      (* checkTruncNeg/Pos assume their argument is negative (or zero) or 
         positive, respectively *)
      fun checkTruncNeg x=
         if x>int_low_m1 then {} else checkNan x
      fun checkTruncPos x=
         if x<int_high then {} else checkNan x

      fun checkRound x=
         if x>=int_low_half andalso x<int_high_half then {} else checkNan x
         (* Assuming Math.round works to IEEE specifications, which is what
            we can assume from the preamble to the Math structure,
            int_low_half and int_high_half will round to the nearest even 
            integer, which for int_low_half is int_low (in range) but for
            int_high_half is int_high (out of range). *)
        

      fun floor x=
        (checkFloor x; Prim.d2i (java.lang.Math.floor(x)))
      fun ceil x=
        (checkCeil x; Prim.d2i (java.lang.Math.ceil(x)))
      fun trunc x=
         if x<=0.0 then
           (checkTruncNeg x; Prim.d2i (java.lang.Math.ceil(x)))
         else
           (checkTruncPos x; Prim.d2i (java.lang.Math.floor(x)))
      fun round (x:real)= (checkRound x; Prim.d2i (x))


      fun toInt mode=
      (case mode of
         IEEEReal.TO_NEAREST => round
      |  IEEEReal.TO_NEGINF => floor
      |  IEEEReal.TO_POSINF => ceil
      |  IEEEReal.TO_ZERO => trunc
      )

      fun fromInt (i:int) = Prim.i2d i

      val large_precision= Int64.toInt(valOf (Int64.precision))
      val large_high= pow2 (Int.-(large_precision,1))
      val large_low= ~large_high

      (* For Int64s things are simpler, because Int64s have more bits of 
         precision than reals.
         So any of the operations fail on x <=> checkInt64 calls checkNan.  
         We split it up into Neg and Pos parts for trunc again.  *) 
      fun checkInt64 x= 
         if x>=large_low andalso x<large_high then {} else checkNan x
      fun checkInt64Neg x=
         if x>=large_low then {} else checkNan x
      fun checkInt64Pos x=
         if x<large_high then {} else checkNan x

      fun floorInt64 x=
        (checkInt64 x; Prim.d2l (java.lang.Math.floor(x)))
      fun ceilInt64 (x:real)=
        (checkInt64 x; Prim.d2l (java.lang.Math.ceil(x)))
      fun truncInt64 (x:real)=
         if x<=0.0 then
           (checkInt64Neg x;
            Prim.d2l (java.lang.Math.ceil(x)))
         else
           (checkInt64Pos x;
            Prim.d2l (java.lang.Math.floor(x)))
      fun roundInt64 (x:real)=
        (checkInt64 x; Prim.d2l (x))


      fun toInt64 mode=     
      (case mode of
         IEEEReal.TO_NEAREST => roundInt64
      |  IEEEReal.TO_NEGINF => floorInt64
      |  IEEEReal.TO_POSINF => ceilInt64
      |  IEEEReal.TO_ZERO => truncInt64
      )

      fun fromInt64 (i:Int64.int)= Prim.l2d i


      val zero=IntInf.fromInt 0

      fun truncLarge x=
      (case sme_unpack x of
         NAN => raise General.Domain
      |  INFINITE _ => raise General.Overflow
      |  ZERO _ => zero
      |  FINITE {negative, exponent,mantissa}=>
      let
         val res=IntInfmulpow2(MLJIntInfUtils.fromFixedInt 
           mantissa,exponent)
      in
         if negative then IntInf.~ res else res
      end
      )

      (* Now for realFloor/realCeil/realTrunc.  These are implemented independently
         of floor/ceil/trunc, because they were not added to the standard basis
         until after those were written.  We know that the Java functions Math.floor/ceil/trunc
         return x when given x which is infinite or NaN, because that's what the
         corresponding netlib functions do. *)
      fun realFloor (x:real)=
          java.lang.Math.floor(x)

      fun realCeil (x:real)=
         java.lang.Math.ceil(x)

      fun realTrunc x=
         if x<0.0
         then
            realCeil x
         else
            realFloor x          

      (* bitwise operations for Int64 *)
      fun pow64(i:Int.int):Int64.int = Prim.shl(1:Int64.int,i)
      fun and64(x:Int64.int,y:Int64.int) = Prim.And(x,y)
      fun shl64(x:Int64.int,y:Int.int)= Prim.shl(x,y)
      fun ushr64(x:Int64.int,y:Int.int)= Prim.ushr(x,y)

      fun toInfLarge x=
      (case sme_unpack x of
         NAN => raise General.Domain
      |  INFINITE _ => raise General.Overflow
      |  ZERO _ => zero
      |  FINITE {negative, exponent,mantissa}=>
      let
         val res=
            if Int.>=(exponent,0) 
            then
               IntInfmulpow2(MLJIntInfUtils.fromFixedInt mantissa,
                 exponent)
            else
            let
               (* compute mantissa shifted right by -exponent 
                  rounded up *)
               val negexp=Int.~ exponent
               val rounder=Int64.-(pow64 negexp,1)
               val result64=ushr64(Int64.+(mantissa,rounder),negexp)
            in
               MLJIntInfUtils.fromFixedInt result64
            end
      in
         if negative then IntInf.~ res else res
      end
      )

      fun floorLarge x=
      if x>=0.0 then truncLarge x else toInfLarge x

      fun ceilLarge x=
      if x>=0.0 then toInfLarge x else truncLarge x

      fun roundLarge x=
      (case sme_unpack x of
         NAN => raise General.Domain
      |  INFINITE _ => raise General.Overflow
      |  ZERO _ => zero
      |  FINITE {negative, exponent,mantissa}=>
      let
         val res=
            if Int.>=(exponent,0) 
            then
               IntInfmulpow2(MLJIntInfUtils.fromFixedInt mantissa,
                 exponent)
            else
            let
               (* compute mantissa shifted right by -exponent 
                  rounded up *)
               val negexp=Int.~ exponent
               val rounder=pow64(Int.-(negexp,1))               
               val m1=Int64.+(mantissa,rounder)
               val m2=Int64.-(m1,1)
               val r1=ushr64(m1,negexp)
               val r2=ushr64(m2,negexp)
            in
               MLJIntInfUtils.fromFixedInt 
                  (* round to nearest or if tie to even *)
                 (if r1=r2
                  then
                     r1
                  else
                     if and64(r1,1)=0
                     then r1
                     else r2
                  )
            end
      in
         if negative then IntInf.~ res else res
      end
      )

      fun toLargeInt mode=     
      (case mode of
         IEEEReal.TO_NEAREST => roundLarge
      |  IEEEReal.TO_NEGINF => floorLarge
      |  IEEEReal.TO_POSINF => ceilLarge
      |  IEEEReal.TO_ZERO => truncLarge
      )

      fun fromLargeIntGeneral mode i=
      if i=zero
      then
         0.0
      else
      let
         (* compute provisional values of negative, exponent and mantissa in
            smereal form.  (We may have to revise exponent and mantissa because
            of rounding) *)
         val negative=(IntInf.<(i,zero))
         val absi=if negative then IntInf.~ i else i
         val exponent=Int.-(IntInf.log2 absi,msize)

         val (mantissa',mustbe_exact)=
            if Int.<=(exponent,0)
            then
               (IntInf.<<(absi,Word.fromInt(Int.~ exponent)),true)
            else
               (IntInf.~>>(absi,Word.fromInt exponent),false)

         val mantissa=MLJIntInfUtils.toFixedInt mantissa'
         val em=(exponent,mantissa)

         (* adjust exponent and mantissa if necessary *)      
         fun do_roundup()=
         (* returns (exponent,mantissa) rounded up by 1 *)
         let
            val inc_mantissa=FixedInt.+(mantissa,1)
         in
            if inc_mantissa=0
            then
               (Int.+(exponent,1),mhigh)
            else
               (exponent,inc_mantissa)
         end

         fun roundup()=(* round exponent and mantissa to infinity if mantissa is not exact,
                          assuming exponent>=0 *)
         let
            val remainder=IntInf.xorb(IntInf.<<(mantissa',Word.fromInt exponent),absi)
            (* this is horrible but all the other ways I can think of for extracting
               the lower bits (without writing more functions on IntInfs) involve
               creation of at least two intermediate int infs; EG by taking 2^exponent and
               then subtracting 1. *)
         in
            if remainder=zero
            then
               em
            else
               do_roundup()
         end

         fun roundnearest()=(* round exponent and mantissa to nearest, ties to even, 
                               assuming exponent>=0 *)
         let
            val remainder=IntInf.xorb(IntInf.<<(mantissa',Word.fromInt exponent),absi)    
            val tocompare=IntInf.<<(IntInfone,Word.fromInt(Int.-(exponent,1)))
            val doit=
               (case IntInf.compare(remainder,tocompare) of
                  GREATER => true
               |  LESS => false
               |  EQUAL => (FixedInt.rem(mantissa,2)<>0)
               )
         in
            if doit
            then
               do_roundup()
            else
               em
         end

         val (exponent,mantissa)=
            if mustbe_exact 
            then
               em
            else
               (case mode of
                  IEEEReal.TO_NEAREST => roundnearest()
               |  IEEEReal.TO_NEGINF =>
                  if negative then
                     roundup()         
                  else
                     em
               |  IEEEReal.TO_POSINF =>
                  if negative then
                     em
                  else
                     roundup()
               |  IEEEReal.TO_ZERO =>
                  em
               )
      in
         sme_pack(
           if Int.>=(exponent,meoffset)
           then
              (* Overflow! *)
              INFINITE negative
           else
              FINITE {
                 negative=negative,
                 exponent=exponent,
                 mantissa=mantissa
                 }
           )
      end

      val fromLargeInt=fromLargeIntGeneral IEEEReal.TO_NEAREST
      
      fun toLarge x=x
      fun fromLarge _ x=x
    

      local
         fun nzeros n=CharVector.tabulate(n,fn _ => #"0")

         type decimal_real=
            (* decimal representation of a real with the
               mantissa represented by digits which corresponds
               to a number in [1,10) *)
            {negative:bool,
             digits:string,
             exponent:int
             }

         fun fix({negative,digits,exponent}:decimal_real)=
         let
            val unsigned=
               if Int.>=(Int.+(exponent,1),String.size digits) 
               then 
               (* They should in fact be equal *)
                  [digits]
               else
                  if Int.<(exponent,0) then
                  ["0.",nzeros(Int.-(~1,exponent)),digits]
               else
                 [String.substring(digits,0,Int.+(exponent,1)),
                  ".",
                  String.extract(digits,Int.+(exponent,1),NONE)
                  ]
         in
            String.concat
               (if negative then List.::("~",unsigned) else unsigned)
         end   


         fun sci({negative,digits,exponent}:decimal_real)=
         let
            val unsigned=
            if Int.>(String.size digits,1) then
              [String.substring(digits,0,1),
               ".",
               String.extract(digits,1,NONE),
               "E",
               Int.toString exponent
               ]
            else
              [String.substring(digits,0,1),
               "E",
               Int.toString exponent
               ]
         in
             String.concat
                (if negative then List.::("~",unsigned) else unsigned)
         end
 
         fun fix0 n=
         (* (FIX(SOME n)) for 0.0 *)
         let
            val digits=nzeros(Int.+(n,1))
         in
            fix{negative=false,digits=digits,exponent=0} 
         end

         fun sci0 n=
         (* (SCI(SOME n)) for 0.0 *)
         let
            val digits=nzeros(Int.+(n,1))
         in
            sci{negative=false,digits=digits,exponent=0}
         end

         fun gen0 n=fix0 (Int.-(n,1))
         (* (GEN(SOME n) for 0.0) *)

         fun gOfs(SOME n)=
            if Int.<(n,0) then raise General.Size else n
         |  gOfs(NONE)=6

         fun gOg(SOME n)=
            if Int.<(n,1) then raise General.Size else n
         |  gOg(NONE)=12  


         fun fmt0 spec=
         (case spec of
            StringCvt.FIX g => fix0(gOfs g)
         |  StringCvt.SCI g => sci0(gOfs g)
         |  StringCvt.GEN g => gen0(gOg g)
         )
            
         (* We need a logarithm function.  There is one in Math
            but we haven't defined that yet. *)
         fun ln (r:real) = _pure (java.lang.Math.log(r))
         val log10= ln 10.0
         val one=IntInfone
         val two=IntInf.fromInt 2
         val ten=IntInf.fromInt 10
      in   
         fun fmt spec x=
(* RATIONALE.
   The result of fmt is not very well specified in the current version of
   the standard basis.  There is one obvious mistake, and a number of other
   choices.  Here are various points to consider.
   1) Infinities are sent to "inf" and "~inf".  This is contrary to
      the standard basis, which decrees "+inf" and "-inf", but is what
      New Jersey does.  However judging by what was said by John Reppy, the
      current version of the standard basis (which we don't have) has been
      changed following my comments on this.  The reasons "+inf" and "-inf"
      are wrong is (a) Int.toString does not output "+" for positive numbers;
      (b) Int.toString does not output "-" at all, nor should Real.fmt
      (since ML real constants may not contain -).
   2) For negative zero the sign is not printed (apparently this is what
      IEEE 754 decrees; it is also what New Jersey does).  However fmt
      WILL return strings like "~0.0" where it has a negative number where all
      significant digits are rounded away (EG if we do fmt(FIX(SOME 1)) ~0.01).
   3) Zero is deemed to have a decimal exponent of 0.
   4) FIX(SOME 0) does not produce a decimal point.
      This is contentious because the ML parser will not accept, EG
      "1000", as a real.  But Real.fromString must.  The rationale is
      that if the user asks for FIX(SOME 0) they actually want something that
      looks like an integer.  Furthermore, the specification for Real.scan
      insists that if there is a decimal point, it must be followed by at
      least one digit.
   5) The decision whether, for GEN format, to use fixed or scientific format
      is made as follows: scientific format will be used if either
      a) the decimal exponent < ~4
      or
      b) the decimal exponent > the number of digits required - 2.
       
      Test (a) was discovered because that seems to be the one New Jersey
      uses.  However it is arguable.  There is a case for replacing
      ~4 by ~3 or even ~2 because, for example, we get as many digits
      in the same number of characters in "1.2E~3" as "0.0012".

      Test (b) is also contentious.  New Jersey seems to make a mess of this
      (Real.fmt(StringCvt.GEN(SOME 1)) 1.5 returns "2.0" under 109.32!).
      Essentially we want Real.fmt to produce a string which is acceptable to 
      Real.scan and the ML parser (as a real constant).  There are in 
      fact 2 exceptions to this rule already.  Firstly "nan", "~inf" and "inf" 
      are not valid ML real constants, nor can they be accepted by Real.scan 
      under the current basis.  Secondly fmt(FIX(SOME 0)) (see point (4)) 
      returns no decimal point, making its output unacceptable to the ML parser
      as a real constant, though it is acceptable to Real.scan. There is 
      nothing we can do about the first exception. But while I think point (4) 
      reasonable, I do not think we should introduce the effect of FIX(SOME 0)
      unless the caller explicitly requests it.  Nor should 
      Real.fmt(StringCvt.GEN(SOME 1)) 1.5
      return "2.", since that is not acceptable to Real.scan or ML
      (at least one digit is always required after a decimal point).  And New 
      Jersey's habit of tacking on an extra incorrect "0" is utterly vile! 
      So we return "2E0".
   6) Rounding - we approximate what I believe to be the correct IEEE rounding
      used for other things (such as multiplication) in TO_NEAREST rounding
      mode: IE let x be the exact real value represented by the floating point
      number.  Then we choose the decimal representation with the
      specified number of digits which has exact value nearest to x.  Suppose 
      there is a tie, so that the nearest decimals are u and v with
      u<x<v and x-u = v-x.  We use u unless the last (mantissa) digit of u
      is odd, in which case we use v (normally that means we choose whichever
      of u and v has even last digit, but that's not quite true if for example
      we want SCI(SOME 0) and have to format 95, so the choice is between
      "9E1" and "1E2").  Incidentally the reason why IEEE specifies this
      strange way of breaking ties, rather than always rounding down or up, is
      so that we don't get any strange upward or downward drift when we do
      some large calculation because of the bias introduced by always breaking
      ties in the same direction.
   It was decided to implement "fmt" properly, using exact arithmetic (via
   IntInf) to get the exact sequence of digits specified by (6).  This is 
   unlike New Jersey.  In general I prefer to do things properly, however there
   is an extra very good reason in this case, since conversion the other way
   (via scan) calls the Java function java.lang.Double.valueOf, which DOES do
   it properly, at least in version 1.1.3 - I haven't checked earlier versions.
   Doing the conversion naively (using inexact multiplication by
   10) would therefore be not inverse to scan, and so the results could be
   embarassing.  However there is a substantial cost in CPU time of doing the
   job properly, since we have to manipulate big integers.  I have tried to
   streamline this within reason (using powers of 5 rather than 10 and using
   shifts where possible), so hopefully the cost will be bearable. 
   There are a number of special cases where fmt can be speeded up;
   for example see netlib (currently at cm.bell-labs.com/netlib) chapter
   fp, and references therein.  Most of these special cases seem to consist of
   spotting where we can get sufficient precision from longs or doubles.
   An interesting functional solution would be to compute the digits of the
   denominator and numerator lazily! (but I doubt if that would help much
   really).
   *)
         (case sme_unpack x of
            NAN => "nan"
         (* We do what New Jersey does rather than what the basis says *)
         |  INFINITE true => "~inf"
         |  INFINITE false => "inf"
         |  ZERO _ => fmt0 spec
         |  FINITE {negative,mantissa,exponent} =>      
         let
            (* 1.  Compute r and y such that
               10^r <= abs x < 10^{r+1}

               and y = abs x/10^r

               We do this by guessing a value and then adjusting it
               if necessary (it should not be unless x is very
               close to a power of 10). *)
            val guessed_r=floor(ln(abs x)/log10)
            val guessed_y=
               FloatFrac.make_frac
                 {mantissa=mantissa,
                  exp2=exponent,
                  exp10= Int.~ guessed_r
                  }
            fun correct_guess(guessed_r,guessed_y)=
            let
               val (yint,_)=FloatFrac.modf(guessed_y)
            in
               if IntInf.<(yint,one)
               then (* r is too large *)
                  correct_guess(Int.-(guessed_r,1),FloatFrac.mul10 guessed_y)
               else 
               if IntInf.>=(yint,ten)
               then (* r is too small *)
                  correct_guess(Int.+(guessed_r,1),FloatFrac.div10 guessed_y)
               else (* r is just right *)
                  (guessed_r,guessed_y) 
            end

            val (r,y)=correct_guess(guessed_r,guessed_y)

            (* 2.  Compute d, the number of significant digits we have to
               find. *)
            val d=
               (case spec of
                  StringCvt.SCI n => Int.+(gOfs n,1)
               |  StringCvt.FIX n => Int.max(0,Int.+(Int.+(gOfs n,r),1))
               |  StringCvt.GEN n => gOg n
               )
         in
            if d=0 (* should only happen for FIX with very small numbers *)
            then 
            let
               val s=fmt0 spec
            in
               if negative then String.^("~",s) else s
            end
            else 
            let
               (* 3.  Compute floor(y^(d-1)) (these will be the digits, modulo
                  rounding up) and the remainder. *)
               val yy=FloatFrac.mulpow10(y,Int.-(d,1))
               val (guessed_digits,remainder)=FloatFrac.modf(yy)
               (* The digits are guessed_digits, unless we round up.
                  *)
               val do_roundup=
                  (case FloatFrac.compare1(FloatFrac.mul2(remainder)) of
                     LESS => false
                  |  GREATER => true
                  |  EQUAL => 
                     (* Convention - if there is a tie we round to the
                        nearest EVEN integer *)
                        IntInf.mod(guessed_digits,two)=one
                  )
               (* 4.  Compute rounded digits (as a string) and exponent *)
               val (digit_string,exponent)=
                  let
                     val rounded_digits=
                        if do_roundup 
                        then 
                           IntInf.+(guessed_digits,one)
                        else
                           guessed_digits
 
                     val digit_string'=IntInf.toString(rounded_digits)
                  in
                     if Int.>(String.size digit_string',d)
                     then
                        (* We are in the unusual situation of having got an
                           extra digit, because of the rounding.  What to
                           do?  We can either chop off the last digit
                           (this is harmless, because it must be 0) or keep it.
                           Which depends on the format.  FIX specifies the
                           number of digits after the decimal point, so we
                           keep the last digit; SCI and GEN specify the total
                           number of digits, so we throw the 0 away. *)
                        let
                           fun chopped_digits {}=
                              String.substring(digit_string',0,d)
                           val new_digits=
                              (case spec of
                                 StringCvt.FIX _ => digit_string'
                              |  StringCvt.SCI _ => chopped_digits()
                              |  StringCvt.GEN _ => chopped_digits()
                              )
                        in
                           (new_digits,Int.+(r,1))
                        end
                     else
                        (digit_string',r)
                  end
                val decimal_form:decimal_real=
                  {negative=negative,digits=digit_string,exponent=exponent}
            in
                (case spec of
                   StringCvt.FIX _ =>
                      fix decimal_form
                |  StringCvt.SCI _ =>
                      sci decimal_form
                |  StringCvt.GEN _ =>
                      (if Int.>=(exponent,~4) andalso
                          Int.<=(exponent,Int.-(String.size digit_string,2)) 
                      then fix else sci) decimal_form
                )
            end
         end (* fmt *) 
         )

         val toString=fmt (StringCvt.GEN NONE)
      end (* local *)

      exception FOUNDNONE

      (* Finally we get onto the functions for parsing strings as reals.
         Fortunately this is easier than turning reals into strings since
         java.lang.Double.valueOf does almost what we need; the only problem
         lies in parsing the string and turning it into a Java form *)
      fun scan(char_reader:(char, 'a)StringCvt.reader) state=
      let
         (* first skip whitespace and read the sign. 
            (We do this separately so that "~0.0" returns negative
            zero, which the Java equivalent cannot be trusted to do).
            *)
         val sgn_reader:(bool,'a)StringCvt.reader =
            (* read a sign as "+", "~", "-" or "" and return
               true for negative, false for positive.  This never
               returns NONE. *)
            fn state =>
               (case char_reader state of
                  SOME(#"+",next) => SOME(false,next)
               |  SOME(#"-",next) => SOME(true,next)
               |  SOME(#"~",next) => SOME(true,next)
               |  _  => SOME(false,state)
               ) 
         (* skip the whitespace and read the sign (as a bool). *)
         val state=StringCvt.skipWS char_reader state
         val SOME(is_negative,state)=sgn_reader state

         (* To do the rest of the real we implement some basic combinators 
            on readers, all of which return strings in a form suitable
            to pass to Java. *)
         fun concat_readers(readers:(string,'a)StringCvt.reader list) state=
         (* reader which calls each reader in succession and if each returns
            SOME returns the concatenation of their string results, otherwise
            returns NONE *)
         (let
            val (final_string_list,final_state)=
               List.foldl
                  (fn (reader,(strings_sf,state_sf)) =>
                     (case reader state_sf of
                        SOME(s,next_state) =>
                           (List.::(s,strings_sf),next_state)
                     |  NONE => raise FOUNDNONE
                     )
                  )
                  ([],state)
                  readers
         in
            SOME(String.concat(List.rev final_string_list),final_state)
         end handle FOUNDNONE => NONE)

         fun mustbe_nonnull(reader1:(string,'a)StringCvt.reader) state=
         (* tranforms a string reader into one which insists that
            the result is non-null *)
         (case reader1 state of
            NONE => NONE
         |  res as SOME(s,next) => if s="" then NONE else res
         )

         fun allow_null(reader1:(string,'a)StringCvt.reader) state =
         (* transforms a string reader into one which returns "" rather than
            NONE *)
         (case reader1 state of
            NONE => SOME("",state)
         |  res => res
         ) 

         val sign_reader:(string,'a)StringCvt.reader =
            (* read a sign as "+", "~", "-" or "" and return a Java
               sign ("" for positive, "-" for negative). *)
            fn state =>
               (case char_reader state of
                  SOME(#"+",next) => SOME("",next)
               |  SOME(#"-",next) => SOME("-",next)
               |  SOME(#"~",next) => SOME("-",next)
               |  _  => SOME("",state)
               )

                  
         fun digits_reader state =
            (* read 0 or more decimal digits *)
            SOME(StringCvt.splitl (Char.isDigit) char_reader state)

         val point_reader:(string,'a)StringCvt.reader =
            (* read a decimal point returning it if there is one,
               NONE otherwise *)
            (fn state =>
               (case char_reader state of
                  SOME(#".",next) => SOME(".",next) 
               |  _ => NONE
               ))

         val e_reader:(string,'a)StringCvt.reader =
            (* read an exponent sign ("E" or "e") *)
            (fn state =>
               (case char_reader state of
                  SOME(#"E",next) => SOME("E",next)
               |  SOME(#"e",next) => SOME("E",next)
               |  _ => NONE
               ))


         (* Finally we put them together *)
         val mantissa_reader:(string,'a)StringCvt.reader =
(* read the mantissa (not including the initial whitespace or sign or
   the exponent).
   My interpretation of the regular expression in the standard basis
   definition of Real.scan is that this
   must consist of either or both of 
   1) 1 or more decimal digits
   followed by
   2) A decimal point followed by 1 or more decimal digits
   *)
            mustbe_nonnull(
               concat_readers[
                  digits_reader,
                  allow_null(
                     concat_readers[
                        point_reader,
                        mustbe_nonnull digits_reader
                        ]
                      )
                  ]
               )

         val exponent_reader:(string,'a)StringCvt.reader =
            allow_null(
               concat_readers[
                  e_reader,
                  sign_reader,
                  mustbe_nonnull digits_reader
                  ])

         (* read the real, apart from the sign. *)
         val realstring_reader:(string,'a)StringCvt.reader =
            concat_readers[
               mantissa_reader,
               exponent_reader
               ]
      in
         (case realstring_reader state of
            SOME(s,next) =>
         let
            val r= 
              (Prim.unsafeValOf(java.lang.Double.valueOf(s))).#doubleValue()
         in
            if isFinite r 
            then
               SOME(if is_negative then ~r else r,next)
            else raise General.Overflow
         end
         |  NONE => NONE
         ) 
      end (* scan *)

      val fromString=StringCvt.scanString scan


(* ------------------------------------------------------------------------- 

   Math structure.  This will eventually be bound by the signature
   MATH.  
   *)
      structure Math=
      struct
         type real=real
      
         val pi = java.lang.Math.PI
         val e  = java.lang.Math.E
         
         (* The Java book says that the functions in Math, which we use
            heavily, should produce the same results as the C functions in 
            netlib.  The current URL (the one in the book is out of date)
            for the netlib is
               http://cm.bell-labs.com/netlib/index.html
            This needs to be checked to verify agreement
            with the ML standard basis definition. 
         
            Note on accuracy.  It would probably be quite difficult for
            any implementation to guarantee that the answer returned by
            transcendental functions was guaranteed to be the closest 
            representable double to the exact answer, because arbitrarily
            large amounts of work would be required at boundary cases.  The 
            absolute
            best I imagine one can expect is that the answer returned is
            one of the closest representable doubles on either side of
            the exact answer.  However I imagine there will many implementations
            of java/lang/Math which are much worse than this.  Fortunately
            the ML basis document appears to make no claims as to accuracy. 
         
            In the interests of balance, I should mention that consistency
            is in some ways just as important as accuracy; EG a
            monotonic function should remain monotonic; functions should 
            appear smooth even at points where we switch from one power
            series expansion to another, and so on.  But this isn't
            under our control unless, again, we reimplement the lot from
            scratch. *)
         
         fun sqrt (r:real) = java.lang.Math.sqrt(r)
         (* Yes, netlib's sqrt(~0.0) does return ~0.0 *)
         
         fun sin (r:real) = java.lang.Math.sin(r)
         (* Yes netlib's sin function returns NaN at infinity (there is
            more detail in netlib about which NaN *)
         fun cos (r:real) = java.lang.Math.cos(r)
         (* Ditto cos *)
         fun tan (r:real) = java.lang.Math.tan(r)
         (* Ditto tan.  Which infinity tan produces is not specified in either
            document. *)
         fun asin (r:real) = java.lang.Math.asin(r)
         (* Probably, the result of asin is in [-pi/2,pi/2] and acos is 
            in [0,pi) or else
            are NaN, or at least so it appears from my cursory reading of the
            C source. CHECK THESE FUNCTIONS *)
         fun acos (r:real) = java.lang.Math.acos(r)
         fun atan (r:real) = java.lang.Math.atan(r)
         (* Yes, the result of atan is in [-pi/2,pi/2] with appropriate signs 
            for infinite arguments *)
         fun atan2 (r1:real,r2:real) = java.lang.Math.atan2(r1, r2)
         (* netlib says: 
         >   Special cases:
         >      *
         >      *      ATAN2((anything), NaN ) is NaN;
         >      *      ATAN2(NAN , (anything) ) is NaN;
         >      *      ATAN2(+-0, +(anything but NaN)) is +-0  ;
         >      *      ATAN2(+-0, -(anything but NaN)) is +-pi ;
         >      *      ATAN2(+-(anything but 0 and NaN), 0) is +-pi/2;
         >      *      ATAN2(+-(anything but INF and NaN), +INF) is +-0 ;
         >      *      ATAN2(+-(anything but INF and NaN), -INF) is +-pi;
         >      *      ATAN2(+-INF,+INF ) is +-pi/4 ;
         >      *      ATAN2(+-INF,-INF ) is +-3pi/4;
         >      *      ATAN2(+-INF, (anything but,0,NaN, and INF)) is +-pi/2;
            This does indeed match the table in the basis document, which is
            hardly surprising *)
              
         fun exp (r:real) = java.lang.Math.exp(r)
         
         fun pow(x:real,y:real)= java.lang.Math.pow(x,y)
         (* As with atan, again I quote the relevant comment from netlib: 
         >    Special cases:
         >    *      1.  (anything) ** 0  is 1
         >    *      2.  (anything) ** 1  is itself
         >    *      3.  (anything) ** NAN is NAN
         >    *      4.  NAN ** (anything except 0) is NAN
         >    *      5.  +-(|x| > 1) **  +INF is +INF
         >    *      6.  +-(|x| > 1) **  -INF is +0
         >    *      7.  +-(|x| < 1) **  +INF is +0
         >    *      8.  +-(|x| < 1) **  -INF is +INF
         >    *      9.  +-1         ** +-INF is NAN
         >    *      10. +0 ** (+anything except 0, NAN)               is +0
         >    *      11. -0 ** (+anything except 0, NAN, odd integer)  is +0
         >    *      12. +0 ** (-anything except 0, NAN)               is +INF
         >    *      13. -0 ** (-anything except 0, NAN, odd integer)  is +INF
         >    *      14. -0 ** (odd integer) = -( +0 ** (odd integer) )
         >    *      15. +INF ** (+anything except 0,NAN) is +INF
         >    *      16. +INF ** (-anything except 0,NAN) is +0
         >    *      17. -INF ** (anything)  = -0 ** (-anything)
         >    *      18. (-anything) ** (integer) is (-1)**(integer)*(+anything**integer)
         >    *      19. (-anything except 0 and inf) ** (non-integer) is NAN
         Again, this checks with the standard basis document.
         *)     
         
         fun ln (r:real) = _pure (java.lang.Math.log(r))
         (* Yes, log 0 is -inf, yes log inf is inf, yes log x<0 is NaN *)
         
         val invln10= 1.0/ln 10.0
         fun log10 r = ln r * invln10
         
         (* OK, now we come to the tricky bit, namely the hyperbolic functions.
            These are not in java/lang/Math, so we must synthesise them. 
         
            The algorithms for these are stolen, with various modifications,
            from the fdlibm algorithms in netlib.  This is OK so long as we 
            include the following copyright notice, which of course applies
            only to the portions of the code for the functions
            expm1, sinh, cosh, tanh which have something in common
            with one of the files
               e_cosh.c      
               e_sinh.c      
               s_expm1.c     
               s_tanh.c
            in netlib/fd

            (copies of these files should be in docs/Math/netlib)            
         
          * ====================================================
          * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
          *
          * Developed at SunSoft, a Sun Microsystems, Inc. business.
          * Permission to use, copy, modify, and distribute this
          * software is freely granted, provided that this notice 
          * is preserved.
          * ====================================================
         
            *)
         (* ---------- Constants -------------------- *)
         
         (* scaled coefficients related to expm1.
            It has been verified by crosschecking with s_expm1.c that
            these real constants match exactly those there, for
            SML/NJ 109.32.  This verification was done by comparing
            the hexadecimal representations given there with the output
            of JavaDouble.pack.pack. *)
         val Q1  =  ~3.33333333333331316428e~02
         val Q2  =   1.58730158725481460165e~03
         val Q3  =  ~7.93650757867487942473e~05
         val Q4  =   4.00821782732936239552e~06
         val Q5  =  ~2.01099218183624371326e~07
         
         (* ln2_hi and ln2_lo are used for various trickery in expm1 *)   
         val ln2_hi = 6.93147180369123816490e~01
         val ln2_lo = 1.90821492927058770002e~10
         
         val log2=ln(2.0)
         val mlog2= ~log2
         val halflog2=0.5*log2
         val mhalflog2= ~halflog2
         val pow2m55=pow2 ~55        
         
         val lnovt=709.7827128
         (* For x<=lnovt, exp x should not overflow (this was computed by 
            taking ln(maxFinite) and removing a few digits for security).
            *)
         
         
         (* ---------- Service functions -------------- *)
         fun getE(x:real)=
         let
            val hfx = 0.5*x
            val hxs = x*hfx
            val r1 = 1.0+hxs*(Q1+hxs*(Q2+hxs*(Q3+hxs*(Q4+hxs*Q5))))
            val t  = 3.0-r1*hfx
            val e  = hxs*((r1-t)/(6.0 - x*t))
         in
            (e,hxs)
         end
         
         fun expm1(x:real)=
         (* expm1 returns e^x - 1 for all x *)
         if x>=log2 orelse x<= mlog2    
         then (* e^x>=2 or <= -0.5 so exp x should have enough precision *)
            (exp x - 1.0)
         else
            if x>halflog2
            then
               (* k=1 for s_expm1.c *)
            let
               val hi= x-ln2_hi
               val lo= ln2_lo   
               val x= hi-lo
               val c= (hi-x)-lo
               val (e,hxs)= getE x
               val e=(x*(e-c) - c)
               val e=e-hxs
            in
               if x< ~0.25 
               then ~2.0 * (e-(x+0.5))
               else 1.0 + 2.0*(x-e)
            end
            else if x< mhalflog2
            then
               (* k= -1 for s_expm1.c *)
            let            
               val hi=x+ln2_hi
               val lo= ~ln2_lo
               val x=hi-lo
               val c=(hi-x)-lo
               val (e,hxs)=getE x
               val e=(x*(e-c)-c)
               val e=e-hxs
            in
               0.5*(x-e)-0.5
            end
            else (* k=0 in s_expm1.c *)
            let
               val (e,hxs)=getE x
            in   
               x-(x*e - hxs)
            end
         
         (* ------- Hyperbolic Functions ----------- *)
         
         fun sinh x=
         (* We adopt approximately the same strategy as netlib *)
         let
            val absx=abs x
            val signed=signBit x (* we want sinh(~0.0)= ~0.0 and so on *)
            val h=if signed then ~0.5 else 0.5 (* final multiplier *)
         in
            if absx<22.0
            then
            (* We do not do the netlib optimisation sinh(x)=x for tiny x *)
            let
               val t=expm1 absx
            in 
               h*(t+t/(t+1.0))
            end
            else
               (* Would exp absx overflow? *)
               if absx<=lnovt
               then (* No. *)
                  h*(exp absx)
               else (* Yes *)
               let
                  val exphalf=exp(0.5*absx)
               in
                  (h*exphalf)*exphalf
               end
         end 
                  
         fun cosh x=
         let
            val absx=abs x (* cosh(~x)=cosh x *)
         in
            if absx<=22.0
            then
               if absx<=halflog2
               then
               let
                  val t=expm1 absx
                  val w=1.0+t
               in
                  1.0 + (t*t)/(w+w)
               end
               else
               let
                  val t=exp absx
               in
                  0.5*t + 0.5/t
               end
            else  
               if absx<=lnovt 
               then
                  0.5 * exp absx
               else
               let
                  val abshalf=exp(0.5*absx)
               in
                  (0.5*abshalf)*abshalf
               end
         end            
         
         fun tanh x= 
         let
            val absx=abs x
            val abstanh=
               if absx < 22.0
               then
                  if absx < pow2m55 then absx
                  else 
                    if absx >= 1.0
                    then
                    let
                       val t=expm1(2.0*absx)
                    in
                       1.0 - 2.0/(2.0 + t)
                    end
                    else
                    let
                       val t=expm1(~2.0*absx)
                    in
                       ~t/(2.0+t)
                    end
               else
                    1.0
         in
            if signBit x then ~abstanh else abstanh
         end
      end (* of struct Math *)
   end (* local *)

end (* struct *)













