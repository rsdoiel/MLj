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

(* Operations we might need on fractions to implement floating point <> 
   decimal conversion.  All operations should be exact *)
structure FloatFrac:>FLOATFRAC =
struct
   local
      open IntInf
      open MLJIntInfUtils
      open General
      val op= = Prim.=

      val zero=fromInt 0
      val one=fromInt 1
      val five=fromInt 5
      val ten=fromInt 10


      fun mulpow2(x,n:Int.int)=IntInf.<<(x,Word.fromInt n)
      fun pow2(n:Int.int)=IntInf.<<(one,Word.fromInt n)
    
      (* We need to compute powers of 5 rapidly.  We do this
         by precomputing 5^i for i from 0 to fivepow.  To compute
         5^k we compute ((5^27)^(k quot fivepow)) and
         multiply it by 5^(k rem fivepow). *)
      val fivepow=27 
      (* This value is chosen because 5^27 is the highest power of 5 to fit
         in a 64 bit signed or unsigned integer. *)

      val pow5table=
         _pure (
         let
           val mulreg=Prim.ref one
         in
          Vector.tabulate(Int.+(fivepow,1),
            fn _ =>
               let         
                  val ans= !mulreg
                  val _ = mulreg:= ans*five
               in
                  ans
               end)
         end)

      fun pow5 n=
         (* return nth power of 5 (n>=0) *)
      let
         val q=Int.quot(n,fivepow)
         val r=Int.rem(n,fivepow)
         val lowpart=Vector.sub(pow5table,r)
(*
                  val _ = TextIO.print ("\nlowpart = ")
                  val _ = TextIO.print (IntInf.toString lowpart)
*)
      in
         (case q of
            0 => lowpart
         |  _ =>
            pow(Vector.sub(pow5table,fivepow),q)*lowpart
         )
      end

      fun imulpow10(x:int,d:Int32.int)=
         mulpow2(x*(pow5 d),d)
   in      
      datatype frac=
         F of 
           {num:int,
            denom:int
            }
   
      fun make_frac
        {mantissa:Int64.int,
         exp2:Int32.int,
         exp10:Int32.int
         } = 
      let
         val e2=Int.+(exp2,exp10)
         val e5=exp10
       
         fun mulp5ifpos n value=
            if Int.<=(n,0) then value else value*(pow5 n)
         fun mulp2ifpos n value=
            if Int.<=(n,0) then value else mulpow2(value,n)
      in
         F{
            num=
               ((mulp2ifpos e2) o (mulp5ifpos e5))
               (fromFixedInt mantissa)  ,
            denom=
               if Int.<(e2,0) 
               then
                  mulp5ifpos(Int.~ e5) (pow2 (Int.~ e2))
               else
                  if Int.<(e5,0) 
                  then pow5(Int.~ e5) 
                  else one
            }
      end

      fun mul2(F{num,denom})=
         F{num=mulpow2(num,1),denom=denom}
      fun div2(F{num,denom})=
         F{num=num,denom=mulpow2(denom,1)}

      fun mul10(F{num,denom})=
         F{num=num*ten,denom=denom}
      fun mulpow10(f as F{num,denom},d)=
         if Int.>(d,0) 
         then 
            F{num=imulpow10(num,d),denom=denom}
         else if Int.<(d,0)
         then
            F{num=num,denom=imulpow10(denom,Int.~ d)}
         else f
 
      fun div10(F{num,denom})=
         F{num=num,denom=denom*ten}

      fun compare1(F{num,denom})=compare(num,denom)
      fun is0(F{num,denom})=(num=zero)

      fun modf(F{num,denom})=
      let
         val (q,r)=quotRem(num,denom)
      in
         (q,F{num=r,denom=denom})
      end
   end (* local *)
end (* struct *)

