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

(* the Attribute structure contains general code for decoding attributes. *)
structure Attribute:>ATTRIBUTE=
struct
   datatype Attribute=
   (* Attribute is supposed to consist of the various attributes that
      decode_attribute is supposed to be able to process. *)
      Exceptions of Exceptions.t
   |  ConstantValue of ConstantValue.t
   |  Signature of Signature.t 
   |  Code of CodeAttr.t
   |  Deprecated of Deprecated.t

   type attributes=Attribute list
   val empty=[]

   fun setExceptions(x,a)=(Exceptions x)::a
   fun setConstantValue(x,a)=(ConstantValue x)::a
   fun setSignature(x,a)=(Signature x)::a
   fun setCode(x,a)=(Code x)::a
   fun setDeprecated(x,a)=(Deprecated x)::a

   fun getExceptions []=NONE
   |   getExceptions(Exceptions x::_)=SOME x
   |   getExceptions(_::t)=getExceptions t

   fun getConstantValue []=NONE
   |   getConstantValue(ConstantValue x::_)=SOME x
   |   getConstantValue(_::t)=getConstantValue t

   fun getSignature []=NONE
   |   getSignature(Signature x::_)=SOME x
   |   getSignature(_::t)=getSignature t


   fun getDeprecated []=NONE
   |   getDeprecated(Deprecated x::_)=SOME x
   |   getDeprecated(_::t)=getDeprecated t

   datatype Attribute'=
      Exceptions' of Exceptions.t2
   |  ConstantValue' of ConstantValue.t2
   |  Signature' of Signature.t2
   |  Code' of CodeAttr.t2
   |  Deprecated' of Deprecated.t2

   type hndl=unit->int
   type t2=(hndl*Attribute') list

   fun pool_pass(A,attributes)=
      List.map
         (fn (Exceptions x) =>
            (AllPools.r_Utf8(A,Exceptions.name),
               Exceptions'(Exceptions.pool_pass(A,x)))
         |  (ConstantValue x) =>
            (AllPools.r_Utf8(A,ConstantValue.name),
               ConstantValue'(ConstantValue.pool_pass(A,x)))
         |  (Code x) =>
            (AllPools.r_Utf8(A,CodeAttr.name),
               Code'(CodeAttr.pool_pass(A,x)))
         |  (Signature x) =>
            (AllPools.r_Utf8(A,Signature.name),
               Signature'(Signature.pool_pass(A,x)))
         |  (Deprecated x) =>
            (AllPools.r_Utf8(A,Deprecated.name),
               Deprecated'(Deprecated.pool_pass(A,x)))
         ) 
         attributes

   fun bytecode_pass attributes'=
      W8.combine(
         List.map
            (fn (name_hndl,data) =>
               let
                  val data_w8=
                     (case data of
                        Exceptions' x => Exceptions.bytecode_pass x
                     |  ConstantValue' x => ConstantValue.bytecode_pass x
                     |  Code' x => CodeAttr.bytecode_pass x
                     |  Signature' x => Signature.bytecode_pass x
                     |  Deprecated' x => Deprecated.bytecode_pass x
                     )
               in
                  W8.concat[
                     W8.vv1(Numbers.h2 name_hndl),
                     W8.vv1(Numbers.u4(W8.length data_w8)),
                     data_w8
                     ]
               end
               )
            attributes'
         )

   fun decode_attribute(pis as (pool,is))=
   let
      val name=ReadPool.get_utf8(pool,ReadInts.u2 is)
      val nbytes=ReadInts.u4 is
   in
      if JavaString.equal(name,Exceptions.name) then
         SOME(Exceptions(Exceptions.decode_attr(pis,nbytes)))
      else
      if JavaString.equal(name,ConstantValue.name) then
         SOME(ConstantValue(ConstantValue.decode_attr(pis,nbytes)))
      else
      if JavaString.equal(name,Signature.name) 
      then
         SOME(Signature(Signature.decode_attr(pis,nbytes)))
      else
      if JavaString.equal(name,Deprecated.name)
      then
         SOME(Deprecated(Deprecated.decode_attr(pis,nbytes)))
      else
      let
         val _=ReadInts.skipbytes(is,nbytes)
      in
         NONE
      end
   end

   fun decode_attributes(pis as (pool,is))=
      ReadInts.getlistpartial (fn is=>decode_attribute pis) is
end







