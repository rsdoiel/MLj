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

(* ReadPool:>READPOOL reads a constant pool (including the constant pool
   count) from the supplied instream.
   *)
structure ReadPool:>READPOOL=
struct
   (* The pool is a vector of the following quantities . . *)
   datatype item=
      Utf8 of JavaString.t
   |  empty (* appears in the blank slot in entry 0 and after double word
               quantities *)
   (* The following pool types are not yet decoded so no data is
      attached to them *)
   |  String of int
   |  Integer of JavaInt.t
   |  Float of JavaFloat.t
   |  Double of JavaDouble.t
   |  Long of JavaLong.t
   |  iClass of int
   (* We don't actually need iField, iMethod and iImethod yet *)
   |  iField of int*int
   |  iMethod of int*int
   |  iImethod of int*int
   |  NnT of int*int (* used to construct the others *)
   (* ( the ints are indices in the constant pool ) *)

   type pool=item vector

   fun readpool is=
   let
      val length=ReadInts.u2 is
      fun get_item ()=
      (* get_item gets an item from the pool *)
      let
         val tag=ReadInts.u1 is
      in
         (case tag of
            7 => iClass(ReadInts.u2 is)
         |  9 => iField(ReadInts.u2 is,ReadInts.u2 is)
         |  10 => iMethod(ReadInts.u2 is,ReadInts.u2 is)
         |  11 => iImethod(ReadInts.u2 is,ReadInts.u2 is)
         |  8 => String(ReadInts.u2 is)
         |  3 => Integer(JavaInt.getint is)
         |  4 => Float(JavaFloat.getfloat is)
         |  5 => Long(JavaLong.getlong is) 
         |  6 => Double(JavaDouble.getdouble is) 
         |  12 =>
            NnT(ReadInts.u2 is,ReadInts.u2 is)
         |  1 =>
            Utf8(JavaString.getstring is)
         |  _ => Assert.fail("Bad tag "^Int.toString tag^" in constant pool")
         )
      end

      fun skip t= (* Number of slots in the constant pool t requires *)
      (case t of
         Long _ => 2
      |  Double _ => 2
      |  _ => 1
      )

      val prepool=Array.array(length,empty) 
      fun getitems(pool_index)=
      (* get the items in the constant pool and put them in prepool *)
         if pool_index>=length then 
         let 
           val _= Assert.assert(pool_index=length,
"Missing empty slot in constant pool for double word quantity"
              )
         in 
            {}
         end
         else
         let
            val item=get_item()
            val _= Array.update(prepool,pool_index,item)
         in
            getitems(pool_index+skip item)
         end
      val _=getitems(1)

      val vec=Array.extract(prepool,0,NONE)
   in
      vec
   end

   fun badent()=Assert.fail "Entry in constant pool has the wrong type"

   fun sub(vec,i)=
   (Vector.sub(vec,i) handle Subscript => 
       Assert.fail "Constant pool index out of bounds")

   fun get_utf8 vi=
   (case sub vi of
      Utf8 s => s
   |  _ => badent()
   )

   fun get_class(vi as (vec,_))=
   (case sub vi of
      iClass i => ClassHandle.unknown(get_utf8(vec,i))
   |  _ => badent()
   )

   fun get_string(vi as (vec,_))=
   (case sub vi of
      String i => get_utf8(vec,i)
   |  _ => badent()
   )

   fun get_int(vi as (vec,_))=
   (case sub vi of
      Integer ji => ji
   |  _ => badent()
   )

   fun get_long(vi as (vec,_))=
   (case sub vi of
      Long jl => jl
   |  _ => badent()
   )

   fun get_float(vi as (vec,_))=
   (case sub vi of
      Float jf => jf
   |  _ => badent()
   )

   fun get_double(vi as (vec,_))=
   (case sub vi of
      Double jd => jd
   |  _ => badent()
   )

   fun get_const(vi as (vec,_))=
   (case sub vi of
      String i => Constants.STRING(get_utf8(vec,i))
   |  Integer ji => Constants.INT(ji)
   |  Long jl => Constants.LONG(jl)
   |  Float jf=> Constants.FLOAT(jf)
   |  Double jd => Constants.DOUBLE(jd)
   |  _ => badent()
   )

   fun get_mdesc vi=
   let
      val s=get_utf8 vi
   in
      Descriptors.mdin s
   end

   fun get_fdesc vi=
   let
      val s=get_utf8 vi
   in
      Descriptors.fdin s
   end

end





