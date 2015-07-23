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

(* AllPools contains the code for gluing together all the constant
   pools, plus functions for adding particular items to the
   constant pool and sorting out all the pool_handles. *)
structure AllPools:>ALLPOOLS=
struct
   type hndl=unit->int

   structure ClassPool=Pool(ConstClass)
   structure FrefPool=Pool(ConstFref)
   structure MrefPool=Pool(ConstMref)
   structure IMrefPool=Pool(ConstIMref)
   structure StringPool=Pool(ConstString)
   structure IntPool=Pool(ConstInt)
   structure FloatPool=Pool(ConstFloat)
   structure LongPool=Pool(ConstLong)
   structure DoublePool=Pool(ConstDouble)
   structure NnTPool=Pool(ConstNnT)
   structure Utf8Pool=Pool(ConstUtf8)

   type AllPools={
      Classes: ClassPool.pool,
      Frefs  : FrefPool.pool,
      Mrefs  : MrefPool.pool,
      IMrefs : IMrefPool.pool,
      Strings: StringPool.pool,
      Ints   : IntPool.pool,
      Floats : FloatPool.pool,
      Longs  : LongPool.pool,
      Doubles: DoublePool.pool,
      NnTs   : NnTPool.pool,
      Utf8s  : Utf8Pool.pool
      }

   fun create ():AllPools={
(* the numbers give the size of the hash tables and may be adjusted to
   conform with expected sizes *)

      Classes= ClassPool.create(100),
      Frefs  = FrefPool.create(100),
      Mrefs  = MrefPool.create(100),
      IMrefs = IMrefPool.create(100),
      Strings= StringPool.create(100),
      Ints   = IntPool.create(100),
      Floats = FloatPool.create(100),
      Longs  = LongPool.create(100),
      Doubles= DoublePool.create(100),
      NnTs   = NnTPool.create(100),
      Utf8s  = Utf8Pool.create(100)
      }

   fun resolve ({Classes,Frefs,Mrefs,IMrefs,Strings,Ints,Floats,Longs,
                Doubles,NnTs,Utf8s}:AllPools)=let
(* resolve produces a Word8Vector encoding the whole constant pool *)

(* we order the constant pool as follows: ints, floats,
   strings, doubles, longs, Utf8s, and other things.
   The reasons for this are as follows:
   1) if ints, floats and strings have a 1-byte index rather than
      a 2-byte index, they can be loaded with a 2-byte instruction rather
      than a 3-byte one.
   2) things which are going to be used arbitrarily often should be put
      before the "other things", which are class references, etcetera,
      which should be replaced by _quick instructions if the VM machine has
      any brains at all.
   *)

(* nevertheless, Utf8s have to be resolved before strings, so we adjust
   the irefs specially for them. *)

   val int_size=IntPool.pool_size(Ints)
   val float_size=FloatPool.pool_size(Floats)
   val string_size=StringPool.pool_size(Strings)
   val double_size=DoublePool.pool_size(Doubles)
   val long_size=LongPool.pool_size(Longs)

   val iref=(ref (1+int_size+float_size+string_size+double_size+long_size))
                  (* the first pool index is 1 *)
   val Utf8bytes=Utf8Pool.resolve(Utf8s,iref)
   val next_iref= !iref
   val _= iref:=1 (* the first pool index is 1 *)

(* We will have to prefix the pool with the number of items
   in it, +1 (for the absent zeroth element) *)
      val pool=W8.concat [
(* NB this code depends very much on the order of evaluation of elements
   of a list! *)

      IntPool.resolve(Ints,iref),
      FloatPool.resolve(Floats,iref),
      StringPool.resolve(Strings,iref), (* pool_handle to Utf8 *)
      DoublePool.resolve(Doubles,iref),
      LongPool.resolve(Longs,iref),
      Utf8bytes,

(* then . . *)
      let val _= iref:=next_iref in
         ClassPool.resolve(Classes,iref) (* pool_handle to Utf8 *)
      end,

      NnTPool.resolve(NnTs,iref), (*  two pool_handles to Utf8 *)

      FrefPool.resolve(Frefs,iref), (* pool_handle to Class and NnT *)
      MrefPool.resolve(Mrefs,iref), (* ditto *)
      IMrefPool.resolve(IMrefs,iref) (* ditto *)

      ]
   in W8.concat[
      W8.vv1(Numbers.u2(!iref)),
      pool
      ]
   end

(* We now come to the functions for actually adding things to the
   collection of pools.  These take as argument a value of AllPools +
   the thing being added and return a function unit->int which, when the
   pool is resolved, will return the constant pool index (before then,
   the function raises an UnresolvedHandle exception) *)

   fun r_Utf8(A:AllPools,ji)=let
      val h=Utf8Pool.add(#Utf8s(A),ji)
   in
      (fn {}=>Handles.lookup(h))
   end

   fun r_int(A:AllPools,ji)=let
      val h=IntPool.add(#Ints(A),ji)
   in
      (fn {}=>Handles.lookup(h))
   end

   fun r_long(A:AllPools,ji)=let
      val h=LongPool.add(#Longs(A),ji)
   in
      (fn {}=>Handles.lookup(h))
   end

   fun r_float(A:AllPools,ji)=let
      val h=FloatPool.add(#Floats(A),ji)
   in
      (fn {}=>Handles.lookup(h))
   end

   fun r_double(A:AllPools,ji)=let
      val h=DoublePool.add(#Doubles(A),ji)
   in
      (fn {}=>Handles.lookup(h))
   end

(* things now get a little more interesting. . *)
   fun r_string(A:AllPools,ji)=let
      val h1=Utf8Pool.add(#Utf8s(A),ji)
      val h=StringPool.add(#Strings(A),h1)
   in
      (fn {}=>Handles.lookup(h))
   end

   local
      open Descriptors
   in
      fun r_class(A:AllPools,class)=let
	 val h1=Utf8Pool.add(#Utf8s(A),ClassHandle.name class)
	 val h=ClassPool.add(#Classes(A),h1)
      in
	 (fn {}=>Handles.lookup(h))
      end

     fun r_array(A:AllPools,jt)=let
        (* This is for array types which go in the class pool *)
        val h1=Utf8Pool.add(#Utf8s(A),Descriptors.fdout jt)
        val h=ClassPool.add(#Classes(A),h1)
      in
	 (fn {}=>Handles.lookup(h))
      end

(* We now have to deal with field, method and interface method references.
   This is really quite complicated. . .*)
      fun r_field(A:AllPools,fref{class,name,desc})=let
	 val h1=Utf8Pool.add(#Utf8s(A),ClassHandle.name(class))
	 val h2=ClassPool.add(#Classes(A),h1)
	 (* h2 gives the class_index *)
	 val h3=Utf8Pool.add(#Utf8s(A),name)
	 val h4=Utf8Pool.add(#Utf8s(A),fdout(desc))
         val h5=NnTPool.add(#NnTs(A),
            Handles.make_pair(h3,h4))
         (* h5 gives the name_and_type index *)
         val h6=FrefPool.add(#Frefs(A),
            Handles.make_pair(h2,h5))
      in
         (fn {}=>Handles.lookup(h6))
      end

(* Fortunately methods and interface methods are almost exactly similar
   to fields *)
      fun r_method(A:AllPools,mref{class,name,desc})=let
	 val h1=Utf8Pool.add(#Utf8s(A),ClassHandle.name(class))
	 val h2=ClassPool.add(#Classes(A),h1)
	 (* h2 gives the class_index *)
	 val h3=Utf8Pool.add(#Utf8s(A),name)
	 val h4=Utf8Pool.add(#Utf8s(A),mdout(desc))
         val h5=NnTPool.add(#NnTs(A),
            Handles.make_pair(h3,h4))
         (* h5 gives the name_and_type index *)
         val h6=MrefPool.add(#Mrefs(A),
            Handles.make_pair(h2,h5))
      in
         (fn {}=>Handles.lookup(h6))
      end

      fun r_interface_method(A:AllPools,iref{class,name,desc})=let
	 val h1=Utf8Pool.add(#Utf8s(A),ClassHandle.name(class))
	 val h2=ClassPool.add(#Classes(A),h1)
	 (* h2 gives the class_index *)
	 val h3=Utf8Pool.add(#Utf8s(A),name)
	 val h4=Utf8Pool.add(#Utf8s(A),mdout(desc))
         val h5=NnTPool.add(#NnTs(A),
            Handles.make_pair(h3,h4))
         (* h5 gives the name_and_type index *)
         val h6=IMrefPool.add(#IMrefs(A),
            Handles.make_pair(h2,h5))
      in
         (fn {}=>Handles.lookup(h6))
      end
   end
end
