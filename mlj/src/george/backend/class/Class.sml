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

structure Class:>CLASS=
struct
   datatype flag=PUBLIC|FINAL|INTERFACE|ABSTRACT

   datatype class_data=
(*
      simple of {
      	 this:ClassHandle.Handle, (* name of this class *)
      	 flags:flag list,
      	 super:ClassHandle.Handle option,
            (* name of superclass.  If NONE there is no superclass
               (this must be java/lang/Object!).
               *)
      	 fields:Field.field_data list,
      	 methods:Method.method_data list
      	 }
   |  
*)
      middling of {
      	 this:ClassHandle.Handle, (* name of this class *)
      	 flags:flag list,
      	 super:ClassHandle.Handle option,
            (* name of superclass.  If NONE there is no superclass
               (this must be java/lang/Object!).
               *)
         interfaces:ClassHandle.Handle list,
      	 fields:Field.field_data list,
      	 methods:Method.method_data list,
         attributes:Attribute.attributes
      	 }

   fun flag_bit(PUBLIC)      =0wx1
   |   flag_bit(FINAL)       =0wx10
   |   flag_bit(INTERFACE)   =0wx200
   |   flag_bit(ABSTRACT)    =0wx400
   
   val flags=[PUBLIC,FINAL,INTERFACE,ABSTRACT]
   
   fun flag_word f=let
   in
      List.foldl Word.orb 0wx20 (* the 0wx20 sets the ACC_SUPER flag
                                   automatically *)
      (map flag_bit f)
   end
   
   fun decode_flags w=
      List.filter 
         (fn flag => Word.andb(flag_bit flag,w)<>0w0)
         flags

   val CLASSMAGIC:Word32.word=0wxCAFEBABE

   local
      val minor_version=3
      val major_version=45
      open Numbers
   in
      val version=W8.vv1(
         Word8Vector.concat[W4 CLASSMAGIC,u2 minor_version,u2 major_version])
   end

   fun compile(
      middling{this,flags,super,interfaces,fields,methods,attributes})=
   let
      val fword=flag_word flags
      val A=AllPools.create()
      val pooled_fields=List.map (fn fd=>Field.pool_pass(A,fd)) fields
      val pooled_methods=List.map(fn md=>Method.pool_pass(A,md)) methods
      val pooled_this=AllPools.r_class(A,this)
      val pooled_super=
      (case super of
         SOME c => AllPools.r_class(A,c)
      |  NONE => (fn {} => 0)
      )
      val pooled_interfaces=List.map (fn cl=>AllPools.r_class(A,cl))
         interfaces
      val pooled_attributes=Attribute.pool_pass(A,attributes)
      val pool = AllPools.resolve A
      open W8
      open Numbers
   in
      W8.concat[
         version,
         pool,
         W8.fromvList[
            w2 fword,
            h2 pooled_this,
            h2 pooled_super
            ],
         W8.combine(List.map (vv1 o h2) pooled_interfaces),
         W8.combine(List.map Field.bytecode_pass pooled_fields),
         W8.combine(List.map Method.bytecode_pass pooled_methods),
         Attribute.bytecode_pass pooled_attributes
         ]
   end (* let *)

   fun save(file_name,file_contents)=let
      val stream=BinIO.openOut(file_name)
      val ()=W8.output(stream,file_contents)
      val ()=BinIO.closeOut(stream)
   in {}
   end

   fun do_all(file_name,simple_class_data)=
      save(file_name,compile simple_class_data)

   fun zip_do_all(zipfile,file_name,simple_class_data)=
      Zip.zipOutput(zipfile,file_name,
         W8.toWord8(compile(simple_class_data)),Zip.STORED)
  
   val quick_directory=ref ""
   fun quick b=let
      val middling{this,...}=b

      val SOME name=JavaString.toString(ClassHandle.name this)
      (* NONE indicates that the class has non-ASCII characters in its
         name *)
   in
      do_all(String.concat[!quick_directory,name,".class"],b)
   end

   fun zip_quick(zipfile,b)=let
      val middling{this,...}=b
      val SOME name=JavaString.toString(ClassHandle.name this)
      (* NONE indicates that the class has non-ASCII characters in its
         name *)
   in
      zip_do_all(zipfile,String.concat[!quick_directory,name,".class"],b)
   end

   local
      open Assert
      open ReadInts
      open ReadPool
   in
      fun decode_class is=
      let
         val magic=W4 is
         val ()=must_assert(magic=CLASSMAGIC,"Not a class file")
         val (minor_version,major_version)=(u2 is,u2 is)
         val pool=readpool is
         val flags=decode_flags(w2 is)
         val this=get_class(pool,u2 is)
         val super=
            (case u2 is of
               0 => NONE
            |  n => SOME(get_class(pool,n))
            )
         val interfaces=
            getlist (fn is => get_class(pool,u2 is)) is
         val fields=
            getlist (fn is => Field.decode_field(pool,is)) is
         val methods=
            getlist (fn is => Method.decode_method(pool,is)) is
         val attributes=
            Attribute.decode_attributes(pool,is)
      in     
         middling {
            flags=flags,
            this=this,
            super=super,
            interfaces=interfaces,
            fields=fields,
            methods=methods,
            attributes=attributes
            }
       end 
   end
end (* struct *)
