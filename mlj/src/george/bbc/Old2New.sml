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

(* Old2New is a functor which provides an implemention of the
   new BLOCKS signature in terms of the old OBLOCKS signature.
   It is used for easing the conversion process.
   *)
functor Old2New(O:OBLOCKS):BLOCKS=
struct
   open O

   type old_block_descriptor=block_descriptor
   val old_D=D

   datatype block_descriptor=D of {input:Types.java_type list}
   fun convert_bd(D{input})=old_D{input=input,output=[]}
  
   type block_operation=operation

   val old_aload=aload
   fun aload(ah,c)=old_aload
     (if ArrayHandle.is_mutable ah then MUTABLE else IMMUTABLE,
      c)

   val old_astore=astore
   fun astore(ah,c)=old_astore
     (if ArrayHandle.is_mutable ah then MUTABLE else IMMUTABLE,
      c)

   fun make_fref fh=
      Descriptors.fref
        {class=FieldHandle.class fh,
         name=FieldHandle.name fh,
         desc=FieldHandle.field_type fh
         }

   fun fmut fh=if FieldHandle.is_mutable fh then MUTABLE else IMMUTABLE

   val old_getfield=getfield
   val old_putfield=putfield
   val old_getstatic=getstatic
   val old_putstatic=putstatic

   fun getfield(fh,c)=old_getfield(make_fref fh,(fmut fh,c))
   fun putfield(fh,c)=old_putfield(make_fref fh,(fmut fh,c))
   fun getstatic(fh,c)=old_getstatic(make_fref fh,(fmut fh,c))
   fun putstatic(fh,c)=old_putstatic(make_fref fh,(fmut fh,c))

   fun make_mref mh=
      Descriptors.mref
        {class=MethodHandle.class mh,
         name=MethodHandle.name mh,
         desc=Descriptors.M(MethodHandle.output_type mh,MethodHandle.input_types mh)
         }

   fun make_iref mh=
      Descriptors.iref
        {class=MethodHandle.class mh,
         name=MethodHandle.name mh,
         desc=Descriptors.M(MethodHandle.output_type mh,MethodHandle.input_types mh)
         }

   val old_invoke_interface=invoke_interface
   val old_invoke_special=invoke_special
   val old_invoke_static=invoke_static
   val old_invoke_virtual=invoke_virtual
   val old_new=new (* haha *)

   fun invoke_interface mh=old_invoke_interface(make_iref mh)
   fun invoke_special mh=old_invoke_special(make_mref mh)
   fun invoke_static mh=old_invoke_static(make_mref mh)
   fun invoke_virtual mh=old_invoke_virtual(make_mref mh)
   fun new mh=old_new(make_mref mh)

   val old_monitorexit=monitorexit
   val old_monitorenter=monitorenter

   val monitorexit=old_monitorexit MUSTDO
   val monitorenter=old_monitorenter MUSTDO 

   val old_make_block=make_block
   val old_make_handler_block=make_handler_block

   fun make_block(bd,vl,il,ex,el)=old_make_block(convert_bd bd,vl,il,ex,el)
   fun make_handler_block(bd,vl,il,ex,el,hd)=
      old_make_handler_block(convert_bd bd,vl,il,ex,el,hd)
end

   