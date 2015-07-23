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

structure Field:>FIELD=
struct
   local
      type hndl=unit->int
      open Numbers
   in
      datatype flag=PUBLIC|PRIVATE|PROTECTED|STATIC|FINAL|VOLATILE|TRANSIENT

      fun flag_bit PUBLIC      =0wx1
      |   flag_bit PRIVATE     =0wx2
      |   flag_bit PROTECTED   =0wx4
      |   flag_bit STATIC      =0wx8
      |   flag_bit FINAL       =0wx10
      |   flag_bit VOLATILE    =0wx40
      |   flag_bit TRANSIENT   =0wx80

      val flags=[PUBLIC,PRIVATE,PROTECTED,STATIC,FINAL,VOLATILE,TRANSIENT]
   

      fun flag_word f=List.foldl Word.orb 0wx0 (map flag_bit f)
      (* The flags for a field are given as a list of flags *)

      fun decode_flags w=
         List.filter 
            (fn flag => Word.andb(flag_bit flag,w)<>0w0)
            flags

      datatype field_data=simple of {
         name:JavaString.t, (* name of the field, in unqualified form *)
         field_type:Descriptors.field_descriptor,
         flags:flag list,
         attributes:Attribute.attributes
         }

      datatype t2=T of {
         name:hndl,
         field_type:hndl,  (* indices of name and descriptor, respectively *)
         fword:Word.word,
         attributes:Attribute.t2
            (* index of constant value *)
         }

      fun pool_pass(A,simple {name,field_type,flags,attributes})=
         T {
           name=AllPools.r_Utf8(A,name),
           field_type=AllPools.r_Utf8(A,Descriptors.fdout(field_type)),
           fword=flag_word flags,
           attributes=Attribute.pool_pass(A,attributes)
           }
        
      fun bytecode_pass(T{name,field_type,fword,attributes})=
         W8.concat [
            W8.fromvList [
               u2(Word.toInt(fword)), (* flags *)
               h2(name),
               h2(field_type)
               ],
            Attribute.bytecode_pass attributes
            ]
   
      fun decode_field(pool,is)=
      let
         val flag_word=ReadInts.w2 is
         val flags=decode_flags flag_word
         val name=ReadPool.get_utf8(pool,ReadInts.u2 is)
         val field_type=ReadPool.get_fdesc(pool,ReadInts.u2 is)
         val attributes=Attribute.decode_attributes(pool,is)            
      in
         simple {
            flags=flags,
            name=name,
            field_type=field_type,
            attributes=attributes
            }
      end
   end (* local *)
end
