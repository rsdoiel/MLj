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

(* The Method structure contains the datatype corresponding to Java methods.
   *)
structure Method:>METHOD=
struct
   local
      type hndl=unit->int
      open Numbers
   in
      datatype flag=PUBLIC|PRIVATE|PROTECTED|STATIC|FINAL|SYNCHRONIZED
                   |NATIVE|ABSTRACT

      datatype method_data=simple of {
         name:JavaString.t,
         flags:flag list,
         method_type:Descriptors.method_descriptor,
         attributes:Attribute.attributes
         }

      (* The flags for a method are given as a list of flags *)
      fun flag_bit(PUBLIC)      =0wx1
      |   flag_bit(PRIVATE)     =0wx2
      |   flag_bit(PROTECTED)   =0wx4
      |   flag_bit(STATIC)      =0wx8
      |   flag_bit(FINAL)       =0wx10
      |   flag_bit(SYNCHRONIZED)=0wx20
      |   flag_bit(NATIVE)      =0wx100
      |   flag_bit(ABSTRACT)    =0wx400

      val flags=[PUBLIC,PRIVATE,PROTECTED,STATIC,FINAL,SYNCHRONIZED,
         NATIVE,ABSTRACT]
      
      fun flag_word f=List.foldl Word.orb 0w0 (map flag_bit f)
     
      fun decode_flags w=
      List.filter 
         (fn flag => Word.andb(flag_bit flag,w)<>0w0)
         flags

     datatype t2=T of {
        name:hndl,
        method_type:hndl,
        fword:Word.word,
        attributes:Attribute.t2
        }

     fun pool_pass(A,simple {name,flags,method_type,attributes})=
        T { name=AllPools.r_Utf8(A,name),
            method_type=AllPools.r_Utf8(A,Descriptors.mdout method_type),
            fword=flag_word flags,
            attributes=Attribute.pool_pass(A,attributes)
            }

      fun bytecode_pass(T {name,method_type,fword,attributes})=
         W8.concat[
            W8.fromvList[
               u2(Word.toInt(fword)),
               h2(name),
               h2(method_type)
               ],
            Attribute.bytecode_pass attributes    
            ]

      fun decode_method(pool,is)=
      let
         val flags=decode_flags(ReadInts.w2 is)
         val name=ReadPool.get_utf8(pool,ReadInts.u2 is)
         val method_type=ReadPool.get_mdesc(pool,ReadInts.u2 is)
         val attributes=Attribute.decode_attributes(pool,is)
      in
         simple {
            flags=flags,
            name=name,
            method_type=method_type,
            attributes=attributes
            }
      end          
   end (* local *)
end


