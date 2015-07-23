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


(* Descriptors:>DESCRIPTORS contains the datatype declarations for
   field and method descriptors, and the code for turning them into
   and out of JavaStrings.
   *)
structure Descriptors:>DESCRIPTORS=
struct
   open Types

   type field_descriptor=Types.java_type

   val field_descriptor_equal=Types.java_type_equal

   datatype method_descriptor=M of Types.java_void_type*
      (Types.java_type list)
      (* The list contains descriptors of the arguments; the optional
         argument is the return type *)

   type java_string=JavaString.t

   datatype field_ref=fref of {
      class:ClassHandle.Handle,
      name :java_string, (* name of the field *)
      desc :field_descriptor
                         (* type of the field *)
      }

   datatype method_ref=mref of {
      class:ClassHandle.Handle,
      name :java_string, (* name of the method *)
      desc :method_descriptor
                         (* type of the method *)
      }

   datatype interface_method_ref=iref of {
      class:ClassHandle.Handle,
      name :java_string, (* name of the interface method *)
      desc :method_descriptor
                         (* type of the interface method *)
      }

   (* is_init tests if a method ref refers to an init method *)
   local
      val init_string=JavaString.fromString "<init>"
   in
      fun is_init(mref{name,...})=
         JavaString.equal(init_string,name)
   end

   (* the XXX_toString functions are intended for debugging purposes only and
      should not be used in production code or in any other way relied on. *)
   val field_descriptor_toString=Types.java_type_toString

   fun method_descriptor_toString(M(vt,tl))=
      String.concat(
         Types.java_void_type_toString vt::
         "("::
            (case tl of
               [] => []
            |  hd::rest =>
               java_type_toString hd::
                  List.concat(
                     List.map
                        (fn jt=>[",",java_type_toString jt])
                        rest)
            )@
         [")"]
         )

   fun field_ref_toString(fref {class=class,name=name,desc=desc})=
      String.concat[
         JavaString.toMLString(ClassHandle.name class),
         ".",
         JavaString.toMLString name,
         field_descriptor_toString desc
         ]

   fun method_ref_toString(mref {class=class,name=name,desc=desc})=
      String.concat[
         JavaString.toMLString(ClassHandle.name class),
         ".",
         JavaString.toMLString name,
         method_descriptor_toString desc
         ]

   fun interface_method_ref_toString(iref r)=method_ref_toString(mref r)


   exception TooManyDimensions
      (* raised if there are more than 255 dimensions for an array, which
         is outlawed by the VM book in section 4.10 *)

  (* We now define functions for encoding descriptors in a file.
     *)

   local
      fun v1(s)=JavaString.fromString(s)
      fun bd(BYTE)   =v1("B")
      |   bd(CHAR)   =v1("C")
      |   bd(DOUBLE) =v1("D")
      |   bd(FLOAT)  =v1("F")
      |   bd(INT)    =v1("I")
      |   bd(LONG)   =v1("J")
      |   bd(SHORT)  =v1("S")
      |   bd(BOOLEAN)=v1("Z")
      |   bd(CLASS(class))=
	     JavaString.concat[
		      v1("L"),
		      ClassHandle.name(class),
		      v1(";")
		      ]
      fun arrd(i)=  (* Produces a JavaString containing i '[' chars *)
         if i>=0 andalso i<=255 then
            JavaString.fromString(
	       substring(
(* apologies for this hack, which I couldn't resist! *)
"[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[\
\[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[\
\[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[\
\[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[",
               0,i))
         else
            raise TooManyDimensions
   in
      fun fdout(F(i,b))=JavaString.concat [arrd(i),bd(b)]
      fun mdout(M(ret,args))=JavaString.concat (
         v1("(")::
         (map fdout args) @
         [
            v1(")"),
            (case ret of
                NONE   => v1("V")
             |  SOME f => fdout f
               )
            ]
         )
   end

   local
      open JavaString
      open Assert

      fun err ()=fail("Bad field or method descriptor")

      fun rdin(pos:JavaString.pos):(Types.java_void_type*JavaString.pos)=
      (* Read a "ReturnDescriptor"  (see the VM book page 91) from
         pos.  This is like a Field Descriptor (indeed, we use it for
         reading field descriptors) but may also include the Void type,
         for which it returns NONE.
         *)
      let
         fun rdin'(s,pos)= 
         (* like rdin, but we have already read s>=0 #"[" characters *)
         (case JavaString.read_char pos of
            NONE => err()
         |  SOME(ch,pos) =>
            let
               fun c bt=(SOME(F(s,bt)),pos) (* a common case *)
            in
               (case JavaChar.toAscii ch of
                  SOME #"B" => c BYTE
               |  SOME #"C" => c CHAR
               |  SOME #"D" => c DOUBLE
               |  SOME #"F" => c FLOAT
               |  SOME #"I" => c INT
               |  SOME #"J" => c LONG
               |  SOME #"L" => 
                  (case JavaString.read_to_ascii_char(pos,#";") of
                     NONE => err()
                  |  SOME (classname,pos) =>
                        (SOME(F(s,CLASS(ClassHandle.unknown classname))),
                         pos)
                  )
               |  SOME #"S" => c SHORT
               |  SOME #"Z" => c BOOLEAN
               |  SOME #"[" => rdin'(s+1,pos)
               |  SOME #"V" => if s=0 then (NONE,pos) else err()
               |  _ => err()
               )
            end
         )
      in
         rdin'(0,pos)
      end
   in
      fun fdin js=
      let
         val pos=read_begin js
         val (fdv,pos)=rdin pos
         val fd=
            (case fdv of
               NONE => err()
            |  SOME fd => fd
            )
      in 
         if read_atend pos
         then
            fd
         else
            err()
      end

      val lpar=JavaChar.fromAscii #"("

      fun mdin js=
      let
         val pos=read_begin js
         val pos=
            (case read_char pos of
               NONE => err()
            |  SOME(ch,pos) => 
                  if ch=lpar then pos else err()
            )

         fun getargs(sofar,pos)=
         if isit_ascii(pos,#")") 
         then
         let
            val SOME (_,pos)=read_char pos
         in
            (List.rev sofar,pos)
         end
         else 
         let 
            val (argv,pos)=rdin pos
            val arg=
               (case argv of
                  SOME arg => arg
               |  NONE => err()
               )
         in getargs(arg::sofar,pos)
         end   
         
         val (args,pos)=getargs([],pos)
         val (res_type,pos)=rdin pos
      in
         if read_atend pos
         then
            M(res_type,args)
         else
            err()
      end
   end
end





