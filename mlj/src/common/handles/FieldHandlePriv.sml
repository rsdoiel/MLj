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

structure FieldHandlePriv:>FIELDHANDLEPRIV=
struct
   datatype Handle=H of
      {class:ClassHandle.Handle,
       name:JavaString.t,
       field_type:Types.java_type,
       is_mutable:bool
       }
   structure F:FIELDHANDLE=
   struct
      type Handle=Handle
      fun class(H h)= #class h
      fun name(H h)= #name h
      fun field_type(H h)= #field_type h
      fun is_mutable(H h)= #is_mutable h

      fun unknown {class,name,field_type}=
         H{class=class,name=name,field_type=field_type,is_mutable=true}
   
      (* The code for maybe_same_static and maybe_same_virtual is 
         modelled on the equivalent code in an early version of
         MakeDag.sml.  NB - we assume that fields with different
         mutability are different *)
      fun maybe_same_static(
        H{class=c1,name=n1,field_type=t1,is_mutable=m1},
        H{class=c2,name=n2,field_type=t2,is_mutable=m2})=
(* If both fields are static we check whether the mutability,
   classes, names and field types are the same (it's not clear 
   whether we really need to check the types, since surely one
   can't have two fields with the same name in the same class).  *)
            (m1=m2) andalso
            ClassHandle.equal(c1,c2) andalso
            JavaString.equal(n1,n2) andalso
            Types.java_type_equal(t1,t2)
      fun maybe_same_virtual( 
        H{class=c1,name=n1,field_type=t1,is_mutable=m1},
        H{class=c2,name=n2,field_type=t2,is_mutable=m2})=
            (* The mutability, names and types of the field are the same and
               c1 may subclass c2 or vice-versa *)
            (m1=m2) andalso
            JavaString.equal(n1,n2) andalso
            Types.java_type_equal(t1,t2) andalso
            (ClassHandle.maybe_subclass(c1,c2) orelse
             ClassHandle.maybe_subclass(c2,c1))

      fun field_handle_toString(H{class,name,field_type,is_mutable})=
      let
         val cstring=ClassHandle.class_handle_toString class
         val nstring=JavaString.toMLString name
         val tstring=Types.java_type_toString field_type
         val mstring=
            if is_mutable then "M" else ""
      in
         cstring^"."^nstring^":"^tstring^"/"^mstring
      end
   end
end        
  




