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

structure MethodHandlePriv:>METHODHANDLEPRIV=
struct
   datatype Handle=H of 
      {class:ClassHandle.Handle,
       name:JavaString.t,
       input_types:Types.java_type list,
       output_type:Types.java_type option,
       reads:bool,
       writes:bool,
       writes_first:bool,
       allocs:bool,
       exceptional:bool,
       unbounded:bool,
       synchronize:bool
       }

   structure M:METHODHANDLE=
   struct
      type Handle=Handle
      fun class(H h)= #class h
      fun name(H h)= #name h
      fun input_types(H h)= #input_types h
      fun output_type(H h)= #output_type h
      fun reads(H h)= #reads h
      fun allocs(H h)= #allocs h
      fun writes(H h)= #writes h
      fun writes_first(H h)= #writes_first h
      fun exceptional(H h)= #exceptional h
      fun unbounded(H h)= #unbounded h
      fun synchronize(H h)= #synchronize h      

      local
         val init_string=JavaString.fromString "<init>"
      in
         fun is_init(H h)=JavaString.equal(#name h,init_string)
      end
      fun unknown {class,name,input_types,output_type}=
         H{class=class,name=name,input_types=input_types,
          output_type=output_type,reads=true,allocs=true,writes=true,
          writes_first=true,exceptional=true,unbounded=true,
          synchronize=false}

      fun method_handle_toString(H{class,name,input_types,output_type,
         reads,writes,writes_first,allocs,exceptional,unbounded,synchronize})=
      let
         val cstring=ClassHandle.class_handle_toString class
         val mstring=JavaString.toMLString name
         val instring=
            (case input_types of
               [] => ""
            |  hd::rest =>
                  Types.java_type_toString hd ^ 
                     String.concat(List.concat(
                        List.map
                           (fn t => [",",Types.java_type_toString t]) 
                           rest
                        ))
            )
         val outstring=Types.java_void_type_toString output_type
         val r_string=if reads then "R" else ""
         val w_string=if writes then "W" else ""
         val wf_string=if writes_first then "Wf" else ""
         val a_string=if allocs then "A" else ""
         val e_string=if exceptional then "E" else ""
         val u_string=if unbounded then "U" else ""
         val s_string=if synchronize then "S" else ""

      in
         String.concat[outstring," ",cstring,".",mstring,"(",
            instring,")/",r_string,w_string,wf_string,a_string,
            e_string,u_string,s_string]
      end
   end
end

      
      
     

