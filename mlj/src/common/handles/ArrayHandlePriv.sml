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

structure ArrayHandlePriv:>ARRAYHANDLEPRIV=
struct
   datatype Handle=H of {is_mutable:bool}
   structure A=
   struct
      type Handle=Handle
      val unknown= H{is_mutable=true}
      fun is_mutable(H{is_mutable,...})= is_mutable
      local
         open Types
         fun maybe_interface(c:ClassHandle.Handle)=
         (case ClassHandle.is_interface c of
            SOME false => false
         |  _ => true
         )
      in
         fun maybe_same{hand1=H{is_mutable=m1,...},type1=F(n1,bt1),
                        hand2=H{is_mutable=m2,...},type2=F(n2,bt2)}=
            (m1=m2) andalso
         (  (n1<n2 andalso
               (case bt1 of
                  CLASS c1 =>
                     ClassHandle.equal(c1,ClassHandle.object) orelse
                     ClassHandle.equal(c1,ClassHandle.cloneable)
               |  _ => false
               ))
            orelse
            (n1>n2 andalso
               (case bt2 of
                  CLASS c2 =>
                     ClassHandle.equal(c2,ClassHandle.object) orelse
                     ClassHandle.equal(c2,ClassHandle.cloneable)
               |  _ => false
               ))
            orelse
            (n1=n2 andalso
               (case (bt1,bt2) of
                  (CLASS c1,CLASS c2) =>
                     maybe_interface c1 orelse
                     maybe_interface c2 orelse
                     ClassHandle.maybe_subclass(c1,c2) orelse
                     ClassHandle.maybe_subclass(c2,c1)
               |  _ => Types.base_type_equal(bt1,bt2)
               ))
            )
      end

      fun array_handle_toString(H{is_mutable})=if is_mutable then "M" else ""
   end
end




