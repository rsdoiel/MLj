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

(* DArray:>DARRAY implements a lightweight dynamic array of integers.
   The default element is 0.
   *)
signature DARRAY=
sig
   type array
   val array:int->array (* creates a new array with the suggested size. *)
   val sub:array*int->int 
   val update:array*int*int->unit
   val clear:array -> unit
   val bound:array -> int
end

structure DynIntArray:>DARRAY=
struct
   type array=int array ref
  
   val default= 0

   fun array n = ref(Array.array(n,default))

   fun clear a = a := Array.array(Array.length (!a),default)

   fun sub(ref arr,i)=Array.sub(arr,i) handle Subscript => 
      if i<0
      then
         raise Subscript
      else
         default

   fun update(aref as ref arr,i,value) =
      (Array.update(arr,i,value)) handle Subscript =>
        if i<0 then raise Subscript
        else
        let
          val oldlen=Array.length arr
          val newlen=Int.max(i+1,2*oldlen)
          val newarr=Array.array(newlen,default)
        in
          Array.copy{di=0,dst=newarr,len=NONE,si=0,src=arr};
          aref:=newarr;
          Array.update(newarr,i,value)
        end

  fun bound a = Array.length (!a) - 1
end

