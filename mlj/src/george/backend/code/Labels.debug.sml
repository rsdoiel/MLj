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

(* Labels:LABELS implements basic labels in the back end.  Labels are used for storing
   code positions to jump to *)
structure Labels:>LABELS=
struct
   datatype label=
      foo of int option ref
   |  bah of string*int option ref (* Used for debugging *)

   fun new_label {}=foo(ref NONE)
   fun new_named_label s=bah(s,ref NONE)

   fun index lab=
   (case lab of
      foo l =>
      (case !l of
         SOME i=>i
      |  NONE  =>raise Fail("Unresolved Label!")
      )
   |  bah (s,l) =>
      (case !l of
         SOME i=>i
      |  NONE  =>raise Fail("Unresolved Label: "^s)
      )
   )

   fun set_label(lab,i)=
   (case lab of
      foo l =>
      (case !l of
         SOME i=>raise Fail("Label multiply defined!")
      |  NONE  =>l:=SOME i
      )
   |  bah (s,l) =>
      (case !l of
         SOME i=>raise Fail("Label multiply defined: "^s)
      |  NONE  =>l:=SOME i
      )
   )

   fun unset_label lab=
   (case lab of
      foo l =>
      (case !l of
         SOME i=>l:=NONE
      |  NONE  =>raise Fail("Label unset but never defined??")
      )
   |  bah (s,l) =>
      (case !l of
         SOME i=>l:=NONE
      |  NONE  =>raise Fail("Label unset but never defined?? "^s)
      )
   )

   fun equal(l1,l2)=(index(l1)=index(l2))
end
