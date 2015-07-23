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
signature LABELS=
sig
   type label
   val new_label:unit->label (* create a label *)
   val new_named_label:string->label
(* create a named label.  When we are debugging, the name is stored and can be output in error messages *)
   val set_label:label*int->unit
(* Sets the label (Fail is raised if it is already set) *)
   val index:label->int
(* Returns the label (Fail is raised if it is unset *)
   val unset_label:label->unit
(* Make label unset again (Fail is raised if it is already unset) *)
   val equal:label*label->bool
(* equal(l1,l2) is equivalent to (index(l1)=index(l2)) *)
   val l2:label->Word8Vector.vector
   val l4:label->Word8Vector.vector
(* l2 and l4 return the absolute address (IE not as an offset) of the label
   in 2 or 4 bytes in Java format. *)
   val o2:label*int->Word8Vector.vector
   val o4:label*int->Word8Vector.vector
(* o2 and o4 find the offset to a label from the given integer argument 
   [EG the label is where we are jumping to and the integer is the
   position we are jumping from] and output them in 2 or 4 bytes in
   Java format. *)
end
