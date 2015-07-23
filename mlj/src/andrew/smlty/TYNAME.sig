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

(*======================================================================*)
(* ML type names (Section 4.1, p16 Defn)				*)
(* The *stamp* part of a type name consists of:                         *)
(*    - the separately-compiled entity from which it is generated,      *)
(*    - an integer unique within the tynames generated from that entity *)
(* Type names also have a *sort*, and a *descriptor* which is used only *)
(* for pretty-printing.                                                 *)
(*======================================================================*)
signature TYNAME = 
sig

(*----------------------------------------------------------------------*)
(* Type names (Section 4.1, p16 Defn)					*)
(* have a sort, a defining structure identifier, a tycon identifier     *)
(* and an integer tag unique to the structure.                          *)
(*----------------------------------------------------------------------*)
type TyName and Supply

(*----------------------------------------------------------------------*)
(* Sets of type names (Section 4.2, p17 Defn)				*)
(* and finite maps whose domains are type names.                        *)
(*----------------------------------------------------------------------*)
structure Set : ORD_SET where type Key.ord_key = TyName 
structure Map : ORD_MAP where type Key.ord_key = TyName

(*----------------------------------------------------------------------*)
(* Renaming of type names         					*)
(*----------------------------------------------------------------------*)
type Renaming = TyName Map.map
val rename        : Renaming -> TyName -> TyName

(*----------------------------------------------------------------------*)
(* Equivalence of type names: only the stamps are compared.             *)
(*----------------------------------------------------------------------*)
val eq            : TyName * TyName -> bool

(*----------------------------------------------------------------------*)
(* Return the sort attribute of a type name				*)
(* Note that this is the best way of discovering that it's a class.     *)
(*----------------------------------------------------------------------*)
val sort          : TyName -> TySort.Sort

(*----------------------------------------------------------------------*)
(* Hash the tyname							*)
(*----------------------------------------------------------------------*)
val hash          : TyName -> word

(*----------------------------------------------------------------------*)
(* Generation of new names in a functional style			*)
(*----------------------------------------------------------------------*)
val initial       : Entity.Ref -> Supply
val fresh         : Syntax.longid * TySort.Sort -> Supply -> TyName * Supply
val freshRec      : Syntax.longid list * TySort.Sort -> Supply -> 
                    TyName list * Supply
val freshen       : Syntax.longid -> TyName * Supply -> TyName * Supply
val temp          : Syntax.longid list * TySort.Sort -> TyName list
val newSort       : TyName -> TySort.Sort -> TyName

(*----------------------------------------------------------------------*)
(* Convert a type name into a string for printing, qualifying with 	*)
(* the stamp if "showStamps" is set.                                    *)
(*----------------------------------------------------------------------*)
val toString	  : TyName -> string

(*----------------------------------------------------------------------*)
(* Was a tyname generated earlier than the point specified?     	*)
(*----------------------------------------------------------------------*)
val earlier       : TyName * Supply -> bool

val longid        : TyName -> Syntax.longid

(*----------------------------------------------------------------------*)
(* Primitive type names							*)
(*----------------------------------------------------------------------*)
val boolTyName 	  : TyName
val charTyName    : TyName
val exnTyName     : TyName
val intTyName     : TyName
val int8TyName    : TyName
val int16TyName   : TyName
val int64TyName   : TyName
val intinfTyName  : TyName
val listTyName    : TyName
val optionTyName  : TyName
val orderTyName   : TyName
val realTyName    : TyName
val real32TyName  : TyName
val stringTyName  : TyName
val substringTyName: TyName
val vectorTyName  : TyName
val wordTyName    : TyName
val word8TyName   : TyName
val word64TyName  : TyName

(*----------------------------------------------------------------------*)
(* Java type names (from Java signature)				*)
(*----------------------------------------------------------------------*)
val javaBoolean   : TyName
val javaByte      : TyName
val javaChar      : TyName
val javaDouble    : TyName
val javaFloat     : TyName
val javaInt       : TyName
val javaLong      : TyName
val javaShort     : TyName

(*----------------------------------------------------------------------*)
(* External Java class							*)
(*----------------------------------------------------------------------*)
val externalClass     : Syntax.longid -> TyName
val fromExternalClass : TyName -> Syntax.longid option

val default       : Set.set -> TyName

end
