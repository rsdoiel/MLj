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
(*======================================================================*)
structure TyName :> TYNAME = 
struct

(*----------------------------------------------------------------------*)
(* A type name consists of:						*)
(*   1. The entity in which the tyname was generated.                   *)
(*      NONE if this type name is an external Java class.		*)
(*   2. An integer identifier unique to this tyname within that entity. *)
(*      zero for external Java classes.					*)
(*   3. The sort of the tyname.                                         *)
(*   4. A longid used for pretty-printing.                              *)
(*      The actual name for Java classes.				*)
(*   5. The index within its scc (if recursive).                        *)
(*----------------------------------------------------------------------*)
datatype TyName = 
  TyName of 
  {
    entity : Entity.Ref option,
    tag    : int,
    sort   : TySort.Sort,
    longid : Syntax.longid,
    index  : int option
  }

type Supply = Entity.Ref * int

(*----------------------------------------------------------------------*)
(* ORD structure							*)
(*----------------------------------------------------------------------*)
structure Ord =
  struct
    type ord_key = TyName
    fun compare 
      (TyName {entity = e1, tag = i1, longid = id1, index = j1, ...}, 
       TyName {entity = e2, tag = i2, longid = id2, index = j2, ...}) =
      case (e1,e2) of
        (NONE, NONE) =>
        Compare.list Symbol.OrdKey.compare (id1, id2)

      | (NONE, SOME _) =>
        LESS

      | (SOME _, NONE) =>
        GREATER

      | (SOME e1, SOME e2) =>
        (case Entity.Set.Key.compare(e1, e2) of
          EQUAL => 
          (case Int.compare(i1, i2) of
            EQUAL =>
            (case (j1, j2) of
              (NONE, NONE) => EQUAL
            | (SOME j1, SOME j2) => Int.compare(j1, j2)
            | (NONE, SOME _) => LESS
            | (SOME _,NONE) => GREATER)
          | other => other)
       
        | other => other)
  end

(*----------------------------------------------------------------------*)
(* Sets of type names and finite maps from type names			*)
(*----------------------------------------------------------------------*)
structure Set = SetFn(Ord)
structure Map = MapFn(Ord)

type Renaming = TyName Map.map

fun rename r tyname =
case Map.find(r, tyname) of
  NONE => tyname
| SOME tyname' => tyname'

fun longid ss = map Ids.symbol ss

(*----------------------------------------------------------------------*)
(* Create a tyname associated with an external Java class		*)
(*----------------------------------------------------------------------*)
local
  val eqclasses = 
    map longid [["java", "lang", "String"], ["java", "math", "BigInteger"]]
in
  fun externalClass longid = 
    TyName 
    { 
      entity = NONE,
      index = NONE, 
      sort = 
      {
        eq = List.exists 
           (fn longid' => Eq.list Symbol.equal (longid,longid')) eqclasses,
        class = true, 
        constrained = false 
      }, 
      longid = longid, 
      tag = 0 
    }
end

(*----------------------------------------------------------------------*)
(* Primitive type names							*)
(*----------------------------------------------------------------------*)
fun strid s = (Entity.Str, Ids.symbol s)
val primsigid = SOME (Entity.Sig, Ids.symbol "PRIM")
val datstrid = SOME (Entity.Str, Ids.symbol "Datatypes")

(*----------------------------------------------------------------------*)
(* Base types identified with external Java classes			*)
(*----------------------------------------------------------------------*)
val exnTyName     = 
  externalClass (longid ["java", "lang", "Exception"])

val stringTyName  = 
  externalClass (longid ["java", "lang", "String"])

val intinfTyName   = 
  externalClass (longid ["java", "math", "BigInteger"])

(*----------------------------------------------------------------------*)
(* Datatypes defined in the structure "Datatypes"			*)
(*----------------------------------------------------------------------*)
val boolTyName 	  = 
  TyName { entity = datstrid, tag = 1, sort = TySort.eqSort, 
           longid = longid ["bool"], index = NONE }

val listTyName    = 
  TyName { entity = datstrid, tag = 2, sort = TySort.eqSort, 
           longid = longid ["list"], index = SOME 0 }

val optionTyName  = 
  TyName { entity = datstrid, tag = 3, sort = TySort.eqSort, 
           longid = longid ["option"], index = NONE }

val orderTyName   = 
  TyName { entity = datstrid, tag = 4, sort = TySort.eqSort, 
           longid = longid ["order"], index = NONE }

(*----------------------------------------------------------------------*)
(* Opaque types defined in the signature "PRIM"				*)
(*----------------------------------------------------------------------*)
val vectorTyName  = 
  TyName { entity = primsigid, tag = 1, sort = TySort.eqSort,
           longid = longid ["vector"], index = NONE }

val int8TyName   = 
  TyName { entity = primsigid, tag = 2, sort = TySort.eqSort,
           longid = longid ["Int8", "int"], index = NONE }

val charTyName    = 
  TyName { entity = primsigid, tag = 3, sort = TySort.eqSort, 
           longid = longid ["char"], index = NONE }

val realTyName    = 
  TyName { entity = primsigid, tag = 4, sort = TySort.anySort, 
           longid = longid ["real"], index = NONE }

val real32TyName  = 
  TyName { entity = primsigid, tag = 5, sort = TySort.anySort, 
           longid = longid ["Real32", "real"], index = NONE }

val intTyName     = 
  TyName { entity = primsigid, tag = 6, sort = TySort.eqSort, 
           longid = longid ["int"], index = NONE }

val int64TyName   = 
  TyName { entity = primsigid, tag = 7, sort = TySort.eqSort,
           longid = longid ["Int64", "int"], index = NONE }

val int16TyName   = 
  TyName { entity = primsigid, tag = 8, sort = TySort.eqSort,
           longid = longid ["Int16", "int"], index = NONE }

val wordTyName    = 
  TyName { entity = primsigid, tag = 9, sort = TySort.eqSort, 
           longid = longid ["word"], index = NONE }

val word8TyName    = 
  TyName { entity = primsigid, tag = 10, sort = TySort.eqSort, 
           longid = longid ["Word8", "word"], index = NONE }

val word64TyName    = 
  TyName { entity = primsigid, tag = 11, sort = TySort.eqSort, 
           longid = longid ["Word64", "word"], index = NONE }

(*----------------------------------------------------------------------*)
(* Others								*)
(*----------------------------------------------------------------------*)
val substringTyName  = 
  TyName { entity = SOME (strid "Substring"), tag = 5, sort = TySort.eqSort,  
           longid = longid ["substring"], index = NONE }


(*----------------------------------------------------------------------*)
(* Java type names, identified with ML base types			*)
(*----------------------------------------------------------------------*)
val javaBoolean = boolTyName
val javaByte = int8TyName
val javaChar = charTyName
val javaDouble = realTyName
val javaFloat = real32TyName
val javaInt = intTyName
val javaLong = int64TyName
val javaShort = int16TyName

(*----------------------------------------------------------------------*)
(* Equality on type names						*)
(*----------------------------------------------------------------------*)
fun eq (tyname1,tyname2) = Ord.compare(tyname1, tyname2) = EQUAL

(*----------------------------------------------------------------------*)
(* Type name supply functions						*)
(*----------------------------------------------------------------------*)
fun initial entity = (entity,1)
fun fresh (longid, sort) (entity,i) = 
  (TyName { entity = SOME entity, tag = i, sort = sort, longid = longid,
            index = NONE }, (entity,i+1))

fun freshen longid' (TyName { entity, tag, sort, index, longid },
            (entity',tag')) =
  (TyName {entity = SOME entity', tag = tag', sort = sort, index = index,
           longid = longid' }, (entity',tag'+1))

fun freshRec (longids, sort) (entity,i) = 
  (Gen.mapi (fn (j,longid) => 
    TyName { entity = SOME entity, tag = i, index = SOME j, longid = longid,
             sort = sort }) longids, (entity,i+1))

fun newSort (TyName { entity,tag,sort,longid,index }) sort' =
  TyName { entity = entity, tag = tag, sort = sort',longid = longid,
           index = index }

fun temp (longids, sort) = Gen.mapi (fn (i, longid) =>
  TyName { entity = SOME (strid "$temp"), 
           tag = ~i, sort = sort, longid = longid, 
           index = NONE }) longids

fun entity (TyName { entity, ... }) = entity
fun longid (TyName { longid, ... }) = longid
fun sort (TyName { sort, ... }) = sort
fun fromExternalClass (TyName { entity, longid, ... }) = 
  if isSome entity then NONE
  else SOME longid

fun combine (x,y) = x + y * 0w509
    
fun hash 
  (TyName { entity, index, tag, ... }) =
  case entity of
    NONE => 0w0  (* Shouldn't happen *)
  | SOME e =>
    case index of
      NONE =>
      combine (Word.fromInt tag + 0w1, Symbol.HashKey.hashVal (#2 e))

    | SOME i =>
      combine (combine (Word.fromInt tag + 0w1, Symbol.HashKey.hashVal (#2 e)),
        Word.fromInt i)

(*----------------------------------------------------------------------*)
(* Was a tyname generated earlier than the point specified?     	*)
(*----------------------------------------------------------------------*)
fun earlier 
  (TyName { entity, tag, ...}, (entity', tag')) = 
  not (Eq.option EntityOps.eq (entity, SOME entity')) orelse tag<tag'

(*----------------------------------------------------------------------*)
(* Display a type name							*)
(*----------------------------------------------------------------------*)
fun toString (tyname as TyName { entity,tag,longid,... }) = 
  (if eq(tyname, boolTyName) then "bool"
   else if eq(tyname, exnTyName) then "exn"
   else if eq(tyname, listTyName) then "list"
   else if eq(tyname, optionTyName) then "option"
   else if eq(tyname, orderTyName) then "order"
   else if eq(tyname, stringTyName) then "string"
   else Pretty.longidToString longid) 
  ^
  (if isSome entity andalso 
     (Controls.isOn "showStamps" orelse null longid) then "(" ^ 
     EntityOps.toString (valOf entity) ^ "#" ^ Int.toString tag ^ ")" else "")

fun default tynames =
  if Set.member(tynames, intTyName) then intTyName
  else
  if Set.member(tynames, wordTyName) then wordTyName
  else
  if Set.member(tynames, realTyName) then realTyName
  else
  if Set.member(tynames, stringTyName) then stringTyName
  else raise Fail "TyName.default: no default type"


end
