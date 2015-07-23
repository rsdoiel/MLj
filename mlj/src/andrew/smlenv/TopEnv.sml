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
(* Top-level environment, hard-wired to get overloading and prevent	*)
(* cycles in source.                                                    *)
(*======================================================================*)
structure TopEnv :> TOPENV =
struct

local 
  open SMLTy Env SMLPrimTy
in

fun tyVarOf sort = #1 (TyVar.fresh sort TyVar.initial)
val any = tyVarOf (TyVar.Normal TySort.anySort)
val anyty = tyVarType any

fun class ss = baseType (TyName.externalClass (map Ids.symbol ss))

val objectclass = class ["java","lang","Object"]
val exceptionclass = class ["java","lang","Exception"]
val cloneableclass = class ["java", "lang", "Cloneable"]

(*----------------------------------------------------------------------*)
(* The initial type constructor environment				*)
(*----------------------------------------------------------------------*)
val initialTE = 
  List.foldr (fn ((v, tystr), TE) => 
  Symbol.OrdMap.insert(TE, Ids.symbol v, tystr)) 
  Symbol.OrdMap.empty
[
  ("array",     TyStr.makeConcrete ([any], arrayType anyty)),
  ("bool",      TyStr.makeAbstract ([], TyName.boolTyName)),
  ("exn",       TyStr.makeAbstract ([], TyName.exnTyName)),
  ("char",      TyStr.makeAbstract ([], TyName.charTyName)),
  ("int",       TyStr.makeAbstract ([], TyName.intTyName)),
  ("list",      TyStr.makeAbstract ([any], TyName.listTyName)),
  ("option",    TyStr.makeAbstract ([any], TyName.optionTyName)),
  ("order",     TyStr.makeAbstract ([], TyName.orderTyName)),
  ("real",      TyStr.makeAbstract ([], TyName.realTyName)),
  ("ref",       TyStr.makeConcrete ([any], refType anyty)),
  ("string",    TyStr.makeAbstract ([], TyName.stringTyName)),
  ("substring", TyStr.makeAbstract ([], TyName.substringTyName)),
  ("unit",      TyStr.makeConcrete ([], unitType)),
  ("vector",    TyStr.makeConcrete ([any], vectorType anyty)),
  ("word",      TyStr.makeAbstract ([], TyName.wordTyName))
]

val initialSE = 
  foldl 
  (fn ((strid, id, tyname), SE) =>
  Symbol.OrdMap.insert(SE, Ids.symbol strid,
    EnvOps.TEinE(Symbol.OrdMap.insert(Symbol.OrdMap.empty, 
      Ids.symbol id, TyStr.makeAbstract([], tyname)))))
  Symbol.OrdMap.empty

  [("Char", "char", TyName.charTyName),
   ("Real32", "real", TyName.real32TyName),
   ("Real", "real", TyName.realTyName),
   ("String", "string", TyName.stringTyName),
   ("Int", "int", TyName.intTyName),
   ("Int8", "int", TyName.int8TyName),
   ("Int16", "int", TyName.int16TyName),
   ("Int32", "int", TyName.intTyName),
   ("Int64", "int", TyName.int64TyName),
   ("FixedInt", "int", TyName.int64TyName),
   ("LargeInt", "int", TyName.intinfTyName),
   ("Word8", "word", TyName.word8TyName),
   ("Word", "word", TyName.wordTyName),
   ("Word64", "word", TyName.word64TyName),
   ("LargeWord", "word", TyName.word64TyName)]

val initialE = 
  EnvOps.EplusSE (EnvOps.TEinE initialTE) initialSE

val initialB = EnvOps.EinB initialE

val boolCE = (false, ([], TyName.boolTyName, 
  Symbol.OrdMap.insert(Symbol.OrdMap.insert(
  Symbol.OrdMap.empty, Ids.falseSym, NONE), 
  Ids.trueSym, NONE)) : SMLTy.DatDef)

val listCE = (true, ([any], TyName.listTyName, Symbol.OrdMap.insert(
  Symbol.OrdMap.insert(
  Symbol.OrdMap.empty, Ids.nilSym, NONE), Ids.consSym, 
    SOME (tupleType [anyty, listType anyty])))
  : SMLTy.DatDef)

end (* of local open *)

end (* of struct *)

