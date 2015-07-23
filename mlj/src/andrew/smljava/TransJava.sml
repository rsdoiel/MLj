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
(* Translate Java types into SML types					*)
(*======================================================================*)
structure TransJava =
struct

(* Hacked up *)
fun classToML c =
let 
  val s = JavaString.toMLString (ClassHandle.name c)
  val ss = String.tokens (fn c => c = #"/") s
in
  SMLTy.consType([], TyName.externalClass (map Ids.symbol ss))
end

(*----------------------------------------------------------------------*)
(* Convert an external Java type into its ML equivalent, not forgetting	*)
(* to put option onto reference types.                                  *)
(*----------------------------------------------------------------------*)
fun javaToML (Types.F(0, base)) =
    (case base of
      Types.BOOLEAN => SMLTy.consType([], TyName.javaBoolean)
    | Types.BYTE    => SMLTy.consType([], TyName.javaByte)
    | Types.CHAR    => SMLTy.consType([], TyName.javaChar)
    | Types.DOUBLE  => SMLTy.consType([], TyName.javaDouble)
    | Types.FLOAT   => SMLTy.consType([], TyName.javaFloat)
    | Types.INT     => SMLTy.consType([], TyName.javaInt)
    | Types.LONG    => SMLTy.consType([], TyName.javaLong)
    | Types.SHORT   => SMLTy.consType([], TyName.javaShort)
    | Types.CLASS c => SMLPrimTy.optionType (classToML c))

  | javaToML (Types.F(n, base)) =
    SMLPrimTy.optionType(
    SMLTy.arrayType(javaToML (Types.F(n-1,base))))

(*----------------------------------------------------------------------*)
(* Convert the static members of a class into an SML structure		*)
(*----------------------------------------------------------------------*)
fun classToStruct 
(Class.middling {this, flags, super, interfaces, fields, methods, attributes}) =
Env.Env
(
  Symbol.OrdMap.empty,
  Symbol.OrdMap.empty,
  foldl 
    (fn (Field.simple { name, field_type, flags, attributes }, VE) =>

    (* Only public static fields are accessible through structures *)
    if List.exists (fn f => f=Field.STATIC) flags
    andalso List.exists (fn f => f=Field.PUBLIC) flags
    then
    let
      val smlty = javaToML field_type

      (* Non-final fields are given a ref type *)
      val smlty = 
        if List.exists (fn f => f=Field.FINAL) flags
        then smlty
        else SMLTy.refType smlty

      val sym = Symbol.symbol name
    in
      case Symbol.OrdMap.find(VE, sym) of
        NONE => Symbol.OrdMap.insert(VE, sym, ValBind.JavaTys(this, [smlty]))
      | SOME (ValBind.JavaTys (_, tys)) =>
        Symbol.OrdMap.insert (VE, sym, ValBind.JavaTys (this, smlty::tys))
    end
    else VE)

  (foldl 
    (fn (Method.simple { name, flags, method_type, attributes }, VE) =>

    (* Only public static methods are accessible through structures *)
    if List.exists (fn f => f=Method.STATIC) flags andalso 
       List.exists (fn f => f=Method.PUBLIC) flags
    then
    let
      val Descriptors.M (resty, argtys) = method_type

      (* Void args maps to unit; multiple args to tuples *)
      val smlargty = 
        case argtys of
          [] => SMLPrimTy.unitType
        | [ty] => javaToML ty
        | tys => SMLTy.tupleType (map javaToML tys)

      (* void result maps to unit *)
      val smlresty =
        case resty of
          NONE => SMLPrimTy.unitType
        | SOME ty => javaToML ty

      val smlty = SMLTy.funType (smlargty, smlresty)
      val sym = Symbol.symbol name
    in
      case Symbol.OrdMap.find(VE, sym) of
        NONE => Symbol.OrdMap.insert(VE, sym, ValBind.JavaTys(this, [smlty]))
      | SOME (ValBind.JavaTys (_, tys)) =>
        Symbol.OrdMap.insert(VE, sym, ValBind.JavaTys (this, smlty::tys))
    end
    else VE)
    Symbol.OrdMap.empty
    methods
  )

  fields
)

end
    
