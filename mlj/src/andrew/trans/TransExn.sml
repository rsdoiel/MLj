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
(* Translate exception bindings  					*)
(*======================================================================*)
structure TransExn :> TRANSEXN =
struct

val symbol = Symbol.symbol o JavaString.fromString

val matchExn = 
  MILExn.exn ((Entity.Str, symbol "General"), 2, [symbol "Match"])

val bindExn = 
  MILExn.exn ((Entity.Str, symbol "General"), 1, [symbol "Bind"])

fun transEE entity TNE EE = 
let
  fun transDef (stamp, (exn,longid), EE) =
    case exn of
      SMLTy.MLExn (tyopt, isdynamic) =>
      let
        val extra = if isdynamic then [MILTy.java Types.INT] else []
      in
        case tyopt of
          NONE => 
          SMLTy.ExMap.insert(EE, (entity,stamp), 
            MILTy.exn (MILExn.exn (entity, stamp, longid), extra))

        | SOME ty =>
          let 
            val milty = TransType.transType TyVar.Map.empty TNE ty
          in
            SMLTy.ExMap.insert(EE, (entity,stamp), 
              MILTy.exn (MILExn.exn (entity, stamp, longid), milty :: extra))
          end
      end

      | SMLTy.JavaExn ty =>
        SMLTy.ExMap.insert(EE, (entity,stamp),  
          TransType.transType TyVar.Map.empty TNE ty)
in
  IMap.foldli transDef SMLTy.ExMap.empty EE
end


end
