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
(* Translation of special constants  	                                *)
(*======================================================================*)
structure TransSCon =
struct

(*----------------------------------------------------------------------*)
(* Convert a typed constant into a Java constant, returning             *)
(* NONE if there is an overflow.					*)
(*----------------------------------------------------------------------*)
fun trans (scon, ty) =
  case scon of

  (* Strings go straight through *)
    SCon.StrCon s => 
    SOME (Constants.STRING s)

  (* For characters we look up their code. Beware Unicode/ASCII! *)
  | SCon.CharCon c => 
    SOME (Constants.CHAR c)

  (* Reals are not supported by this function *)
  | SCon.RealCon s =>
    (case Real.fromString s of
      NONE => 
      NONE

    | SOME r => 
      case MILTy.fromJava ty of
        SOME Types.FLOAT => SOME (Constants.FLOAT (JavaFloat.fromReal r))
      | SOME Types.DOUBLE => SOME (Constants.DOUBLE (JavaDouble.fromReal r))
    )


  (* For integers and words we check for overflow *)
  (* Notice that Java does not distinguish signed from unsigned *)
  | SCon.NumCon(base,kind,s) =>
    case (MILTy.fromJava ty,kind) of
      (SOME Types.BYTE, IntConvFlags.Signed _) =>
      (case JavaInt.fromString base kind s of
        NONE => NONE
      | SOME n => if JavaInt.isji1 n then SOME (Constants.BYTE n) else NONE)
 
    | (SOME Types.SHORT, IntConvFlags.Signed _) =>
      (case JavaInt.fromString base kind s of
        NONE => NONE
      | SOME n => if JavaInt.isji2 n then SOME (Constants.SHORT n) else NONE)
 
    | (SOME Types.INT, IntConvFlags.Signed _) =>
      Option.map Constants.INT (JavaInt.fromString base kind s)
    
    | (SOME Types.LONG, IntConvFlags.Signed _) =>
      Option.map Constants.LONG (JavaLong.fromString base kind s)
    
    | (SOME Types.BYTE, IntConvFlags.Unsigned) =>
      (case JavaInt.fromString base kind s of
        NONE => NONE
      | SOME n => if JavaInt.isju1 n then SOME (Constants.BYTE n) else NONE)
   
    | (SOME Types.INT, IntConvFlags.Unsigned) =>
      Option.map Constants.INT (JavaInt.fromString base kind s)
   
    | (SOME Types.LONG, IntConvFlags.Unsigned) =>
      Option.map Constants.LONG (JavaLong.fromString base kind s)    


end