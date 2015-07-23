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
(* Constant folding							*)
(*======================================================================*)
structure ConstOps :> CONSTOPS =
struct

local 
  open Constants 
in

(*----------------------------------------------------------------------*)
(* Apply a particular test from Tests.test to two Java constants.	*)
(* Return SOME ord if the result is known or NONE if it isn't.          *)
(*----------------------------------------------------------------------*)
fun applyTest (t, jcon1, jcon2) =
let
  val orderopt = 
  case (jcon1, jcon2) of
    (BOOLEAN b1, BOOLEAN b2) => JavaInt.numops.Compare(b1, b2)
  | (BYTE b1, BYTE b2) => JavaInt.numops.Compare(b1, b2)
  | (CHAR c1, CHAR c2) => JavaInt.numops.Compare(c1, c2)
  | (SHORT s1, SHORT s2) => JavaInt.numops.Compare(s1, s2)
  | (INT i1, INT i2) => JavaInt.numops.Compare(i1, i2)
  | (LONG l1, LONG l2) => JavaLong.numops.Compare(l1, l2)
  | (FLOAT f1, FLOAT f2) => NONE
  | (DOUBLE d1, DOUBLE d2) => NONE
  | (STRING s1, STRING s2) => NONE
  | (NULL, NULL) => SOME EQUAL
  | _ => 
    raise Fail "ConstOps.applyTest: type mismatch"
in
  if isSome orderopt 
  then SOME (Tests.test(t, orderopt))
  else NONE
end

(*----------------------------------------------------------------------*)
(* Return the compile-time function for the integer and long versions	*)
(* of the operation specified.                                          *)
(*----------------------------------------------------------------------*)
fun whichBinaryFun jop =
case jop of
  Java.Add => SOME (JavaInt.numops.add, JavaLong.numops.add)
| Java.Sub => SOME (JavaInt.numops.sub, JavaLong.numops.sub)
| Java.Mul => SOME (JavaInt.numops.mul, JavaLong.numops.mul)
| Java.Or  => SOME (JavaInt.numops.orb, JavaLong.numops.orb)
| Java.And => SOME (JavaInt.numops.andb,JavaLong.numops.andb)
| Java.Xor => SOME (JavaInt.numops.xorb,JavaLong.numops.xorb)
| _        => NONE

fun whichBinaryFun' jop =
case jop of
  Java.Div => SOME (JavaInt.numops.div, JavaLong.numops.div)
| Java.Rem => SOME (JavaInt.numops.rem, JavaLong.numops.rem)
| _        => NONE

fun intOf (BOOLEAN x) = SOME (x, BOOLEAN)
  | intOf (BYTE x) = SOME (x, BYTE)
  | intOf (CHAR x) = SOME (x, CHAR)
  | intOf (SHORT x) = SOME (x, SHORT)
  | intOf (INT x) = SOME (x, INT)
  | intOf _ = NONE

(*----------------------------------------------------------------------*)
(* Apply a Java operation to a list of constant arguments, if possible, *)
(* or return NONE if the answer isn't known.                		*)
(*----------------------------------------------------------------------*)
fun applyOp (jop, args, cty) =
(if Controls.isOn "showConstOps"
 then Debug.print ("\n" ^ JavaOps.opTypeToString jop ^ "(" ^
   Pretty.simpleVec "," Constants.constant_toString args ^ ")")
 else ();

case (jop, args) of
  (Java.Neg, [LONG x]) => SOME (LONG (JavaLong.numops.neg x))
| (Java.Neg, [INT x]) => SOME (INT (JavaInt.numops.neg x))
| (Java.Cast, [x]) =>
  let
    val SOME jty = MILTy.fromJava (hd (#2 (MILTy.fromCmp cty)))
  in
    Constants.convert (x, jty)
  end
| (Java.Shl, [INT x, INT y]) => SOME (INT (JavaInt.numops.shl(x,y)))
| (Java.Shr, [INT x, INT y]) => SOME (INT (JavaInt.numops.shr(x,y)))
| (Java.Ushr, [INT x, INT y]) => SOME (INT (JavaInt.numops.ushr(x,y)))
| (Java.Shl, [LONG x, INT y]) => SOME (LONG (JavaLong.numops.shl(x,y)))
| (Java.Shr, [LONG x, INT y]) => SOME (LONG (JavaLong.numops.shr(x,y)))
| (Java.Ushr, [LONG x, INT y]) => SOME (LONG (JavaLong.numops.ushr(x,y)))

| (jop, [LONG x, LONG y]) =>
  (case whichBinaryFun jop of
    NONE => 
    (case whichBinaryFun' jop of
      NONE => NONE
    | SOME (intop, longop) =>
      Option.map LONG (longop (x,y))
    )
  | SOME (intop, longop) => SOME (LONG (longop (x,y))))

| (jop, [x, y]) =>
  (case (intOf x, intOf y) of 
    (SOME (x, c), SOME (y, _)) =>
    (case whichBinaryFun jop of
      NONE => 
      (case whichBinaryFun' jop of
        NONE => NONE
      | SOME (intop, longop) =>
        Option.map c (intop (x,y))
      )
    | SOME (intop, longop) => SOME (c (intop (x,y))))
  | _ => NONE)

| _ => NONE)

end (* of local open *)

end (* of struct *)

