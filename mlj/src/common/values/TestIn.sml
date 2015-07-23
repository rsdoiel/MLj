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

CM.make' "backend/sources.debug.cm";
structure A=JavaLong.numops;
val f=valOf o (JavaLong.fromString IntConvFlags.Decimal IntConvFlags.Unsigned)
val mill=f "1000000";
val bill=A.mul(mill,mill);
val trill=A.mul(bill,mill);
val big=A.mul(trill,bill); (* overflows *)
use "/rnd/george/mlj/george/backend/pack/Test.sml";
structure T=Test(A);
val ts=T.toString;
ts mill;
ts bill;
ts trill;
ts (valOf(A.div(trill,trill)));
ts (valOf(A.div(big,big)));
