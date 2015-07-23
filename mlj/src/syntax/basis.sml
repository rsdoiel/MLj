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

val basis_files=[
"/rnd/working/mlj/basis/ARRAY.sig"
,"/rnd/working/mlj/basis/Array.sml"
,"/rnd/working/mlj/basis/BOOL.sig"
,"/rnd/working/mlj/basis/BYTE.sig"
,"/rnd/working/mlj/basis/Basis.sml"
,"/rnd/working/mlj/basis/BinIO.sml"
,"/rnd/working/mlj/basis/BinPrimIO.sml"
,"/rnd/working/mlj/basis/Bool.sml"
,"/rnd/working/mlj/basis/BoolArray.sml"
,"/rnd/working/mlj/basis/BoolVector.sml"
,"/rnd/working/mlj/basis/Byte.sml"
,"/rnd/working/mlj/basis/CHAR.sig"
,"/rnd/working/mlj/basis/COMMAND_LINE.sig"
,"/rnd/working/mlj/basis/Char.sml"
,"/rnd/working/mlj/basis/CharArray.sml"
,"/rnd/working/mlj/basis/CharString.sml"
,"/rnd/working/mlj/basis/CharVector.sml"
,"/rnd/working/mlj/basis/CommandLine.sml"
,"/rnd/working/mlj/basis/DATE.sig"
,"/rnd/working/mlj/basis/Datatypes.sml"
,"/rnd/working/mlj/basis/Date.sml"
,"/rnd/working/mlj/basis/FLOATFRAC.sig"
,"/rnd/working/mlj/basis/FixedInt.sml"
,"/rnd/working/mlj/basis/FixedWord.sml"
,"/rnd/working/mlj/basis/FloatFrac.sml"
,"/rnd/working/mlj/basis/GENERAL.sig"
,"/rnd/working/mlj/basis/General.sml"
,"/rnd/working/mlj/basis/IEEEReal.sml"
,"/rnd/working/mlj/basis/IEEE_REAL.sig"
,"/rnd/working/mlj/basis/IMPERATIVE_IO.sig"
,"/rnd/working/mlj/basis/INTEGER.sig"
,"/rnd/working/mlj/basis/INT_INF.sig"
,"/rnd/working/mlj/basis/IO.sig"
,"/rnd/working/mlj/basis/IO.sml"
,"/rnd/working/mlj/basis/Int.sml"
,"/rnd/working/mlj/basis/Int32.sml"
,"/rnd/working/mlj/basis/Int64.sml"
,"/rnd/working/mlj/basis/Int64Array.sml"
,"/rnd/working/mlj/basis/Int64Vector.sml"
,"/rnd/working/mlj/basis/IntArray.sml"
,"/rnd/working/mlj/basis/IntVector.sml"
,"/rnd/working/mlj/basis/Java.sml"
,"/rnd/working/mlj/basis/LIST.sig"
,"/rnd/working/mlj/basis/LIST_PAIR.sig"
,"/rnd/working/mlj/basis/LargeInt.sml"
,"/rnd/working/mlj/basis/LargeReal.sml"
,"/rnd/working/mlj/basis/LargeWord.sml"
,"/rnd/working/mlj/basis/List.sml"
,"/rnd/working/mlj/basis/ListPair.sml"
,"/rnd/working/mlj/basis/MATH.sig"
,"/rnd/working/mlj/basis/MLJBaseUtils.sml"
,"/rnd/working/mlj/basis/MLJINT_INF_UTILS.sig"
(*
,"/rnd/working/mlj/basis/MLJSubstring.sml"
*)
,"/rnd/working/mlj/basis/MLJUtils.sml"
,"/rnd/working/mlj/basis/MLJVector.sml"
,"/rnd/working/mlj/basis/MONO_ARRAY.sig"
,"/rnd/working/mlj/basis/MONO_VECTOR.sig"
,"/rnd/working/mlj/basis/Math.sml"
,"/rnd/working/mlj/basis/MyTest.sml"
,"/rnd/working/mlj/basis/OPTION.sig"
,"/rnd/working/mlj/basis/OS.sig"
(*
,"/rnd/working/mlj/basis/OS.sml"
*)
,"/rnd/working/mlj/basis/OS_FILE_SYS.sig"
,"/rnd/working/mlj/basis/OS_IO.sig"
,"/rnd/working/mlj/basis/OS_PATH.sig"
,"/rnd/working/mlj/basis/OS_PROCESS.sig"
,"/rnd/working/mlj/basis/Option.sml"
,"/rnd/working/mlj/basis/PACK_REAL.sig"
,"/rnd/working/mlj/basis/PACK_WORD.sig"
,"/rnd/working/mlj/basis/PackBig.sml"
,"/rnd/working/mlj/basis/PackRealBig.sml"
,"/rnd/working/mlj/basis/Position.sml"
,"/rnd/working/mlj/basis/REAL.sig"
,"/rnd/working/mlj/basis/Real.sml"
,"/rnd/working/mlj/basis/Real64.sml"
,"/rnd/working/mlj/basis/RealArray.sml"
,"/rnd/working/mlj/basis/RealVector.sml"
,"/rnd/working/mlj/basis/STREAM_IO.sig"
,"/rnd/working/mlj/basis/STRING.sig"
,"/rnd/working/mlj/basis/STRINGBUFFER.sig"
,"/rnd/working/mlj/basis/STRING_CVT.sig"
,"/rnd/working/mlj/basis/SUBSTRING.sig"
,"/rnd/working/mlj/basis/SinCos.sml"
,"/rnd/working/mlj/basis/String.sml"
,"/rnd/working/mlj/basis/StringBuffer.sml"
,"/rnd/working/mlj/basis/StringCvt.sml"
,"/rnd/working/mlj/basis/Substring.sml"
,"/rnd/working/mlj/basis/TEXT_IO.sig"
,"/rnd/working/mlj/basis/TEXT_STREAM_IO.sig"
,"/rnd/working/mlj/basis/TIME.sig"
,"/rnd/working/mlj/basis/TIMER.sig"
,"/rnd/working/mlj/basis/TestOverload.sml"
(*
,"/rnd/working/mlj/basis/TextIO.sml"
*)
,"/rnd/working/mlj/basis/Time.sml"
,"/rnd/working/mlj/basis/Timer.sml"
,"/rnd/working/mlj/basis/VECTOR.sig"
,"/rnd/working/mlj/basis/Vector.sml"
,"/rnd/working/mlj/basis/WORD.sig"
,"/rnd/working/mlj/basis/WideChar.sml"
,"/rnd/working/mlj/basis/WideCharVector.sml"
,"/rnd/working/mlj/basis/WideString.sml"
,"/rnd/working/mlj/basis/WideSubstring.sml"
,"/rnd/working/mlj/basis/Word.sml"
,"/rnd/working/mlj/basis/Word32.sml"
,"/rnd/working/mlj/basis/Word64.sml"
,"/rnd/working/mlj/basis/Word64Array.sml"
,"/rnd/working/mlj/basis/Word64Vector.sml"
,"/rnd/working/mlj/basis/Word8.sml"
,"/rnd/working/mlj/basis/Word8Array.sml"
,"/rnd/working/mlj/basis/Word8Vector.sml"
,"/rnd/working/mlj/basis/WordArray.sml"
,"/rnd/working/mlj/basis/WordVector.sml"
]