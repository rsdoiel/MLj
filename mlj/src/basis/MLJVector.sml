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

structure MLJVector =
struct

structure BoolVector = struct

open Vector

type elem = bool
type vector = bool vector

end

structure IntVector = 
struct

open Vector

type elem = int
type vector = int vector

end

structure Int64Vector = 
struct

open Vector

type elem = Int64.int
type vector = Int64.int vector

end

structure RealVector =
struct

open Vector

type elem = real
type vector = real vector

end

structure Word8Vector =
struct

open Vector

type elem = Word8.word
type vector = Word8.word vector

end

structure WordVector =
struct

open Vector

type elem = Word.word
type vector = Word.word vector

end

structure Word64Vector =
struct

open Vector

type elem = Word64.word
type vector = Word64.word vector

end

end


