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
(* Special constants							*)
(*======================================================================*)
structure SCon =
struct
   local 
      open IntConvFlags 
   in
      datatype SCon = 
        NumCon of Base*Kind*string
      | StrCon of JavaString.t
      | CharCon of JavaInt.t
      | RealCon of string
      
      
      (*--------------------------------------------------------------------*)
      (* Convert a special constant into its original string form for	    *)
      (* pretty-printing.                                                   *)
      (*--------------------------------------------------------------------*)
      fun toString scon =
        (case scon of
          NumCon(Hex, Signed false, s) => "0x" ^ s
        | NumCon(Hex, Signed true, s) => "~0x" ^ s
        | NumCon(Decimal, Signed false, s) => s
        | NumCon(Decimal, Signed true, s) => "~" ^ s
        | NumCon(Hex, Unsigned, s) => "0wx" ^ s
        | NumCon(Decimal, Unsigned, s) => "0w" ^ s
        | RealCon s => s
        | StrCon s => "\"" ^ JavaString.toMLString s ^ "\""
        | CharCon c => 
          let
             val SOME jc=JavaChar.fromJavaInt c
          in
             "#\"" ^ (JavaChar.toMLescape jc) ^ "\""
          end
        )
   end
end



