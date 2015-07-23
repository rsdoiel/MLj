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

(* Reserved:>RESERVED fills in the reserved words in the symbol table.
   These are supposed to have numbers 0,1,....
   The numbering should be exactly the same as in TokenTable (which will
   raise Fail if it isn't).
   *)
structure Reserved:>RESERVED=
struct
   (* Lists of reserved words.  These should be derived from the lists in 
      TokenTable, with the functions omitted.  (query-replace-regexp ,fn.+), 
      with ), and sort out the last element of each list manually. *)

   val ML_reserved:string list=
     [
      (* 1-character symbols *)
      ("*"        ),
      ("|"        ),
      (":"        ),
      ("="        ),
      ("#"        ),

      (* 2-character symbols *)
      (":>"       ),
      ("->"       ),
      ("=>"       ),
      ("as"       ),
      ("do"       ),
      ("fn"       ),
      ("if"       ),
      ("in"       ),
      ("of"       ),
      ("op"       ),

      (* 3-character symbols *)
      ("end"      ),
      ("fun"      ),
      ("let"      ),
      ("rec"      ),
      ("sig"      ),
      ("val"      ),
      ("and"      ),

      (* 4-character symbols *)
      ("case"     ),
      ("else"     ),
      ("open"     ),
      ("then"     ),
      ("type"     ),
      ("with"     ),

      (* 5-character symbols *)
      ("infix"    ),
      ("local"    ),
      ("raise"    ),
      ("where"    ),
      ("while"    ),

      (* 6-character symbols *)
      ("eqtype"   ),
      ("handle"   ),
      ("infixr"   ),
      ("nonfix"   ),
      ("struct"   ),
      ("orelse"   ),

      (* 7-character symbols *)
      ("abstype"  ),
      ("functor"  ),
      ("include"  ),
      ("sharing"  ),
      ("andalso"  ),

      (* 8-character symbols *)
      ("datatype" ),
      ("withtype" ),

      (* 9-character symbols *)
      ("exception"),
      ("signature"),
      ("structure")
      ]

      val MLJ_reserved_optional:string list=[]

      val J_reserved_optional:string list=
        [
         ("pure")
         ]

      val J_reserved_compulsory:string list=
         (* 3-character symbols *)
        [
         ("new"),

         (* 4-character symbols *)
         ("cast"),
         ("this"),

         (* 5-character symbols *)
         ("field"),
         ("final"),
         ("super"),

         (* 6-character symbols *)
         ("invoke"),
         ("method"),
         ("public"),
         ("static"),

         (* 7-character symbols *)
         ("extends"),
         ("private"),

         (* 8-character symbols *)
         ("abstract"),
         ("getfield"),
         ("putfield"),
         ("volatile"),

         (* 9-character symbols *)
         ("classtype"),
         ("protected"),
         ("transient"),

         (* 10-character symbols *)
         ("implements"),
         ("instanceof"),

         (* 11-character symbols *)
         ("constructor"),

         (* 12-character symbols *)
         ("synchronized"),

         (* 13-character symbols *)
         ("interfacetype")
         ]

   fun reserve()=
      List.app(ignore o GeneralSymbol.symbolAsciiString) 
         (List.concat
           [ML_reserved,
            MLJ_reserved_optional,
            J_reserved_optional,
            J_reserved_compulsory])

end
