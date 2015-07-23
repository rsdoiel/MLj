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

(* TokenTable is the structure that looks up reserved words in ML, and 
   atomises others.  It is functorised on MLJ_TOKENS. 

   For speed, this structure uses the SML/NJ unchecked array operations.
   *)
functor TokenTable(Tokens:MLJ_TOKENS):>TOKENTABLE where type
   token=(Tokens.svalue,Syntax.Position) Tokens.token =
struct
   val usub=Unsafe.Vector.sub 
   (* Replace by Vector.sub to get checked operations *)


   type input= (* Format of arguments to functions in this structure *)
     {characters:string, (* Where the tokens come from *)
      initial:int, (* Index of first character in the token *)
      length:int, (* Length of the token *)
      position:Syntax.Position
      }

   type token=(Tokens.svalue,Syntax.Position) Tokens.token

   val mkSymbol=Symbol.symbolAsciiStringSlice

   fun right(p,len)=
      Word.toInt(Word.fromInt p + Word.fromInt len)
   (* Used for moving right N positions.  We omit the overflow check. *)

   (* lookup_alpha, lookup_symbolic and lookup_underline all
      classify tokens by number (as output by Symbol.number).
      This is a very dirty trick but means we only have to
      hash symbols once.

      The reserved words are divided into four lists, according
      to when they are meaningful:

      Meaningful for         ML   (MLJ Java extension when preceded by _ ) 
                                  in the basis     outside the basis
      ML_reserved            X
      J_reserved_optional         X
      J_reserved_compulsory       X                X

      These are allocated numbers in this order.
      No words are both ML_reserved words and J_reserved words, though
      "and" used to be.
      *)   

   type mktok=Syntax.Position->token

   val ML_reserved:(string*mktok) list=
   (* We write the functions out explicitly to get New Jersey to
      compile them properly.  The only bit of this which can't
      be done by cut-and-paste is the length; we do this by
      sorting the reserved words by length. *)  
     [
      (* 1-character symbols *)
      ("*"        ,fn p=>Tokens.ASTERISK (p,right(p,1))),
      ("|"        ,fn p=>Tokens.BAR      (p,right(p,1))),
      (":"        ,fn p=>Tokens.COLON    (p,right(p,1))),
      ("="        ,fn p=>Tokens.EQUALOP  (p,right(p,1))),
      ("#"        ,fn p=>Tokens.HASH     (p,right(p,1))),

      (* 2-character symbols *)
      (":>"       ,fn p=>Tokens.COLONGT  (p,right(p,2))),
      ("->"       ,fn p=>Tokens.ARROW    (p,right(p,2))),
      ("=>"       ,fn p=>Tokens.DARROW   (p,right(p,2))),
      ("as"       ,fn p=>Tokens.AS       (p,right(p,2))),
      ("do"       ,fn p=>Tokens.DO       (p,right(p,2))),
      ("fn"       ,fn p=>Tokens.FN       (p,right(p,2))),
      ("if"       ,fn p=>Tokens.IF       (p,right(p,2))),
      ("in"       ,fn p=>Tokens.IN       (p,right(p,2))),
      ("of"       ,fn p=>Tokens.OF       (p,right(p,2))),
      ("op"       ,fn p=>Tokens.OP       (p,right(p,2))),

      (* 3-character symbols *)
      ("end"      ,fn p=>Tokens.END      (p,right(p,3))),
      ("fun"      ,fn p=>Tokens.FUN      (p,right(p,3))),
      ("let"      ,fn p=>Tokens.LET      (p,right(p,3))),
      ("rec"      ,fn p=>Tokens.REC      (p,right(p,3))),
      ("sig"      ,fn p=>Tokens.SIG      (p,right(p,3))),
      ("val"      ,fn p=>Tokens.VAL      (p,right(p,3))),
      ("and"      ,fn p=>Tokens.AND      (p,right(p,3))),

      (* 4-character symbols *)
      ("case"     ,fn p=>Tokens.CASE     (p,right(p,4))),
      ("else"     ,fn p=>Tokens.ELSE     (p,right(p,4))),
      ("open"     ,fn p=>Tokens.OPEN     (p,right(p,4))),
      ("then"     ,fn p=>Tokens.THEN     (p,right(p,4))),
      ("type"     ,fn p=>Tokens.TYPE     (p,right(p,4))),
      ("with"     ,fn p=>Tokens.WITH     (p,right(p,4))),

      (* 5-character symbols *)
      ("infix"    ,fn p=>Tokens.INFIX    (p,right(p,5))),
      ("local"    ,fn p=>Tokens.LOCAL    (p,right(p,5))),
      ("raise"    ,fn p=>Tokens.RAISE    (p,right(p,5))),
      ("where"    ,fn p=>Tokens.WHERE    (p,right(p,5))),
      ("while"    ,fn p=>Tokens.WHILE    (p,right(p,5))),

      (* 6-character symbols *)
      ("eqtype"   ,fn p=>Tokens.EQTYPE   (p,right(p,6))),
      ("handle"   ,fn p=>Tokens.HANDLE   (p,right(p,6))),
      ("infixr"   ,fn p=>Tokens.INFIXR   (p,right(p,6))),
      ("nonfix"   ,fn p=>Tokens.NONFIX   (p,right(p,6))),
      ("struct"   ,fn p=>Tokens.STRUCT   (p,right(p,6))),
      ("orelse"   ,fn p=>Tokens.ORELSE   (p,right(p,6))),

      (* 7-character symbols *)
      ("abstype"  ,fn p=>Tokens.ABSTYPE  (p,right(p,7))),
      ("functor"  ,fn p=>Tokens.FUNCTOR  (p,right(p,7))),
      ("include"  ,fn p=>Tokens.INCLUDE  (p,right(p,7))),
      ("sharing"  ,fn p=>Tokens.SHARING  (p,right(p,7))),
      ("andalso"  ,fn p=>Tokens.ANDALSO  (p,right(p,7))),

      (* 8-character symbols *)
      ("datatype" ,fn p=>Tokens.DATATYPE (p,right(p,8))),
      ("withtype" ,fn p=>Tokens.WITHTYPE (p,right(p,8))),

      (* 9-character symbols *)
      ("exception",fn p=>Tokens.EXCEPTION(p,right(p,9))),
      ("signature",fn p=>Tokens.SIGNATURE(p,right(p,9))),
      ("structure",fn p=>Tokens.STRUCTURE(p,right(p,9)))
      ]

   local
      open Java 
   in
      val J_reserved_optional:(string*mktok) list=
        [
         ("pure",fn p=>Tokens.BYTECODE(Pure   ,p,right(p,4)))
         ]

      val J_reserved_compulsory:(string*mktok) list=
         (* 3-character symbols *)
        [
         ("new",fn p=>Tokens.NEW(p,right(p,3))),

         (* 4-character symbols *)
         ("cast",fn p=>Tokens.CAST(p,right(p,4))),
         ("this",fn p=>Tokens.THIS(p,right(p,4))),

         (* 5-character symbols *)
         ("field",fn p=>Tokens.FIELD(p,right(p,5))),
         ("final",fn p=>Tokens.FINAL(p,right(p,5))),
         ("super",fn p=>Tokens.SUPER(p,right(p,5))),

         (* 6-character symbols *)
         ("invoke",fn p=>Tokens.INVOKE(p,right(p,6))),
         ("method",fn p=>Tokens.METHOD(p,right(p,6))),
         ("public",fn p=>Tokens.PUBLIC(p,right(p,6))),
         ("static",fn p=>Tokens.STATIC(p,right(p,6))),

         (* 7-character symbols *)
         ("extends",fn p=>Tokens.EXTENDS(p,right(p,7))),
         ("private",fn p=>Tokens.PRIVATE(p,right(p,7))),

         (* 8-character symbols *)
         ("abstract",fn p=>Tokens.ABSTRACT(p,right(p,8))),
         ("getfield",fn p=>Tokens.GETFIELD(p,right(p,8))),
         ("putfield",fn p=>Tokens.PUTFIELD(p,right(p,8))),
         ("volatile",fn p=>Tokens.VOLATILE(p,right(p,8))),

         (* 9-character symbols *)
         ("classtype",fn p=>Tokens.CLASSTYPE(p,right(p,9))),
         ("protected",fn p=>Tokens.PROTECTED(p,right(p,9))),
         ("transient",fn p=>Tokens.TRANSIENT(p,right(p,9))),

         (* 10-character symbols *)
         ("implements",fn p=>Tokens.IMPLEMENTS(p,right(p,10))),
         ("instanceof",fn p=>Tokens.INSTANCEOF(p,right(p,10))),

         (* 11-character symbols *)
         ("constructor",fn p=>Tokens.CONSTRUCTOR(p,right(p,11))),

         (* 12-character symbols *)
         ("synchronized",fn p=>Tokens.SYNCHRONIZED(p,right(p,12))),

         (* 13-character symbols *)
         ("interfacetype",fn p=>Tokens.INTERFACETYPE(p,right(p,13)))
         ]
   end

   val SAS=Symbol.symbolAsciiString

   (* Get the symbol table numbers for these *)
   val ML_reserved_nos=
      List.map(Symbol.number o SAS o #1) ML_reserved
   val last_ML=List.last ML_reserved_nos

   val J_reserved_optional_nos=
      List.map(Symbol.number o SAS o #1) J_reserved_optional
   val last_J_optional=List.last J_reserved_optional_nos

   val J_reserved_compulsory_nos=
      List.map(Symbol.number o SAS o #1) J_reserved_compulsory
   val last_J_compulsory=List.last J_reserved_compulsory_nos

   (* Check that they really are in order and that we haven't mucked up 
      getting the Reserved.reserve() symbols right and put in at the right 
      time.
      *)
   local
      fun is_ordered(i,[])=true
      |   is_ordered(i,h::t)=(h=i) andalso is_ordered(i+1,t)
      
      val rnos=
         List.concat
           [ML_reserved_nos,J_reserved_optional_nos,J_reserved_compulsory_nos]
      val ()=
         if is_ordered(0,rnos)
         then
            ()
         else
            raise Fail 
            ("TokenTable, Reserved and Symbol disagree upon symbol numbers \
             \for reserved words\n" ^
             (String.concat(List.map (fn i=>(Int.toString i ^",")) rnos)))
   in
   end

   (* Now store the functions in vectors *)
   val reserved_vector=
      Vector.fromList (List.map #2 ML_reserved)
   fun lookup_ML (n:int,p:Syntax.Position)=
      (usub(reserved_vector,n)) p 

   val J_vector=
      Vector.fromList
         ((List.map #2 J_reserved_optional) @
          (List.map #2 J_reserved_compulsory))
   fun lookup_J (n:int,p:Syntax.Position)=
      (usub(J_vector,n-(last_ML+1))) p
      


   fun lookup_tyvar{characters,initial,length,position} =
   let
      val len1=length-1
      val start=right(position,1)
      val after=right(start,len1)
   in
      Tokens.TYVAR(mkSymbol(characters,initial+1,len1),start,after)
   end

   fun lookup_alpha_unreserved{characters,initial,length,position}=
      Tokens.ALPHA(mkSymbol(characters,initial,length),position,right(position,length))

   fun lookup_alpha{characters,initial,length,position} =
   let
      val symbol=mkSymbol(characters,initial,length)
      val nsymbol=Symbol.number symbol
   in
      if nsymbol <= last_ML
      then
         lookup_ML(nsymbol,position)
      else
         Tokens.ALPHA(symbol,position,right(position,length))
   end

   fun lookup_symbolic_unreserved{characters,initial,length,position}=
      Tokens.SYMBOLIC(mkSymbol(characters,initial,length),position,right(position,length))

   fun lookup_symbolic{characters,initial,length,position} =
   let
      val symbol=mkSymbol(characters,initial,length)
      val nsymbol=Symbol.number symbol
   in
      if nsymbol <= last_ML
      then
         lookup_ML(nsymbol,position)
      else
         Tokens.SYMBOLIC(symbol,position,right(position,length))
   end
      
   datatype onetwo=ONE of token | TWO of token

   fun lookup_underline{characters,initial,length,position} =
   let
      val len1=length-1
      val start=right(initial,1)
      val position1=right(position,1)

      val symbol=mkSymbol(characters,start,len1)
      val nsymbol=Symbol.number symbol

      fun J_res()=ONE(lookup_J(nsymbol,position1))
      fun TWOAL()=
         TWO(Tokens.ALPHA(symbol,position1,right(position,length)))
      fun TWOML()=TWO(lookup_ML(nsymbol,position1))
   in
      if nsymbol<=last_J_compulsory
      then
         if nsymbol>last_J_optional
         then
            J_res()
         else
            if nsymbol>last_ML
            then
               if !LexState.frozen
               then
                  TWOAL()
               else
                  J_res()
            else
               TWOML()
      else
         TWOAL()
   end
end

