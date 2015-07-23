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
(* Abstract syntax tree datatypes for source SML			*)
(*======================================================================*)
structure Syntax = 
struct

(*----------------------------------------------------------------------*)
(* Expressions (atexp, appexp, infexp and exp)                        	*)
(*----------------------------------------------------------------------*)
datatype PreExp = 
  SCon of SCon.SCon		(* Special constant: int/real/string *)
| Vid of longid			(* Variable, constructor or exception *)
| App of Exp*Exp		(* Function application *)
| Fn of Match			(* Pattern matching abstraction *)
| Let of Dec*Exp		(* Local declaration in an expression *)
| Handle of Exp*Match		(* Pattern matching exception handler *)
| Raise of Exp			(* Raise an exception *)
| Record of (string*Exp) list	(* Record expression *)
                                (* Java application *)
| Java of Java.OpType * Ty option * string option * Exp list
| Constraint of Exp*Ty		(* exp : ty constraint *)

(* We put most derived forms in the expression datatype itself *)
| Tuple of Exp list		(* (exp_1, ..., exp_n) *)
| Hash of string		(* #lab *)
| Case of Exp*Match		(* case exp of match *)
| If of Exp*Exp*Exp		(* if exp_1 then exp_2 else exp_3 *)
| Orelse of Exp*Exp		(* exp_1 orelse exp_2 *)
| Andalso of Exp*Exp		(* exp_1 andalso exp_2 *)
| Sequence of Exp list		(* (exp_1; ...; exp_n) *)
| While of Exp*Exp		(* while exp_1 do exp_2 *)
| List of Exp list		(* [exp_1, ..., exp_n; exp] *)
    
(*----------------------------------------------------------------------*)
(* Declarations (dec, strdec)                                           *)
(*----------------------------------------------------------------------*)
and PreDecItem =
  Val of BoundTyVars * Match        	(* Non-recursive bindings *)
| ValRec of BoundTyVars * (string*Exp) list (* Recursive bindings *)
| Fun of BoundTyVars* FValBindItem list list
                                        (* Function bindings derived form *)
| Type of TypBind
| Datatype of DatBind * TypBind option
| Abstype of DatBind * TypBind option * Dec
| DatatypeCopy of string * longid
| Exception of (string*ExBind) list	(* Exception declaration *)
| Local of Dec*Dec			(* Local declaration *)
| Open of longid list			(* Open structures *)
| JavaDec of JavaDec                    (* Java declaration *)
| Structure of (string * StrExp) list   (* Structure declaration *)
| Overload of string * Ty * longid list (* Overloading declaration *)

(*----------------------------------------------------------------------*)
(* Exception bindings (exbind)                                          *)
(*----------------------------------------------------------------------*)
and ExBind =
  ExDesc of Ty option		(* Exception declaration *)
| ExBind of longid		(* Rebinding *)

(*----------------------------------------------------------------------*)
(* Types (ty, tyrow)                              			*)
(*----------------------------------------------------------------------*)
and PreTy = 
  TyVar of string 		(* Type variables *)
| TyCon of Ty list * longid	(* Type constructor *)
| TyFun of Ty*Ty		(* Function type *)
| TyRecord of (string*Ty) list	(* Record type *)

(* Derived form *)
| TyTuple of Ty list		(* ty_1 * ... * ty_n *)

(* Java extension *)
| TyClass of string             (* Class type *)

(*----------------------------------------------------------------------*)
(* Patterns (atpat and pat)                                             *)
(*----------------------------------------------------------------------*)
and PrePat = 
  PatWild			(* Wildcard *)
| PatSCon of SCon.SCon		(* Special constant: int/real/string *)
| PatVar of longid		(* Variable or nullary constructor *)
| PatCon of longid*Pat		(* Unary constructor *)
| PatRecord of bool*((string*Pat) list) (* true = open, false = closed *)
| PatLayer of string*Pat	(* Layered pattern: id as pat *)
| PatConstraint of Pat*Ty	(* Constraint: pat : ty *)

(* Derived forms *)
| PatTuple of Pat list		(* (pat_1, ..., pat_n) *)
| PatList of Pat list		(* [pat_1, ..., pat_n] *)

(*----------------------------------------------------------------------*)
(* Specifications (spec)                                                *)
(*----------------------------------------------------------------------*)
and PreSpecItem =
  ValDesc of (string*Ty) list
| TypeDesc of (string list * string * Ty option) list
| EqTypeDesc of TySort.Sort * (string list * string) list
| DatatypeDesc of DatBind * TypBind option
| ExceptionDesc of ConBind list
| DatatypeDescCopy of string * longid
| StructureDesc of (string * SigExp) list
| Include of SigExp
| JavaDesc of JavaDec
| Sharing of Spec * longid list

(*----------------------------------------------------------------------*)
(* Signature expressions (sigexp)					*)
(*----------------------------------------------------------------------*)
and PreSigExp =
  SigSpec of Spec
| Sigid of string 
| Where of SigExp * string list * longid * Ty

(*----------------------------------------------------------------------*)
(* Structure expressions (strexp)					*)
(*----------------------------------------------------------------------*)
and PreStrExp =
  Struct of Dec      
| Strid of longid
| StrTransparent of StrExp * SigExp
| StrOpaque of StrExp * SigExp
| FunApp of string * StrExp
| StrLet of Dec * StrExp

(*----------------------------------------------------------------------*)
(* Class items (classitem from documentation).				*)
(*----------------------------------------------------------------------*)
and PreClassItem =
  Field of
  {
    modifiers : Field.flag list,
    name : string,
    ty : Ty,
    initial : Exp option
  }

| Method of 
  {
    modifiers : Method.flag list,
    name : string,
    args : (string option*Ty) list,
    result : Ty option,
    body : Exp option
  }

| Constructor of
  {
    modifiers : Method.flag list,
    args : (string option*Ty) list,
    inits : Inits,
    body : Exp option
  }

(*----------------------------------------------------------------------*)
(* Constructor invocation and field initialisation (inits).		*)
(*----------------------------------------------------------------------*)
and Inits =
  SuperInvoc of Exp list * (string * Exp) list
| ThisInvoc of Exp list
| NoInit

(*----------------------------------------------------------------------*)
(* Java declaration							*)
(*----------------------------------------------------------------------*)
and JavaDec = 
  ClassType of
  { 
    tycon : string,
    modifiers : Class.flag list,
    super : Ty option,
    implements : Ty list,
    body : ClassItem list 
  }	 	

| ClassException of string * string

(*----------------------------------------------------------------------*)
(* Top-level binding							*)
(*----------------------------------------------------------------------*)
and TopBind = 
  StrBind of StrBind
| SigBind of SigBind
| FunBind of FunBind
| EmptyBind

withtype BoundTyVars = { explicit : string list, implicit : string list }
and Exp = Lex.Location * PreExp 
and SigExp = Lex.Location * PreSigExp
and StrExp = Lex.Location * PreStrExp
and Ty = Lex.Location * PreTy
and Pat = Lex.Location * PrePat
and DecItem = Lex.Location * PreDecItem
and SpecItem = Lex.Location * PreSpecItem
and ClassItem = Lex.Location * PreClassItem
and MRule = Pat * Exp
and Match = MRule list

and FValBindItem = Lex.Location * string * Pat list * Exp
and ConBind = string * Ty option
and longid = string list
and Dec = DecItem list
and Spec = SpecItem list

and TypBind = (string list * string * Ty) list
and DatBind = (string list * string * (ConBind list)) list 

(*----------------------------------------------------------------------*)
(* Top level bindings: structures, signatures and functors		*)
(*----------------------------------------------------------------------*)
and StrBind = string * StrExp
and SigBind = string * SigExp
and FunBind = string * string * SigExp * StrExp

end
