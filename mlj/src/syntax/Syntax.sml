(*======================================================================*)
(* Abstract syntax tree datatypes for source SML                        *)
(*                                                                      *)
(* A file should be parsed to a DecItem list.  This means Andrew must   *)
(* check that structures happen at the right level, but it also means we*)
(* won't have to change anything should we later implement higher-order *)
(* functors. (except to add functor signatures.)                        *)
(*                                                                      *)
(* UNFIXED constructors are only there because we haven't resolved the  *)
(* fixity yet.                                                          *)
(* FIXED constructors are only used when we have resolved fixities      *)
(*                                                                      *)
(* Derived forms are left in there with one exception - the derived form*)
(* funid(strdec) => funid(struct strdec end) for functor applications   *)
(* This is so that MLJ can give better error messages                   *)
(*======================================================================*)
structure Syntax = 
struct

type Position=Position.pos

type Location=
  {left:Position,
   right:Position 
(* the convention (as for SML/NJ) is that #right is the character 1 after
   the last character. *)
   }

type symbol=Symbol.symbol

(*----------------------------------------------------------------------*)
(* Expressions (atexp, appexp, infexp and exp)                          *)
(*                                                                      *)
(* The parser does not sort out fixities, since this might as well be   *)
(* done in the next stage when scopes are resolved.  (We could combine  *)
(* the two stages but that would be messy.)                             *)
(*----------------------------------------------------------------------*)
datatype PreExp = 
  SCon of SCon.SCon             (* Special constant: int/real/string *)
| LongVid of OpLongVid          (* Symbol with possible Op, or field or
                                   method reference. *)
| FlatApp of Exp list           (* List of expressions to be 
                                   sorted out taking fixity into account.
                                   UNFIXED. *)
| App of Exp*Exp		(* Function application.
                                   FIXED. *)
| Fn of Match                   (* Pattern matching abstraction *)
| Let of Dec*Exp                (* Local declaration in an expression
                                   *)
| Handle of Exp*Match           (* Pattern matching exception handler *)
| Raise of Exp                  (* Raise an exception *)
| Record of (symbol*Exp) list   (* Record expression *)
                                (* Java application *)
| Java of Java.OpType * Ty option * JavaString.t option * Exp list
| Constraint of Exp*Ty          (* exp : ty constraint *)
| ConstraintGt of Exp*Ty        (* exp :> ty constraint (MLJ extension) *)

(* We put most derived forms in the expression datatype itself *)
| Tuple of Exp list             (* (exp_1, ..., exp_n) *)
| Hash of symbol                (* #lab *)
| DotHash of symbol             (* .#lab *)
| DotHashHash of symbol         (* .##lab *)
| Case of Exp*Match             (* case exp of match *)
| If of Exp*Exp*Exp             (* if exp_1 then exp_2 else exp_3 *)
| Orelse of Exp*Exp             (* exp_1 orelse exp_2 *)
| Andalso of Exp*Exp            (* exp_1 andalso exp_2 *)
| Sequence of Exp list          (* (exp_1; ...; exp_n) *)
| While of Exp*Exp              (* while exp_1 do exp_2 *)
| List of Exp list              (* [exp_1, ..., exp_n; exp] *)
    
(*----------------------------------------------------------------------*)
(* Declarations (dec, strdec)                                           *)
(*----------------------------------------------------------------------*)
and PreDecItem =
  Val of BoundTyVars * (Pat*Exp) list    (* Non-recursive bindings *)
| ValRec of BoundTyVars * (Pat*Exp) list (* Recursive bindings *)
| Fun of BoundTyVars * FValBindItem list list
                                        (* Function bindings.  
                                           FIXED *)
| FlatFun of BoundTyVars * FlatFValBindItem list list
                                        (* Function bindings.
                                           UNFIXED.
                                           The parentheses should be
                                           preserved, since
                                           fun (x y z)=3
                                           must be an infix declaration
                                           of y, while
                                           fun x y z=3 
                                           could be a declaration of x. *)
| Type of TypBind
| Datatype of DatBind * TypBind option
| Abstype of DatBind * TypBind option * Dec
| DatatypeCopy of symbol * longid
| Exception of (OpVid*ExBind) list      (* Exception declaration *)
| Local of Dec*Dec                      (* Local declaration *)
| Open of longid list                   (* Open structures *)
| JavaDec of JavaDec                    (* Java declaration *)
| Overload of symbol * Ty * OpLongVid list 
                                        (* Overloading declaration *)
| Infix of int * symbol list            (* Infix declaration *)
| Infixr of int * symbol list           (* Infixr declaration *)
| Nonfix of symbol list                 (* Nonfix declaration *)

| Structure of StrBind list             (* Structure declaration *)
| Signature of SigBind list             (* Signature declaration *)
| Functor of FunBind list               (* Functor declaration *)

(*----------------------------------------------------------------------*)
(* Exception bindings (exbind)                                          *)
(*----------------------------------------------------------------------*)
and ExBind =
  ExDesc of Ty option           (* Exception declaration *)
| ExBind of OpLongVid           (* Rebinding *)

(*----------------------------------------------------------------------*)
(* Signature information                                                *)
(*----------------------------------------------------------------------*)
and SigInfo =
  SigNone
| SigConcrete of SigExp
| SigAbstract of SigExp

(*----------------------------------------------------------------------*)
(* Types (ty, tyrow)                                                    *)
(*----------------------------------------------------------------------*)
and PreTy = 
  TyVar of symbol               (* Type variables *)
| TyCon of Ty list * longid     (* Type constructor *)
| TyFun of Ty*Ty                (* Function type *)
| TyRecord of (symbol*Ty) list  (* Record type *)

(* Derived form *)
| TyTuple of Ty list            (* ty_1 * ... * ty_n *)

(* Java extension *)
| TyClass of JavaString.t       (* Class type *)

(*----------------------------------------------------------------------*)
(* Patterns (atpat and pat)                                             *)
(*----------------------------------------------------------------------*)
and PrePat = 
  PatWild                       (* Wildcard *)
| PatSCon of SCon.SCon          (* Special constant: int/real/string *)
| PatVar of OpLongVid           (* Variable or nullary constructor. 
                                   The LongHash and LongHashHash keywords
                                   should not occur here.
                                   *)
| FlatPat of Pat list           (* List of expressions to be sorted out
                                   taking fixity into account.
                                   UNFIXED *)      
| PatCon of longid*Pat		(* Unary constructor
                                   FIXED *)
                           
| PatRecord of bool*((symbol*Pat) list) (* true = open, false = closed *)
| PatLayer of OpVid*Ty option*Pat
                                (* Layered pattern: id:ty as pat *)
| PatConstraint of Pat*Ty       (* Constraint: pat : ty *)

(* Derived forms *)
| PatTuple of Pat list          (* (pat_1, ..., pat_n) *)
| PatList of Pat list           (* [pat_1, ..., pat_n] *)

| OrPat of Pat list             (* pat_1 | pat_2 | . . . | pat_n *)
| PatParen of Pat               (* (pat).  This form is there so that
                                   we are not fooled by the illegal
                                   declarations:
                                      fun ((x + y)) = [exp]
                                   and
                                      val (x y) : [ty] as [pat]
                                   *)
(*----------------------------------------------------------------------*)
(* Specifications (spec)                                                *)
(*----------------------------------------------------------------------*)
and PreSpecItem =
  ValDesc of (symbol*Ty) list
| TypeDesc of (symbol list * symbol * Ty option) list
| EqTypeDesc of (symbol list * symbol) list
| DatatypeDesc of DatBind * TypBind option
| ExceptionDesc of ConBind list
| DatatypeDescCopy of symbol * longid
| StructureDesc of (symbol * SigExp) list
| Include of SigExp
| JavaDesc of JavaDec
| SharingType of longid list
| Sharing of longid list
(*----------------------------------------------------------------------*)
(* Signature expressions (sigexp)                                       *)
(*----------------------------------------------------------------------*)
and PreSigExp =
  SigSpec of Spec
| Sigid of symbol 
| Where of SigExp * symbol list * longid * Ty

(*----------------------------------------------------------------------*)
(* Structure expressions (strexp)                                       *)
(*----------------------------------------------------------------------*)
and PreStrExp =
  Struct of Dec      
| Strid of longid
| StrTransparent of StrExp * SigExp
| StrOpaque of StrExp * SigExp
| FunApp of symbol * StrExp
| StrLet of Dec * StrExp
(*----------------------------------------------------------------------*)
(* Functor Arguments (funarg)                                           *)
(*----------------------------------------------------------------------*)
and FunArg =
  StructArg of symbol * SigExp
| SpecArg of Spec
(*----------------------------------------------------------------------*)
(* Class items (classitem from documentation).                          *)
(*----------------------------------------------------------------------*)
and PreClassItem =
  Field of
  {
    modifiers : JavaFlags.javaflag list,
    name : JavaString.t,
    ty : Ty,
    initial : Exp option
  }

| Method of 
  {
    modifiers : JavaFlags.javaflag list,
    name : JavaString.t,
    args : (symbol option*Ty) list,
    result : Ty option,
    body : Exp option
  }

| Constructor of
  {
    modifiers : JavaFlags.javaflag list,
    args : (symbol option*Ty) list,
    inits : Inits,
    body : Exp option
  }

(*----------------------------------------------------------------------*)
(* Constructor invocation and field initialisation (inits).             *)
(*----------------------------------------------------------------------*)
and Inits =
  SuperInvoc of Exp list * (JavaString.t * Exp) list
| ThisInvoc of Exp list
| NoInit

(*----------------------------------------------------------------------*)
(* Java declaration                                                     *)
(*----------------------------------------------------------------------*)
and JavaDec = 
  ClassType of
  { 
    tycon : symbol,
    modifiers : JavaFlags.javaflag list,
    super : Ty option,
    implements : Ty list,
    body : ClassItem list 
  }             
  (* super=NONE means that none was specified, so before translating
     to backend code we should replace super=NONE by super=SOME object.
     (It is impossible to code java.lang.Object in MLJ).

     Interfaces have 
        hd #modifiers=INTERFACE
     and the extends list in #implements.
     INTERFACE cannot appear in any other way (there is no MLJ keyword
     for it).
     *)
| ClassException of OpVid * JavaString.t

and OpLongVid =
(* We try and optimise this, in particular the most common case
   of a symbol with no op.  The lists are guaranteed to have length>=2. *)
   Short of symbol (* one symbol *)
|  OpShort of symbol (* one symbol with an op *)
|  Long of symbol list (* A structure expression. *)

withtype BoundTyVars = { explicit : symbol list, implicit : symbol list }
and Exp = Location * PreExp 
and SigExp = Location * PreSigExp
and StrExp = Location * PreStrExp
and Ty = Location * PreTy
and Pat = Location * PrePat
and DecItem = Location * PreDecItem
and SpecItem = Location * PreSpecItem
and ClassItem = Location * PreClassItem
and MRule = Pat * Exp
and Match = MRule list
and longid = symbol list
and OpVid = bool * symbol
and FlatFValBindItem = Location * Pat list * Exp * Ty option
and FValBindItem = Location * symbol * Pat list * Exp * Ty option
and ConBind = OpVid * Ty option
and Dec = DecItem list
and Spec = SpecItem list


and TypBind = (symbol list * symbol * Ty) list
and DatBind = (symbol list * symbol * (ConBind list)) list 

(*----------------------------------------------------------------------*)
(* Top level bindings: structures, signatures and functors              *)
(*----------------------------------------------------------------------*)
and StrBind = symbol * StrExp * SigInfo
and SigBind = symbol * SigExp
and FunBind = symbol*FunArg*SigInfo*StrExp
end




