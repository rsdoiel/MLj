(*TACTIC-BASED PROVER FOR CLASSICAL FIRST-ORDER LOGIC*)

(*variables  
    a,b,c: string     qnt: string (quantifier name)  
    i,j: int    (Bound indices)
    t,u: term    p,q: form
    x,y: any     f,g: functions *)

(**** Terms and Formulae ****)
signature Fol =
  sig
  datatype term = Var   of string
		| Param of string * string list
		| Bound of int
		| Fun   of string * term list
  datatype form = Pred  of string * term list
		| Conn  of string * form list
		| Quant of string * string * form

  val termeq : term * term -> bool
  val formeq : form * form -> bool

  type goal = form list * form list
  val precOf: string -> int
  val abstract: int -> term -> form -> form
  val subst: int -> term -> form -> form
  val termVars: term * string list -> string list
  val goalVars: goal * string list -> string list
  val termParams: term * (string * string list) list 
                  -> (string * string list) list
  val goalParams: goal * (string * string list) list 
                  -> (string * string list) list
  end;
 