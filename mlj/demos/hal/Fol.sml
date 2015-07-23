structure Fol :> Fol =
  struct
  datatype term = Var   of string
		| Param of string * string list
		| Bound of int
		| Fun   of string * term list;
  datatype form = Pred  of string * term list
		| Conn  of string * form list
		| Quant of string * string * form;

  fun listeq eq ([],[]) = true
    | listeq eq (x::xs,y::ys) = (eq (x,y)) andalso
                                (listeq eq (xs,ys))

    | listeq _ (_,_) = false

  fun termeq(Var s, Var t) = s=t
    | termeq(Param(s,ss),Param(t,ts)) = (s=t) andalso
                                         (listeq (fn (a:string,b:string) => a=b) (ss,ts))
    | termeq(Bound m, Bound n) = m=n
    | termeq(Fun(s,ss),Fun(t,ts)) = (s=t) andalso
                                    (listeq termeq (ss,ts))
    | termeq(_,_) = false

  fun formeq(Pred(s,ss),Pred(t,ts)) = (s=t) andalso
                                      (listeq termeq (ss,ts))
    | formeq(Conn(s,ss),Conn(t,ts)) = (s=t) andalso
                                       (listeq formeq (ss,ts))
    | formeq(Quant(s1,s2,s3),Quant(t1,t2,t3)) = (s1=t1) andalso
                                                (s2=t2) andalso
                                                (formeq (s3,t3))
    | formeq _ = false

        
  type goal = form list * form list;

  (*Replace the term u1 by u2 throughout a term t*)
  fun replace (u1,u2) t =
      if termeq(t,u1) then u2 else
	  case t of Fun(a,ts) => Fun(a, map (replace(u1,u2)) ts)
		  | _         => t;

  (*Abstraction of a formula over the atomic term t. *)
  fun abstract i t (Pred(a,ts)) = Pred(a, map (replace (t, Bound i)) ts)
    | abstract i t (Conn(b,ps)) = Conn(b, map (abstract i t) ps)
    | abstract i t (Quant(qnt,b,p)) = Quant(qnt, b, abstract (i+1) t p);

  (*Replace (Bound i) by t in formula. t may contain no bound vars *)
  fun subst i t (Pred(a,ts)) = Pred(a, map (replace (Bound i, t)) ts)
    | subst i t (Conn(b,ps)) = Conn(b, map (subst i t) ps)
    | subst i t (Quant(qnt,b,p)) = Quant(qnt, b, subst (i+1) t p);

  (*Precedence table: used by parsing AND display! *)
  fun precOf "~"   = 4
    | precOf "&"   = 3
    | precOf "|"   = 2
    | precOf "<->" = 1
    | precOf "-->" = 1
    | precOf _     = ~1    (*means not an infix*);

  (*Accumulate a term function over all terms in a formula*)
  fun accumForm f (Pred(_,ts), z) = foldr f z ts
    | accumForm f (Conn(_,ps), z) = foldr (accumForm f) z ps
    | accumForm f (Quant(_,_,p), z) = accumForm f (p,z);

  (*Accumulate a form function over all formulae in a goal [POLYMORHPIC]*)
  fun accumGoal f ((ps,qs), z) = foldr f (foldr f z qs) ps;

  (*insertion into ordered list of strings*)
  fun insert (a,[]) = [a]
    | insert (a,b::bs) = case String.compare (a,b) of
                             LESS => a::b::bs
			   | EQUAL => b::bs
			   | GREATER => b::insert(a,bs);

  (*Accumulate all Vars in the term (except in a Param).*)
  fun termVars (Var a, bs) = insert(a,bs)
    | termVars (Fun(_,ts), bs) = foldr termVars bs ts
    | termVars (_, bs) = bs;
  val goalVars = accumGoal (accumForm termVars);

  (*Accumulate all Params*)
  fun termParams (Param(a,bs), pairs) = (a,bs) :: pairs
    | termParams (Fun(_,ts), pairs) = foldr termParams pairs ts
    | termParams (_, pairs) = pairs;
  val goalParams = accumGoal (accumForm termParams);
  end;

