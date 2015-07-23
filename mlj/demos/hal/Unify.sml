structure Unify :> Unify =
  struct
  exception Failed;

  (*Naive unification of terms containing no bound variables*)
  fun unifyLists env =
    let fun chase (Fol.Var a) =  (*Chase variable assignments*)
	      (chase(StringDict.lookup(env,a)) 
	       handle StringDict.E _ => Fol.Var a)
	  | chase t = t
	fun occurs a (Fol.Fun(_,ts)) = occsl a ts
	  | occurs a (Fol.Param(_,bs)) = occsl a (map Fol.Var bs)
	  | occurs a (Fol.Var b) = 
	        (a=b) orelse (occurs a (StringDict.lookup(env,b))  
			      handle StringDict.E _ => false)
	  | occurs a _ = false
	and occsl a = List.exists (occurs a)
	and unify (Fol.Var a, t) = 
	      if Fol.termeq(t, Fol.Var a) then env 
              else if occurs a t then raise Failed  
	                         else StringDict.update(env,a,t)
	  | unify (t, Fol.Var a) = unify (Fol.Var a, t)
	  | unify (Fol.Param(a,_), Fol.Param(b,_)) =
	      if a=b then env  else  raise Failed
	  | unify (Fol.Fun(a,ts), Fol.Fun(b,us)) =  
	      if a=b then unifyl(ts,us) else raise Failed
	  | unify _ =  raise Failed
       and unifyl ([],[]) = env
	 | unifyl (t::ts, u::us) =
               unifyLists (unify (chase t, chase u)) (ts,us)
	 | unifyl _ =  raise Failed
      in  unifyl  end

  (*Unification of atomic formulae*)
  fun atoms (Fol.Pred(a,ts), Fol.Pred(b,us)) =  
	  if a=b then unifyLists StringDict.empty (ts,us)  
	         else raise Failed
    | atoms _ =  raise Failed;

  (*Instantiate a term by an environment*)
  fun instTerm env (Fol.Fun(a,ts)) = Fol.Fun(a, map (instTerm env) ts)
    | instTerm env (Fol.Param(a,bs)) =
	Fol.Param(a, foldr Fol.termVars [] 
		           (map (instTerm env o Fol.Var) bs))
    | instTerm env (Fol.Var a) = (instTerm env (StringDict.lookup(env,a))
			      handle StringDict.E _ => Fol.Var a)
    | instTerm env t = t;

  (*Instantiate a formula*)
  fun instForm env (Fol.Pred(a,ts))   = Fol.Pred(a, map (instTerm env) ts)
    | instForm env (Fol.Conn(b,ps))   = Fol.Conn(b, map (instForm env) ps)
    | instForm env (Fol.Quant(qnt,b,p)) = Fol.Quant(qnt, b, instForm env p);

  fun instGoal env (ps,qs) = 
        (map (instForm env) ps, map (instForm env) qs);
  end;
