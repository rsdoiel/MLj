structure DisplayFol :> DisplayFol =
  struct
  fun enclose sexp = Pretty.blo(1, [Pretty.str"(", sexp, Pretty.str")"]); 

  fun commas [] = []
    | commas (sexp::sexps) = Pretty.str"," :: Pretty.brk 1 :: 
			     sexp :: commas sexps;

  fun list (sexp::sexps) = Pretty.blo(0, sexp :: commas sexps);  

  fun term (Fol.Param(a,_)) = Pretty.str a 
    | term (Fol.Var a) = Pretty.str ("?"^a)
    | term (Fol.Bound i) = Pretty.str "??UNMATCHED INDEX??"
    | term (Fol.Fun (a,ts)) = Pretty.blo(0, [Pretty.str a, args ts])
  and args [] = Pretty.str""
    | args ts = enclose (list (map term ts));

  (*display formula in context of operator with precedence k *)
  fun formp k (Fol.Pred (a,ts)) = Pretty.blo(0, [Pretty.str a, args ts])
    | formp k (Fol.Conn("~", [p])) = 
          Pretty.blo(0, [Pretty.str "~", formp (Fol.precOf "~") p])
    | formp k (Fol.Conn(C, [p,q])) =
	  let val pf = formp (Int.max(Fol.precOf C, k))
	      val sexp = Pretty.blo(0, [pf p, 
					Pretty.str(" "^C), 
					Pretty.brk 1, 
					pf q])
	  in  if (Fol.precOf C <= k) then (enclose sexp) else sexp
	  end
    | formp k (Fol.Quant(qnt,b,p)) =
	  let val q = Fol.subst 0 (Fol.Fun(b,[])) p
	      val sexp = Pretty.blo(2, 
				    [Pretty.str(qnt ^ " " ^ b ^ "."), 
				     Pretty.brk 1,  
				     formp 0 q])
	  in  if  k>0  then  (enclose sexp)  else sexp  end
    | formp k _ = Pretty.str"??UNKNOWN FORMULA??";

  fun formList [] = Pretty.str"empty"
    | formList ps = list (map (formp 0) ps);
  
  fun form p = Pretty.pr (formp 0 p, 50);

  fun goal (n:int) (ps,qs) = 
     Pretty.pr (Pretty.blo (4, [Pretty.str(" " ^ Int.toString n ^  ". "), 
				formList ps,
				Pretty.brk 2,
				Pretty.str"|-  ",
				formList qs]),
	 50);
  end;
