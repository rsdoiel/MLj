structure ParseFol :> ParseFol =
  struct

infix 6 $--;
infix 5 --;
infix 3 >>;
infix 0 ||;

  local 

    open FolParsing 
    (*One or more phrases separated by commas*)
    fun list ph = ph -- repeat ("," $-- ph) >> (op::);

    (*Either (ph,...,ph) or empty. *)
    fun pack ph =    "(" $-- list ph -- $")" >> (fn {1=x,...}=>x) (* was #1 *)
		  || empty;

    (*Make a quantifier from parsed information*)
    fun makeQuant ((qnt,b),p) = 
	Fol.Quant(qnt, b, Fol.abstract 0 (Fol.Fun(b,[])) p);

    (*Make a connective*)
    fun makeConn a p q = Fol.Conn(a, [p,q]);
    fun makeNeg p = Fol.Conn("~", [p]);

    fun term toks =
      (   id   -- pack term		>> Fol.Fun
       || "?" $-- id			>> Fol.Var   ) toks;

    fun form toks =
      (   $"ALL" -- id -- "." $-- form 	>> makeQuant
       || $"EX"  -- id -- "." $-- form 	>> makeQuant
       || infixes (primary, Fol.precOf, makeConn)  ) toks
    and primary toks =
      (   "~" $-- primary      		>> makeNeg
       || "(" $-- form -- $")" 		>> (fn {1=x,...}=>x) (*#1*)
       || id -- pack term		>> Fol.Pred   )  toks;
    in
        val read = reader form
    end
  end;
