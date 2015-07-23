structure FolParsing :> FolParsing =
  struct

(* infixes need to be repeated in each structure since we can't do them globally any more
   PNB
*)

infix 6 $--;
infix 5 --;
infix 3 >>;
infix 0 ||;

  type token = FolLex.token;

  exception SyntaxErr of string;

  (*Phrase consisting of an identifier*)
  fun id (FolLex.Id a :: toks) = (a,toks)
    | id toks = raise SyntaxErr "Identifier expected";

  (*Phrase consisting of the keyword 'a' *)
  fun $a (FolLex.Key b :: toks) = if a=b then (a,toks) 
			       else raise SyntaxErr a
    | $a _ = raise SyntaxErr "Symbol expected";

  (*The empty phrase!*)
  fun empty toks = ([],toks);

  (*Alternative phrases*)
  fun (ph1 || ph2) toks = ph1 toks 
			  handle SyntaxErr _ => ph2 toks;

  fun !! ph toks = ph toks
      handle SyntaxErr msg => raise Fail ("Syntax error: " ^ msg);

  (*One phrase then another*)
  fun (ph1 -- ph2) toks = 
      let val (x,toks2) = ph1 toks
	  val (y,toks3) = ph2 toks2
      in  ((x,y), toks3)  end;

  (*Application of f to the result of a phrase*)
  fun (ph>>f) toks = 
      let val (x,toks2) = ph toks
      in  (f x, toks2)  end;

(*  fun (a $-- ph) = ($a -- !!ph >> #2); *)
  fun (a $-- ph) = ($a -- !!ph >> (fn (x,y)=>y));

  (*Zero or more phrases*)
  fun repeat ph toks = (   ph -- repeat ph >> (op::)
                        || empty   ) toks;

  fun infixes (ph,prec_of,apply) = 
    let fun over k toks = next k (ph toks)
        and next k (x, FolLex.Key(a)::toks) = 
              if prec_of a < k then (x, FolLex.Key a :: toks)
              else next k ((over (prec_of a) >> apply a x) toks)
          | next k (x, toks) = (x, toks)
    in  over 0  end;

  (*Scan and parse, checking that no tokens remain*)
  fun reader ph a = 
	 (case ph (FolLex.scan a) of 
	      (x, []) => x
	    | (_, _::_) => raise SyntaxErr "Extra characters in phrase");

  end;
