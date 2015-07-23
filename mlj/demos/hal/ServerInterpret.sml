(* ServerInterpret.sml
   This is a simple top-level loop interpreter for using the Hal theorem prover.
   It makes rather cunning (though I say it myself...) use of a universal datatype
   for an untyped combinatory calculus. (Unfortunately, this trick has an adverse 
   effect on the quality of the generated code, since it forces all the functions which
   are embedded in the interpreter to be implemented as closures when they might otherwise
   be known....
*)
structure ServerInterpret =
struct

infix 5 --;
infix 3 >>;
infix 0 !!;

open Command;
open Rule;

datatype U = UF of U->U | UP of U*U | UI of int | US of string | UT of tactic | UUnit;

datatype Exp = EId of string | EI of int | ES of string | EApp of Exp*Exp | EP of Exp*Exp;

(* embed : Exp -> U
   embedfun : ('b->U) -> (U->'a) -> ('a->'b) -> U

   projF : U-> (U->U)
   projP : U ->(U*U)
   projI : U -> int
   projS : U -> string
   projT : U -> tactic
*)

exception interperror of string

fun projF (UF(f)) = f (* should raise application specific exception for match *)
  | projF _ = raise interperror "projF not function";
fun projP (UP(p)) = p
  | projP _ = raise interperror "projP not pair";
fun projI (UI(n)) = n
  | projI _ = raise interperror "projI not int";
fun projS (US(s)) = s
  | projS _ = raise interperror "projS not string";
fun projT (UT(t)) = t
  | projT _ = raise interperror "projT not tactic";


fun embedfun g h f = UF(g o f o h);

fun K x y = x;
fun cross (f,g) (x,y) = (f x, g y)
fun embedinttotac f = embedfun UT projI f;
fun embedtactactotac f = embedfun UT (cross(projT,projT) o projP) f

fun embed (EI(n)) = UI(n)
  | embed (ES(s)) = US(s)
  | embed (EApp(e1,e2)) = (case embed e1 of
                              UF(f) => let val z = embed e2
                                       in (f z)
                                       end 
                           | _ => raise interperror "application of non-function"
                          )
  | embed (EP(e1,e2)) = UP(embed e1, embed e2)
  | embed (EId(s)) = 
    (case s of "goal"  => embedfun (K UUnit) projS goal
             | "by"    => embedfun (K UUnit) projT by
             | "basic" => embedinttotac basic
             | "unify" => embedinttotac unify
             | "conjL" => embedinttotac conjL
             | "conjR" => embedinttotac conjR
             | "disjL" => embedinttotac disjL
             | "disjR" => embedinttotac disjR
             | "impL"  => embedinttotac impL
             | "impR"  => embedinttotac impR
             | "negL"  => embedinttotac negL
             | "negR"  => embedinttotac negR
             | "iffL"  => embedinttotac iffL
             | "iffR"  => embedinttotac iffR
             | "allL"  => embedinttotac allL
             | "allR"  => embedinttotac allR
             | "exL"   => embedinttotac exL
             | "exR"   => embedinttotac exR 
             | "safeSteps" => embedinttotac Tac.safeSteps
             | "quant" => embedinttotac Tac.quant
             | "step"  => embedinttotac Tac.step
             | "depth" => UT(Tac.depth)
             | "depthIt" => embedinttotac Tac.depthIt
             | "--"    => embedtactactotac (Tactical.--)
             | "||"    => embedtactactotac (Tactical.||)
             | "|@|"   => embedtactactotac (Tactical.|@|)
             | "all"   => UT(Tactical.all)
             | "no"    => UT(Tactical.no)
             | "try"   => embedfun UT projT Tactical.try
             | "repeat" => embedfun UT projT Tactical.repeat
             | "repeatDeterm"   => embedfun UT projT Tactical.repeatDeterm 
             | _ => raise interperror ("unknown identifier: " ^ s)

       
     );

fun utostring u = case u of
                     UUnit => "()"
                  |  UT(_) => "tactic"
                  |  US(s) => s
                  |  UI(n) => Int.toString n
                  |  UP(u1,u2) => "(" ^ (utostring u1)^","^(utostring u2)^")"
                  |  UF(_) => "function";

fun eval e = utostring (embed e);

(* hack up a quick parser for commands
   This repeats a lot of parser combinator stuff which could be avoided
   if we had functors. Yuck Yuck Yuck.
 *)
local 
 datatype token = Key of string  |  Id of string | Num of int | String of string;

  val alphas = []; (* don't actually have any here *)
  val symbols = ["(",")","[]","--","|@|","||"];

  local 
    (* stupid monomorphic membership functions *)
    infix smem;
    fun (s smem []) = false
      | ((s:string) smem (t::ts)) = (s=t) orelse (s smem ts);
    infix cmem;
    fun (s cmem []) = false
      | ((s:char) cmem (t::ts)) = (s=t) orelse (s cmem ts);

    fun is_letter_or_digit c =
        #"A"<=c andalso c<= #"Z" orelse
        #"a"<=c andalso c<= #"z" orelse
        #"0"<=c andalso c<= #"9";

    fun is_digit c = #"0" <= c andalso c <= #"9";

    val specials = explode"!@#$%^&*()+-={}[]:\"|;'\\,./?`_~<>";

    (*scanning of an alphanumeric identifier or keyword*)
    fun alphanum (id, c::cs) =
     	if is_letter_or_digit c then  alphanum (id^(Char.toString c), cs)
  				else  (id, c::cs)
      | alphanum (id, []) = (id, []);

    (*scanning of positive integers*)
    fun numeric(n, c::cs) =
          if is_digit c then numeric(n*10+((ord c)-(ord #"0")),cs)
                             else (n,c::cs)
     |  numeric(n,[]) = (n,[]);

    fun tokenof a = if a smem alphas  then  Key(a)  else  Id(a);

    (*scanning of a symbolic keyword*)
    fun symbolic (sy, c::cs) =
  	 if sy smem symbols orelse not (c cmem specials)
         then  (sy, c::cs)
         else  symbolic (sy^(Char.toString c), cs)
      | symbolic (sy, []) = (sy, []);

    (* scanning a quoted string constant *)
    fun scanstring (s,cs) = 
      case cs of [] => raise (Fail "non-terminated string constant")
               | (c::cs') => if c = #"\"" then (s,cs') 
                                        else scanstring (s^(Char.toString c), cs')

    fun scanning (toks, []) = rev toks    (*end of chars*)
      | scanning (toks, c::cs) =
          if is_digit c
          then (* number *)
             let val (n,cs2) = numeric((ord c)-(ord #"0"),cs)
             in scanning (Num(n) :: toks, cs2)
             end
  	  else if is_letter_or_digit c 
	  then (*identifier or keyword*)
	     let val (id, cs2) = alphanum(Char.toString c, cs)
	     in  scanning (tokenof id :: toks, cs2)
	     end
          else if (c = #"\"") 
          then (* quoted string constant *)
            let val (s,cs2) = scanstring ("",cs)
            in scanning(String s::toks,cs2)
            end
	  else if c cmem specials
	  then (*special symbol*)
	     let val (sy, cs2) = symbolic(Char.toString c, cs)
	     in  scanning (Key sy :: toks, cs2)
	     end
	  else (*spaces, line breaks, strange characters are ignored*)
	     scanning (toks, cs);
  in
    (*Scanning a list of characters into a list of tokens*)
    fun scan a = scanning([], explode a);
  end;

exception SynError of string;

  (*Phrase consisting of the keyword 'a' *)
  fun $a (Key b :: toks) =
        if a=b then (a,toks) else raise SynError  a
    | $a _ = raise SynError "Symbol expected";

  (*Phrase consisting of an identifier*)
  fun id (Id a :: toks) = (a,toks)
    | id toks = raise SynError  "Identifier expected";

  (*Phrase consisting of a number*)
  fun num (Num n :: toks) = (n,toks)
   |  num (Key "~" :: (Num n :: toks)) = (~n,toks)
   |  num toks = raise SynError "Integer expected";

  fun str (String s :: toks) = (s,toks)
   |  str toks = raise SynError "String expected";

  (*Application of f to the result of a phrase*)
  fun (ph>>f) toks = 
      let val (x,toks2) = ph toks
      in  (f x, toks2)  end;

  (*Alternative phrases*)
  fun (ph1 !! ph2) toks = ph1 toks   handle SynError _ => ph2 toks;

  (*Consecutive phrases*)
  fun (ph1 -- ph2) toks = 
      let val (x,toks2) = ph1 toks
	  val (y,toks3) = ph2 toks2
      in  ((x,y), toks3)  end;

  fun empty toks = ([],toks);

  (*Zero or more phrases*)
  fun repeat ph toks = (   ph -- repeat ph >> (op::)
                        !! empty   ) toks;

  fun infixes (ph,prec_of,apply) = 
    let fun over k toks = next k (ph toks)
        and next k (x, Key(a)::toks) = 
              if prec_of a < k then (x, Key a :: toks)
              else next k ((over (prec_of a) >> apply a x) toks)
          | next k (x, toks) = (x, toks)
    in  over 0  end;

  fun reader ph a =   (*Scan and parse, checking that no tokens remain*)
	 (case ph (scan a) of 
	      (x, []) => x
	    | (_, _::_) => raise SynError "Extra characters in phrase");

fun applylist (t,[]) = t
  | applylist (t,u::us) = applylist(EApp(t,u),us);

(* precedences for infix operators *)
fun infprec "--" = 2
  | infprec "||" = 2
  | infprec "|@|" = 2
  | infprec _ = ~1

(* making an application from an infix and two operands *)
fun infapply s x y = EApp(EId s,EP(x,y))

fun expr toks =
  ( atom -- repeat atom  >> applylist
  ) toks
and atom toks =
  (    id                      >> EId
   !!  $"(" -- exprwinf -- $")"    >> (fn ((_,x),_)=>x)
   !!  num                     >> EI
   !!  str                     >> ES
  ) toks
and exprwinf toks = infixes(expr, infprec,infapply) toks 
in 
  val read = reader exprwinf


(* this version of toplevel is just string to unit - it does the writing imperatively *)
fun toplevel line = Schan.write (let val s = eval (read line)
                                in (s^"\n")
                                end handle (SynError(x)) => ("Syntax error: "^x^"\n")
                                        | (interperror(x)) => ("Interp error :"^x^"\n")
                                        | (FolParsing.SyntaxErr(s)) => ("Form parse error:"^s^"\n"))

end
end
