structure Rule :> Rule =
  struct

  val _ = StringDict.empty

  (*A state contains subgoals, main goal, variable counter *)
  datatype state = State of Fol.goal list * Fol.form * int;

  type tactic = state -> state ImpSeq.t;

  fun main (State(gs,p,_)) = p
  and subgoals (State(gs,p,_)) = gs;

  (*initial state has one subgoal *)
  fun initial p = State([ ([],[p]) ], p, 0);

  (*final state has no subgoals.*)
  fun final (State(gs,_,_)) = null gs;

  (*add the goals "newgs" to replace subgoal i*)
  fun spliceGoals gs newgs i = List.take(gs,i-1) @ newgs @ List.drop(gs,i);

  (*goalF maps goal -> goal list; operator makes a deterministic tactic*)
  fun propRule goalF i (State(gs,p,n)) =
      let val gs2 = spliceGoals gs (goalF (List.nth(gs,i-1))) i
      in  ImpSeq.fromList [State(gs2, p, n)]  end
      handle _ => ImpSeq.empty;

  (*for basic sequents: with identical formulae on opposite sides*)
  val basic = propRule
	       (fn (ps,qs) => 
		if List.exists (fn p => List.exists (fn q => Fol.formeq(p,q)) qs) ps
		then [] else raise Match);

  (*Solves goal p|-q by unifying p with q.
    Returns list of successful environments. *)
  fun unifiable ([], _) = ImpSeq.empty
    | unifiable (p::ps, qs) = 
	let fun find [] = unifiable (ps,qs)
	      | find (q::qs) = ImpSeq.cons(Unify.atoms(p,q), fn() => find qs)
			       handle Unify.Failed => find qs
	in  find qs  end;

  fun inst env (gs,p,n) =
	State (map (Unify.instGoal env) gs,  Unify.instForm env p, n);

  (*for solvable goals with unifiable formulae on opposite sides*)
  fun unify i (State(gs,p,n)) =
    let val (ps,qs) = List.nth(gs,i-1)
	fun next env = inst env (spliceGoals gs [] i, p, n)
    in  ImpSeq.map next (unifiable(ps,qs))  end
    handle Subscript => ImpSeq.empty;


  (*** Tactics for propositional rules ***)

  (* return & delete first formula of the form Conn(a,_,_) *)
  fun splitConn a qs =
    let fun get [] = raise Match
	  | get (Fol.Conn(b,ps) :: qs) = if a=b then ps else get qs
	  | get (q::qs) = get qs;
	fun del [] = []
	  | del ((q as Fol.Conn(b,_)) :: qs) = if a=b then qs 
					       else q :: del qs
	  | del (q::qs) = q :: del qs
    in (get qs, del qs)  end;

  (*leftF transforms left-side formulae to new subgoals*)
  fun propL a leftF = propRule (fn (ps,qs) => leftF (splitConn a ps, qs));

  (*rightF transforms right-side formulae to new subgoals*)
  fun propR a rightF = propRule (fn (ps,qs) => rightF (ps, splitConn a qs));

  val conjL = propL "&" (fn (([p1,p2], ps), qs) => [(p1::p2::ps, qs)]);

  val conjR = propR "&" 
      (fn (ps, ([q1,q2], qs)) => [(ps, q1::qs),  (ps, q2::qs)]);

  val disjL = propL "|" 
      (fn (([p1,p2], ps), qs) => [(p1::ps, qs),  (p2::ps, qs)]);

  val disjR = propR "|" (fn (ps, ([q1,q2], qs)) => [(ps, q1::q2::qs)]);

  val impL = propL "-->" 
      (fn (([p1,p2], ps), qs) => [(p2::ps, qs),  (ps, p1::qs)]);

  val impR = propR "-->" (fn (ps, ([q1,q2], qs)) => [(q1::ps, q2::qs)]);

  val negL = propL "~" (fn (([p], ps), qs) => [(ps, p::qs)]);

  val negR = propR "~" (fn (ps, ([q], qs)) => [(q::ps, qs)]);

  val iffL = propL "<->" 
      (fn (([p1,p2], ps), qs) => [(p1::p2::ps, qs),  (ps, p1::p2::qs)]);

  val iffR = propR "<->"
      (fn (ps, ([q1,q2], qs)) => [(q1::ps, q2::qs),  (q2::ps, q1::qs)]);


  (*** Tactics for quantifier rules ***)

  (* return & delete first formula of the form Quant(qnt,_,_) *)
  fun splitQuant qnt qs =
    let fun get [] = raise Match
	  | get ((q as Fol.Quant(qnt2,_,p)) :: qs) = if qnt=qnt2 then q
						     else get qs
	  | get (q::qs) = get qs;
	fun del [] = []
	  | del ((q as Fol.Quant(qnt2,_,p)) :: qs) = if qnt=qnt2 then qs
						     else q :: del qs
	  | del (q::qs) = q :: del qs
    in (get qs, del qs)  end;

  fun letter n = String.substring("abcdefghijklmnopqrstuvwxyz", n, 1)

  fun gensym n = (* the "_" prevents clashes with user's variable names*)
     if n<26 then "_" ^ letter n
     else gensym(n div 26) ^ letter(n mod 26);

  fun quantRule goalF i (State(gs,p,n)) =
      let val gs2 = spliceGoals gs (goalF (List.nth(gs,i-1), gensym n)) i
      in  ImpSeq.fromList [State(gs2, p, n+1)]  end
      handle _ => ImpSeq.empty;

  val allL = quantRule (fn ((ps,qs), b) =>
      let val (qntForm as Fol.Quant(_,_,p), ps') = splitQuant "ALL" ps
	  val px = Fol.subst 0 (Fol.Var b) p
      in  [(px :: ps' @ [qntForm], qs)]  end);

  val allR = quantRule (fn ((ps,qs), b) =>
      let val (Fol.Quant(_,_,q), qs') = splitQuant "ALL" qs
	  val vars = Fol.goalVars ((ps,qs), [])
	  val qx = Fol.subst 0 (Fol.Param(b, vars)) q
      in  [(ps, qx::qs')]  end);

  val exL = quantRule (fn ((ps,qs), b) =>
      let val (Fol.Quant(_,_,p), ps') = splitQuant "EX" ps
	  val vars = Fol.goalVars ((ps,qs), [])
	  val px = Fol.subst 0 (Fol.Param(b, vars)) p
      in  [(px::ps', qs)]  end);

  val exR = quantRule (fn ((ps,qs), b) =>
      let val (qntForm as Fol.Quant(_,_,q), qs') = splitQuant "EX" qs
	  val qx = Fol.subst 0 (Fol.Var b) q
      in  [(ps, qx :: qs' @ [qntForm])]  end);

  end;

