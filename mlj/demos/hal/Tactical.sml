structure Tactical :> Tactical  =
  struct

infix 6 $--;
infix 5 --;
infix 3 >>;
infix 0 ||;
infix 0 |@|;

  type ('a,'b)multifun = 'a -> 'b ImpSeq.t

  (*-- performs one tactic followed by another*)
  fun (tac1 -- tac2) x = ImpSeq.concat (ImpSeq.map tac2 (tac1 x));

  (*|| commits to the first successful tactic: no backtracking. *)
  fun (tac1 || tac2) x = 
      let val y = tac1 x 
      in  if ImpSeq.null y  then  tac2 x  else y  end;

  (*|@| combines the results of two tactics with backtracking.*)
  fun (tac1 |@| tac2) x = 
      ImpSeq.concat(ImpSeq.cons(tac1 x,  (*delay application of tac2!*)
				fn()=> ImpSeq.cons(tac2 x, 
						   fn()=> ImpSeq.empty)));

  (*accepts all states unchanged;  identity of --*)
  fun all x = ImpSeq.fromList [x];

  (*accepts no states;  identity of || and |@|*)
  fun no x = ImpSeq.empty;

  fun try tac = tac || all;

  (*Performs no backtracking: quits when it gets stuck*)
  fun repeat tac x = (tac -- repeat tac || all) x;

  fun repeatDeterm tac x = 
      let fun drep x = drep (ImpSeq.hd (tac x))
                       handle ImpSeq.Empty => x
      in  ImpSeq.fromList [drep x]  end;      

  (*Repeats again and again until "pred" reports proof tree as satisfied*)
  fun depthFirst pred tac x =
     (if pred x  then  all 
		 else  tac -- depthFirst pred tac) x;

  fun depthIter (pred,d) tac x =
   let val next = ImpSeq.toList o tac
       fun dfs i (y, sf) () = 
	    if i<0 then sf()
	    else if i<d andalso pred y
		 then ImpSeq.cons(y, foldr (dfs (i-1)) sf (next y))
	         else foldr (dfs (i-1)) sf (next y) ()
       fun deepen k = dfs k (x, fn()=> deepen (k+d)) ()
   in  deepen 0  end;

  fun orelseF (tac1, tac2) u = tac1 u || tac2 u;

  (*For combining tactic functions*)
  fun firstF ts = foldr orelseF (fn _ => no) ts;
  end;

