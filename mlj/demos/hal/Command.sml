structure Command :> Command =
  struct

  val currState = ref (Rule.initial (Fol.Pred("No goal yet!",[])));

  fun question (s,z) = " ?" :: s :: z;
  fun printParam (a,[]) = ()     (*print a line of parameter table*)
    | printParam (a,ts) = 
       Schan.write (String.concat (a :: " not in " :: 
			     foldr question ["\n"] ts));

  fun printGoals (_, []) = ()
    | printGoals (n, g::gs) = (DisplayFol.goal n g;  printGoals (n+1,gs));

  fun pr st =  (*print a proof state*)
      let val p  = Rule.main st  
	  and gs = Rule.subgoals st
      in  DisplayFol.form p;
	  if Rule.final st then Schan.write "No subgoals left!\n"
	  else (printGoals (1,gs);
	        app printParam (foldr Fol.goalParams [] gs))
      end;

  (*print new state, then set it*)
  fun setState state = (pr state;  currState := state);

  val goal = setState o Rule.initial o ParseFol.read;

  fun by tac = setState (ImpSeq.hd (tac (!currState)))
                handle ImpSeq.Empty => Schan.write "** Tactic FAILED! **\n" 

  fun getState() = !currState;
  end;
