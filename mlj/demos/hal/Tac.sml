structure Tac : Tac =
  struct

infix 6 $--;
infix 5 --;
infix 3 >>;
infix 0 ||;
infix 0 |@|;

  local open Tactical Rule
    in
    (*Deterministic; applies one rule to the goal; no unification or variable
      instantiation; cannot render the goal unprovable.*)
    val safe =
          firstF [basic, 
		  conjL, disjR, impR, negL, negR, exL, allR, (*1 subgoal*) 
                  conjR, disjL, impL, iffL, iffR (*2 subgoals*)];

    fun safeSteps i = safe i -- repeatDeterm (safe i);

    (*expand a quantifier on the left and the right (if possible!) *)
    fun quant i = (allL i -- try (exR i)) || exR i;

    val depth = depthFirst final (safeSteps 1 || unify 1 || quant 1);

    fun step i = safeSteps i || (unify i |@| allL i |@| exR i);

    fun depthIt d = depthIter (final, d) (step 1);
    end
  end;

