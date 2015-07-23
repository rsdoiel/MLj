signature Rule =
  sig
  type state
  type tactic = state -> state ImpSeq.t
  val main: state -> Fol.form
  val subgoals: state -> Fol.goal list
  val initial: Fol.form -> state
  val final: state -> bool
  val basic: int -> tactic
  val unify: int -> tactic
  val conjL: int -> tactic
  val conjR: int -> tactic
  val disjL: int -> tactic
  val disjR: int -> tactic
  val impL: int -> tactic
  val impR: int -> tactic
  val negL: int -> tactic
  val negR: int -> tactic
  val iffL: int -> tactic
  val iffR: int -> tactic
  val allL: int -> tactic
  val allR: int -> tactic
  val exL: int -> tactic
  val exR: int -> tactic
  end;
