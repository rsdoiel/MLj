signature Unify =
  sig
  exception Failed
  val atoms: Fol.form * Fol.form -> Fol.term StringDict.t
  val instTerm: Fol.term StringDict.t -> Fol.term -> Fol.term
  val instForm: Fol.term StringDict.t -> Fol.form -> Fol.form
  val instGoal: Fol.term StringDict.t -> Fol.goal -> Fol.goal
  end;
