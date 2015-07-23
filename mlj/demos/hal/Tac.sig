signature Tac =
  sig
  val safeSteps: int -> Rule.tactic
  val quant: int -> Rule.tactic
  val step: int -> Rule.tactic
  val depth: Rule.tactic
  val depthIt: int -> Rule.tactic
  end;
