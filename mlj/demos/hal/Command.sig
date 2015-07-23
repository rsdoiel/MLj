signature Command =
  sig
  val goal: string -> unit
  val by: Rule.tactic -> unit
  val pr: Rule.state -> unit
  val getState: unit -> Rule.state
  end;
