signature DisplayFol =
  sig
  val form: Fol.form -> unit
  val goal: int -> Fol.goal -> unit
  end
