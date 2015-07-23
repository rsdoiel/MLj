signature StringOrder = 
  sig
  type t = string (* Made explicit *)
  val compare: t*t -> order
  end;
