signature ImpSeq = 
  sig
  type 'a t
  exception Empty
  val empty: 'a t
  val cons: 'a * (unit -> 'a t) -> 'a t
  val null: 'a t -> bool
  val hd: 'a t -> 'a
  val tl: 'a t -> 'a t
  val take: 'a t * int -> 'a list
  val toList: 'a t -> 'a list
  val fromList: 'a list -> 'a t
  val @ : 'a t * 'a t -> 'a t
  val interleave : 'a t * 'a t -> 'a t
  val concat: 'a t t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val filter: ('a -> bool) -> 'a t -> 'a t
  val cycle: ((unit -> 'a t) -> 'a t) -> 'a t
  end;
