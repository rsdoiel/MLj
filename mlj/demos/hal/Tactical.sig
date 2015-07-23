signature Tactical =
  sig
  type ('a,'b) multifun = 'a -> 'b ImpSeq.t
  val -- :   ('a,'b) multifun * ('b,'c) multifun -> ('a,'c) multifun
  val || : ('a,'b) multifun * ('a,'b) multifun -> ('a,'b) multifun
  val |@| : ('a,'b) multifun * ('a,'b) multifun -> ('a,'b) multifun
  val all : ('a,'a) multifun
  val no :  ('a,'b) multifun
  val try :    ('a,'a) multifun -> ('a,'a) multifun
  val repeat : ('a,'a) multifun -> ('a,'a) multifun
  val repeatDeterm : ('a,'a) multifun -> ('a,'a) multifun
  val depthFirst : ('a -> bool) -> ('a,'a) multifun -> ('a,'a) multifun
  val depthIter : ('a -> bool) * int -> ('a,'a) multifun -> ('a,'a) multifun
  val firstF : ('a -> ('b,'c)multifun) list -> 'a -> ('b,'c)multifun
  end;
