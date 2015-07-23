signature StringDict = 
  sig
  type key = string			(*type of keys 
                                          Made explicit in signature so that we can use opaque matching
                                        *)
  type 'a t				(*type of tables*)
  exception E of key			(*errors in lookup, insert*)
  val empty: 'a t			(*the empty dictionary*)
  val lookup: 'a t * key -> 'a
  val insert: 'a t * key * 'a -> 'a t
  val update: 'a t * key * 'a -> 'a t
  end;
