structure ImpSeq :> ImpSeq =
  struct
  datatype 'a t  = Nil
		 | Cons of 'a * ('a t) ref
		 | Delayed of unit -> 'a t;

  exception Empty;

  fun delay xf = ref(Delayed xf);

  (*sequence "constructors" for export*)
  val empty = Nil;

  fun cons(x,xf) = Cons(x, delay xf);

  (*gets tail value, perhaps with the side effect of storing it*)
  fun force xp = case !xp of
                     Delayed f => let val s = f()
		      	          in  xp := s;  s  end
     	           | s => s;

  (** these functions do not expect Delayed -- it is only permissible
      in the tail of a Cons, where it is enclosed in a reference **)

  fun null Nil = true
    | null (Cons _) = false;

  fun hd Nil = raise Empty
    | hd (Cons(x,_)) = x;

  fun tl Nil = raise Empty
    | tl (Cons(_,xp)) = force xp;

  fun take (xq, 0) = []
    | take (Nil, n) = []
    | take (Cons(x,xp), n) = x :: take (force xp, n-1);

  fun toList Nil = []
    | toList (Cons(x,xp)) = x :: toList (force xp);

  fun fromList [] = Nil
    | fromList (x::xs) = cons(x, fn()=> fromList xs);

  fun Nil @ yq = yq
    | (Cons(x,xp)) @ yq =
	  Cons(x, delay(fn()=> (force xp) @ yq));

  fun interleave (Nil,    yq) = yq
    | interleave (Cons(x,xp), yq) = 
	  Cons(x, delay (fn()=> interleave(yq, force xp)));

  (*concatenate a sequence of sequences, taking care not to loop! *)
   fun concat xqq =
     if null xqq then empty
     else if null(hd xqq) then concat(tl xqq)
	  else cons(hd(hd xqq),  
		    fn()=> tl(hd xqq) @ concat(tl xqq));

  (** functionals for sequences **)
  fun map f Nil  = Nil
    | map f (Cons(x,xp)) = 
          Cons(f x, delay(fn()=> map f (force xp)));

  fun filter pred Nil = Nil
    | filter pred (Cons(x,xp)) =
	  if pred x 
          then Cons(x, delay(fn()=> filter pred (force xp)))
	  else filter pred (force xp);

  (*idea thanks to C. Reade, see Appendix 3 of his book;
    seqfn must not call its argument outside of a closure;
    lest it get Nil rather than the cycle. *)
  fun cycle seqfn =
      let val knot = ref Nil
      in  knot := seqfn (fn()=> !knot);  !knot  end;
  end;
