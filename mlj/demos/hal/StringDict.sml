structure StringDict :> StringDict = 
  struct

  type key = StringOrder.t;

  abstype 'a t = Leaf
               | Bran of key * 'a * 'a t * 'a t
    with

    exception E of key;

    val empty = Leaf;

    fun lookup (Bran(a,x,t1,t2), b) =
          (case StringOrder.compare(a,b) of
               GREATER => lookup(t1, b)
             | EQUAL   => x
             | LESS    => lookup(t2, b))
      | lookup (Leaf, b) = raise E b;

    fun insert (Leaf, b, y) = Bran(b, y, Leaf, Leaf)
      | insert (Bran(a,x,t1,t2), b, y) =
          (case StringOrder.compare(a,b) of
               GREATER => Bran(a, x, insert(t1,b,y), t2)
             | EQUAL   => raise E b
             | LESS    => Bran(a, x, t1, insert(t2,b,y)));

    fun update (Leaf, b, y) = Bran(b, y, Leaf, Leaf)
      | update (Bran(a,x,t1,t2), b, y) =
          (case StringOrder.compare(a,b) of
               GREATER => Bran(a, x, update(t1,b,y), t2)
             | EQUAL   => Bran(a, y, t1, t2)
             | LESS    => Bran(a, x, t1, update(t2,b,y)));
    end
  end;
