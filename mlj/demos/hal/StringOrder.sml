structure StringOrder :> StringOrder =
  struct
  type t = string;
  val compare = String.compare
  end;
