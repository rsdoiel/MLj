signature Pretty = 
  sig
   type t
   val blo : int * t list -> t
   val str : string -> t
   val brk : int -> t
   val pr  : t * int -> unit
   end;

