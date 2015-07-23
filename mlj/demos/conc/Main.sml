structure Main :> sig val main : string option array option -> unit end =
struct

fun twos n = 
  (print (Int.toString n ^ " "); 
  twos (n+2))

fun main args = 
let
  val t1 = Threads.newWith 4 (fn _ => twos 0)
  val t2 = Threads.newWith 8 (fn _ => twos 1)
in
  ()
end


end
