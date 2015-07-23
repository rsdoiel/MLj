(*======================================================================*)
(* Integer quicksort                					*)
(*======================================================================*)
structure Sort :> 
sig
  val main : string option array option -> unit
end 
=
struct

(*----------------------------------------------------------------------*)
(* Print a list of numbers 						*)
(*----------------------------------------------------------------------*)
fun printList [] = ()
  | printList (x::xs) = 
    (print (Int.toString x); print " "; printList xs)

(*----------------------------------------------------------------------*)
(* Quicksort an integer list.                                           *)
(* This is the solution to Exercise 3.38 posed in Paulson, ML for the   *)
(* Working Programmer, second edition.                                  *)
(*----------------------------------------------------------------------*)
fun quick xs =
let
  (* Sort xs and append ys to it *)
  fun quicker (xs, ys) =
    case xs of
      [] => ys
    | [x] => x::ys
    | a::bs =>
      let 
        fun partition (left,right,[]) = quicker (left, a::quicker (right, ys))
          | partition (left,right, x::xs) =
            if x <= a then partition (x::left, right, xs)
                      else partition (left, x::right, xs)
      in  
        partition([],[],bs)  
      end
in
  quicker (xs, [])
end

(*----------------------------------------------------------------------*)
(* Random number generation, using Java.         			*)
(*----------------------------------------------------------------------*)
local
  type Random = java.util.Random

  (* Fixed seed to match User Guide demo output *)
  val generator = _new Random (42)
in
  fun random () = generator.#nextInt ()
end

(*----------------------------------------------------------------------*)
(* Create a list of n random integers between 0 and 99. 		*)
(*----------------------------------------------------------------------*)
fun randlist n =
let
  fun gather (n, tail) =
  if n=0 then tail
  else gather (n-1, Int.rem(Int.abs (random ()), 100)::tail)
in
  gather (n,[])
end

(*----------------------------------------------------------------------*)
(* Main function, separated from class for clarity.			*)
(*----------------------------------------------------------------------*)
fun go nitems =
let
  val randints = randlist nitems
in
  print "Before sorting: "; 
  printList randints; 
  print "\n";
  print "After sorting: ";
  printList (quick randints);
  print "\n"
end

val default = 100

fun main (env : string option array option) =
  case env of 
    NONE => 
    go default 

  | SOME env' => 
    if Array.length env' = 0 then go default
    else
      case Array.sub(env', 0) of
        NONE => 
        go default

      | SOME str =>
        case Int.fromString str of
          NONE => go default
        | SOME nitems => go nitems

end
