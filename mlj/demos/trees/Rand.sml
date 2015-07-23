(*======================================================================*)
(* Random number generator based on Java.				*)
(*======================================================================*)
structure Rand :> Rand =
struct

type generator = java.util.Random

fun newgen () = _new generator ()

fun newgenseed i = _new generator (FixedInt.fromInt i)

fun random (g : generator) = g.#nextDouble ()

end
