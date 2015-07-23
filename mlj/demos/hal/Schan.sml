(* Schan.sml
   This implements a string channel for communication from ML to Java in Hal
*)
structure Schan = 
struct

_public _interfacetype SchanReader {
 _public _abstract _method handlechan (arg: string option)
}

val theReader = ref(NONE) : SchanReader option ref

fun write s = case !theReader of SOME(v) => v.#handlechan (s:string)
                                 | NONE => ()
end