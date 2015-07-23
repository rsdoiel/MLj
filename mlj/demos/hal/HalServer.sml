(* HalServer.sml
   This is the top level structure for making a class which is called
   by the Java-written top-level terminal window to do Hal stuff

*)

structure HalServer =
struct

open Command;
open Tactical;

fun showResult () = 
    if Rule.final(getState()) then DisplayFol.form(Rule.main (getState()))
    else raise Fail "Unsolved subgoals exist!";

fun checkFailed() =
    if Rule.final(getState()) then raise Fail "Should have failed!"
    else "Failed, as expected...";

_public _classtype HalServer  {
 _public _static _synchronized _method processline (arg : string option) : Threads.Thread option =
 SOME (
   Threads.new (fn _ => (ServerInterpret.toplevel (valOf arg)))
  )

 _public _static _synchronized _method registerReader (aReader : Schan.SchanReader option) =
   (Schan.theReader := aReader)

}
end
