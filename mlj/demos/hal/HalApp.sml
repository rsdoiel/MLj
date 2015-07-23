(* HalApp.sml
   This is the top level structure for making the Hal application.

*)

structure HalApp =
struct

open Command;
open Tactical;

fun showResult () = 
    if Rule.final(getState()) then DisplayFol.form(Rule.main (getState()))
    else raise Fail "Unsolved subgoals exist!";

fun checkFailed() =
    if Rule.final(getState()) then raise Fail "Should have failed!"
    else "Failed, as expected...";

_public _classtype HalApp =  "HalApp" {
 _public _static "main"(args : Java.String Java.array) =
  (
   print "Hal\n";
   Interpret.toplevel ()
  )

}
end
