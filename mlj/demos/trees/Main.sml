structure Main =
struct

(*......................................................................*)
(* For standalone use we provide a main method.				*)
(*......................................................................*)
fun main (env : string option array option) = 
  let
    val applet = _new Applet.TreeApplet ()
    val frame =  _new Applet.AppletFrame("Trees", applet, 600, 300)
  in
    ()
  end

end
