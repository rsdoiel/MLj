structure Threads :> Threads =
struct

type Runnable = java.lang.Runnable
type Thread = java.lang.Thread

_classtype MyRunnable _implements Runnable
{
  _private _final _field function : Thread -> unit

  _public _method run () = 
  let
    val f = this.#function
  in
    (f(valOf(java.lang.Thread.currentThread()))) handle _ => ()
  end

  _public _constructor (f : Thread -> unit) {_super(); function = f }
}

fun new f = 
let
  val t = _new Thread(_new MyRunnable f)
in
  t.#start ();
  t
end

fun newWith p f = 
let
  val t = _new Thread(_new MyRunnable f)
in
  t.#setPriority(p:int);
  t.#start ();
  t
end

fun kill (t : Thread) = t.#stop ()

fun sleep (millis : int) = java.lang.Thread.sleep (millis)

fun suspend (t : Thread) = t.#suspend()
fun resume (t : Thread) = t.#resume()

end
