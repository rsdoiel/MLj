(*======================================================================*)
(* Threads in MLJ: just to prove that they can be done.			*)
(*======================================================================*)
signature Threads =
sig

type Thread = java.lang.Thread

(* Creates thread and sets it going; uncaught exceptions are lost *)
val new : (Thread -> unit) -> Thread

(* Creates thread with priority as specified *)
val newWith : int -> (Thread -> unit) -> Thread

(* Stop a thread *)
val kill : Thread -> unit

(* Send the current thread to sleep for the specified time in milliseconds *)
val sleep : int -> unit

(* Suspend a thread's execution *)
val suspend : Thread -> unit

(* Resume a thread's execution *)
val resume : Thread -> unit

end