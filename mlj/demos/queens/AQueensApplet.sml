(* AQueensApplet.sml
   This is the code for animating the stuff computed in AQueens
   In this version, it's delivered as an applet
*)

structure AQueensApplet =
struct

val nqueens = 8

val xsize = 42
val ysize = 42
fun xtocoords x = x*xsize
fun ytocoords y = y*ysize
fun itorcoords (x,y) = (Real.fromInt (xtocoords x), Real.fromInt (ytocoords y))

val light = java.awt.Color.gray
val dark  = java.awt.Color.darkGray

type Event = java.awt.Event
type Image = java.awt.Image
type Thread = java.lang.Thread
type Graphics = java.awt.Graphics
type Applet = java.applet.Applet


structure Seq = ImpSeq

(* A reference cell pointing to the infinite stream of transitions 
   Could tidy this up to remove the ref
*)
val stream = ref AQueens.theend 
val stepsize = 50 (* how many frames in each transition *)
val rstepsize = Real.fromInt stepsize

val currentpositions = ref (map (fn ((x,y),(x',y')) => itorcoords (x,y)) (Seq.hd (!stream)))

val pleasestop = ref false

val theImage = ref NONE : Image option ref
val theAnimator = ref NONE : Threads.Thread option ref

(* move the painting out into a separate function 
   The applet is passed in so it can be an imageobserver
*)
fun dopaint (g: Graphics, applet :Applet) = 
let fun drawsquare (x, y) =
         let val xc = xtocoords x
             val yc = ytocoords y
         in  g.#setColor(if (x+y) mod 2 = 0 then light else dark);
             g.#fillRect(xc, yc, xsize-1, ysize-1)
         end

    fun drawboard (x,y) =
         (if y=0 then ()
         else if x=0 then drawboard (nqueens,y-1)
              else (drawsquare (x,y); 
                    drawboard (x-1,y)))

    fun drawqueens () =
         List.app (fn (xr,yr) =>  
                    ignore (g.#drawImage(!theImage, 
                     Real.floor xr, Real.floor yr, applet))
                  ) (!currentpositions)

  in g.#clearRect(0, 0, 400, 400);
     drawboard (nqueens,nqueens); drawqueens()
  end

 fun animationthread (theApplet : Applet) thethread = 
   let val SOME(offscreen) = theApplet.#createImage(420, 420)
       fun toploop () =
        let val posanddeltas = map (fn ((x,y),(x',y')) => let val (xr,yr) = itorcoords (x,y)
                                                              val (xr',yr') = itorcoords (x',y')
                                                              val dx = (xr'-xr) / rstepsize
                                                              val dy = (yr'-yr) / rstepsize
                                                          in (xr,dx,yr,dy) 
                                                          end
                                   ) (Seq.hd (!stream))

             fun donstep n = if n>stepsize then ()
             else
             (    
              currentpositions := map (fn (xr,dx,yr,dy)=> (xr+dx * (Real.fromInt n), 
                                         yr + dy* (Real.fromInt n))) posanddeltas;
              dopaint (valOf(offscreen.#getGraphics()), theApplet); 
              (valOf(theApplet.#getGraphics())).#drawImage(offscreen, 
                    0, 0, theApplet);
              Threads.sleep 50;
              donstep (n+1)
             )
         in  (donstep 0; 
              stream := Seq.tl (!stream); 
              Threads.sleep 500; 
              if (!pleasestop) then ( Threads.suspend thethread; 
                                     toploop()) else toploop ()
             )
          end
   in (toploop ()) handle _ => ()
   end

_public _classtype Queens _extends Applet
{
  _public _method init () = 
  ( let val db = valOf (this.#getDocumentBase())
    in  theImage := this.#getImage(db, "queen.gif");
        theAnimator := SOME (Threads.new (animationthread(this :> Applet)))
    end
  )
 
  _public _method start () =
     (       pleasestop := false;
             case !theAnimator of SOME(thread) => thread.#resume()
                                | NONE => () (* it's dead, should restart it *) 
     )

  _public _method stop () = (pleasestop := true)

   (* Changed paint method to call ML dopaint function  *)
  _public _method paint (g' : java.awt.Graphics option) = 
    dopaint(valOf(g'), this :> Applet)

 _public _method handleEvent (e : Event option) : bool = 
    if !((valOf e).#id) = java.awt.Event.WINDOW_DESTROY
    then 
      OS.Process.terminate OS.Process.success
    else 
      this.##handleEvent(e)


 _public _constructor (){_super()}

}
end