(* Queens.sml 
   Eight Queens application in ML using Java extensions to display
   the solutions.
   Nick Benton
*)

structure Queens = 
struct

fun threat (x,y) (x',y') = (x = x') orelse (y = y') 
                          orelse (x+y = x'+y') orelse (x-y = x'-y')
   
fun conflict pos = List.exists (threat pos)

fun addqueen(i,n,place) =
    let fun tryqueen(j) =
          if conflict (i,j) place then []
          else if i=n then (i,j)::place
          else addqueen(i+1,n,(i,j)::place)
        fun anyqueen(j) =
          let val newplace = tryqueen(j)
           in if not (null newplace) then newplace
              else if j = n then [] else anyqueen(j+1) end
     in anyqueen(1) end

val nqueens = 8
val answer = addqueen(1,nqueens,[])

val xsize = 42
val ysize = 42
fun xtocoords x = x*xsize
fun ytocoords y = y*ysize
val light = _getfield "java.awt.Color" "gray"
val dark  = _getfield "java.awt.Color" "darkGray"

type Event = "java.awt.Event"
type Image = "java.awt.Image" 
type ImageObserver = "java.awt.image.ImageObserver"

_public _classtype Queens _extends "java.awt.Frame" 
                          _implements ImageObserver
{
  _static _field "theImage" : Image option = NONE

  _public _static _method "main" (env : Java.String option Java.array option) =
  ( let val app = _new Queens ("Eight Queens")
        val tk = valOf (_invoke "java.awt.Toolkit" "getDefaultToolkit" ())
    in _putfield Queens "theImage" (_invoke "getImage" (tk, Java.fromString "queen.gif"));
       _invoke "resize" (app, Java.fromInt ((nqueens+2)*xsize), Java.fromInt ((nqueens+2)*ysize));
       _invoke "show" (app)
    end
  )

  _public _method "paint" (g' : "java.awt.Graphics" option) =
  let val g=valOf(g')
      fun drawsquare (x, y, queen) =
         let val xc = Java.fromInt (xtocoords x)
             val yc = Java.fromInt (ytocoords y)
         in  _invoke "setColor" (g, if (x+y) mod 2 = 0 then light else dark);
             _invoke "fillRect" (g, xc, yc,
                            Java.fromInt (xsize-1), Java.fromInt (ysize-1));
             if queen then ignore (
              _invoke "drawImage" (g, _getfield Queens "theImage", xc, yc, _this))
             else ()
         end
      fun drawboard (x,y) =
         if y=0 then ()
         else if x=0 then drawboard (nqueens,y-1)
              else (drawsquare (x,y, List.exists (fn p=> p=(x,y)) answer); 
                    drawboard (x-1,y))
  in drawboard (nqueens,nqueens)
  end

 _public _method "handleEvent" (e : Event option) : Java.boolean = 
    if Java.toInt(_getfield "id" (valOf e)) = 
       Java.toInt(_getfield Event "WINDOW_DESTROY")
    then 
      OS.Process.terminate OS.Process.success
    else 
     _invoke "handleEvent" (_super, e)


 _private _constructor (title : string){_super(Java.fromString title)}

 _public _method "imageUpdate" (i: Image option, a: Java.int, b: Java.int, c:Java.int, d:Java.int, e:Java.int) : Java.boolean = 
   if Java.toInt a = Java.toInt  (_getfield ImageObserver "ALLBITS") 
   then (_invoke "repaint" (_this); Java.fromBool false)
   else Java.fromBool true
 
}
end
