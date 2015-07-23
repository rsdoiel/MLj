structure Applet =
struct

val currenttree = 
  ref (NONE : ((string*real) Tree.Tree * Tree.Extent list) option)

_public _classtype AppletFrame _extends java.awt.Frame 
{
  _public _constructor (title : string, applet : java.applet.Applet, 
    width : int, height : int) { _super(title) } = 
  (
    this.#add("Center", applet);
    this.#resize(width, height);
    this.#show()
  )

  _public _method "handleEvent" (eopt : java.awt.Event option) : bool =  
  let
    val e = valOf(eopt)
  in
    if ! (e.#id) = java.awt.Event.WINDOW_DESTROY
    then (OS.Process.terminate OS.Process.success; true)
    else this.##handleEvent(e)
  end
}

_public _classtype TreeApplet _extends java.applet.Applet
{
  _public _method "paint" (gopt : java.awt.Graphics option) = 
  case gopt of
    NONE => ()
  | SOME g =>
    let
      (* currenttree should have been set by "ready" *)
      val (tree, extent) = valOf (!currenttree)
      val {left,right,height} = Tree.collapseExtent extent
      val interface = JavaDraw.makeInterface g
    in
      ignore(Draw.draw2 interface (~left, 0, extent) tree)
    end

  _public _method "handleEvent" (eopt : java.awt.Event option) : bool = 
  let
    val e = valOf eopt
  in
    if ! (e.#id) = java.awt.Event.MOUSE_UP
    then
    let 
      val graphics = valOf (this.#getGraphics ())
      val (tree, extent) = JavaDraw.newTree(graphics)
    in
      currenttree := SOME (tree, extent);
      this.#repaint ();
      true
    end 
    else this.##handleEvent(e)
  end

  _public _constructor () { _super() }

  _public _method "addNotify" () = 
  (this.##addNotify();
   currenttree := SOME (JavaDraw.newTree (valOf(this.#getGraphics()))))
}


end
