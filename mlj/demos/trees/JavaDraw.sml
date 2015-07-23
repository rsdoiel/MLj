structure JavaDraw :> JavaDraw =
struct


(*----------------------------------------------------------------------*)
(* The Persimmon colour!                                                *)
(*----------------------------------------------------------------------*)
val persimmon = _new java.awt.Color (51, 0, 51)

(*----------------------------------------------------------------------*)
(* From a Java graphics context determine appropriate functions for     *)
(* drawing lines and labels, and necessary dimensions.                  *)
(*----------------------------------------------------------------------*)
fun makeInterface (g : java.awt.Graphics) =
let
  val fm = valOf (g.#getFontMetrics ())
  val maxAscent = fm.#getMaxAscent ()
  val maxDescent = fm.#getMaxDescent ()
  val leading = fm.#getLeading ()
  val maxAdvance = fm.#getMaxAdvance ()
in
  { 
    label = fn (s:string,(x,y)) => 
      let 
        val w = fm.#stringWidth(s)
        val rw = Real.fromInt w
      in          
        g.#setColor(persimmon);
        g.#drawString(s, x - w div 2, y + maxAscent);
        { left = ~rw/2.0, right = rw/2.0, height = maxAscent + leading }
      end,

    line = fn ((x1,y1),(x2,y2)) =>       
     (g.#setColor java.awt.Color.black;
      g.#drawLine(x1,y1,x2,y2)),

    dropabove = maxDescent*2,
    dropbelow = maxDescent*2
  }
end

val generator = Rand.newgen ()

(*----------------------------------------------------------------------*)
(* Create a new random tree and lay it out.                       	*)
(*----------------------------------------------------------------------*)
fun newTree (g : java.awt.Graphics) =
let
  fun f i = 
    let val r = Rand.random generator
    in
      Int.min(Real.floor 
      (1.0 / Math.ln (1.0 + Real.fromInt (i+1) / 1.3 * r)), 4)
    end

  val fm = valOf (g.#getFontMetrics ())
  val maxAscent = fm.#getMaxAscent ()
  val maxDescent = fm.#getMaxDescent ()
  val leading = fm.#getLeading ()
  val maxAdvance = fm.#getMaxAdvance ()
  
  val t = MakeTree.make f
  fun extent (s:string) = 
    let 
      val w = Real.fromInt(fm.#stringWidth(s) + maxAdvance div 2)
      val halfw = w/2.0
    in
      { left = ~halfw, right = halfw, height = maxAscent + leading }
    end
in
  Tree.design (t, extent)
end

end
