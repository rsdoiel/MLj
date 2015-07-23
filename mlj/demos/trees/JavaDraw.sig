signature JavaDraw =
sig

  val makeInterface : java.awt.Graphics -> string Draw.Interface
  val newTree : java.awt.Graphics -> (string*real) Tree.Tree * Tree.Extent list

end