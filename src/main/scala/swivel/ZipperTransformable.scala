package swivel

trait ZipperTransformable extends Zipper {
  type TranformFunction <: swivel.TransformFunction

  def transformChildren(f: TranformFunction): Value
}

trait TransformFunction {
  def transformWith[Z <: ZipperTransformable, V >: Z#Value](z: Z)(tf: z.TranformFunction, f: PartialFunction[Z, V]): V = {
    val v1 = z.transformChildren(tf)
    val z1: Z = v1.toZipper(z.swivel_parent).asInstanceOf[Z]
    if (f.isDefinedAt(z1)) {
      f(z1)
    } else {
      z1.value
    }
  }
}
