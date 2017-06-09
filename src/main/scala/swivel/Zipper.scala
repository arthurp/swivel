package swivel

trait SwivelValue {
  type RootValue >: this.type
  type RootZipper <: swivel.Zipper
  type RootZipperParent <: swivel.ZipperParent
  type Zipper <: RootZipper

  def toZipper(parent: Option[RootZipperParent]): Zipper
  def toZipper(): Zipper = toZipper(None)

  def subtrees: Seq[RootValue]
}

trait ZipperBase {
  zipper =>
  type Value <: SwivelValue {
    type RootZipper = zipper.RootZipper
    type Zipper <: RootZipper
  }
  type RootValue >: Value <: SwivelValue {
    type RootZipper = zipper.RootZipper
    type Zipper <: RootZipper
  }
  type RootZipper <: swivel.Zipper {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  }
  type RootZipperParent <: swivel.ZipperParent {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  }
  
  protected val swivel_parent: Option[RootZipperParent]
  protected[this] def swivel_parentString: String = swivel_parent.map(p => s"<in ${p.toString()}>").getOrElse("<root>")
}

trait Zipper extends ZipperBase {
  zipper =>
  type RootZipper >: zipper.type <: swivel.Zipper {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  }

  val value: Value
  
  def parent: Option[RootZipper] = swivel_parent map { p =>
    p.swivel_put(value)
  }
  def root: RootZipper = parent.map(_.root).getOrElse(this)

  def subtrees: Seq[RootZipper]
  
  override def toString(): String = value.toString() + swivel_parentString
  
  override def hashCode(): Int = value.hashCode() + swivel_parent.hashCode() * 11
  override def equals(o: Any): Boolean = o match {
    case z: Zipper =>
      z.value == value && z.parent == parent
    case _ =>
      false
  }

  /*
  def right: Option[RootZipper]
  def left: Option[RootZipper]
  def child: Option[RootZipper]
  */
}

trait ZipperParent extends ZipperBase {
  zipper =>
  type RootZipperParent >: zipper.type <: swivel.ZipperParent {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  }
  
  def swivel_put(v: RootValue): RootZipper
  protected[swivel] def swivel_checkSubtree(v: RootValue): Unit
}

trait ZipperReplaceable extends Zipper {
  zipper =>
  type RootValue >: Value <: SwivelValue {
    type RootZipper = zipper.RootZipper
    type Zipper <: RootZipper
  }
    
  type ReplacementValue >: Value {
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  } <: RootValue {
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  }

  def replace(a: ReplacementValue): a.Zipper = {
    swivel_parent.foreach(_.swivel_checkSubtree(a))
    a.toZipper(swivel_parent)
  }
}
