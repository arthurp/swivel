package astlib

trait ASTValue {
  type RootValue >: this.type
  type RootZipper <: astlib.Zipper
  type Zipper <: RootZipper

  def toZipper(parent: Option[RootZipper]): Zipper
  def toZipper(): Zipper = toZipper(None)

  def subtrees: Seq[RootValue]
}

trait Zipper {
  zipper =>
  type MatchingASTValue = ASTValue {
    type RootZipper = zipper.RootZipper
    type Zipper <: RootZipper
  }
  type Value <: MatchingASTValue
  type RootValue >: Value <: MatchingASTValue
  type RootZipper >: zipper.type <: astlib.Zipper {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
  }

  protected val _parent: Option[RootZipper]
  def put(v: RootValue): RootZipper

  def value: Value
  def parent: Option[RootZipper] = _parent map { p =>
    p.put(value)
  }
  def root: RootZipper = parent.map(_.root).getOrElse(this)

  def subtrees: Seq[RootZipper]

  protected[this] def parentString: String = _parent.map(p => s"<in ${p.toStringAsParent()}>").getOrElse("<root>")
  protected def toStringAsParent(): String

  /*
  def right: Option[RootZipper]
  def left: Option[RootZipper]
  def child: Option[RootZipper]
  */
}

trait ZipperReplaceable extends Zipper {
  zipper =>
    
  // TODO: For some reason I need to redeclare all the types here to get replace to typecheck. No idea why.
  override type MatchingASTValue = ASTValue {
    type RootZipper = zipper.RootZipper
    type Zipper <: RootZipper
  }
  type Value <: MatchingASTValue
  type RootValue >: Value <: MatchingASTValue
  type RootZipper >: zipper.type <: astlib.Zipper {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
  }

  type ReplacementValue >: Value <: RootValue

  def replace(a: ReplacementValue): a.Zipper = a.toZipper(_parent.map(_.put(a)))
}
