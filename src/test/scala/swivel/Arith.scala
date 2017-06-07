package swivel

/*
 * formula := add | negate | constant | let | variable
 * add := formula + formula
 * negate := - variable
 * constant := Int(n)
 * let := let variable = formula in formula
 * variable := boundvar | unboundvar
 * boundvar := x_1 ...
 * unboundvar := ub_1 ...
 */

object FormulaInput {
  @replacement[Formula]
  @root
  sealed trait Formula

  @leaf
  final case class Add(l: Formula, r: Formula) extends Formula
  @leaf
  final case class Mul(factors: Seq[Formula]) extends Formula
  @leaf
  final case class Negate(e: Argument) extends Formula
  @leaf
  final case class Let(@notChild x: BoundVar, value: Formula, body: Formula) extends Formula

  @branch
  sealed trait Argument extends Formula
  @leaf
  final case class Constant(n: Int) extends Argument
  @leaf
  final case class BoundVar(name: String) extends Argument
}

object Formula {

  // Main

  sealed trait Formula extends ASTValue {
    type RootZipper = FormulaZ
    type Zipper <: FormulaZ
    type RootValue = Formula

    def toZipper(parent: Option[FormulaZ]): Zipper
  }

  final case class Add(l: Formula, r: Formula) extends Formula {
    type Zipper = AddZ

    def toZipper(parent: Option[FormulaZ]): AddZ = new AddZ_L(l, r, parent)
    def subtrees = Seq(l, r)

    override def toString() = s"$l + $r"
  }
  final case class Mul(factors: Seq[Formula]) extends Formula {
    type Zipper = MulZ

    def toZipper(parent: Option[FormulaZ]): MulZ = new MulZ(factors, 0, parent)
    def subtrees = factors
  }
  final case class Negate(e: Argument) extends Formula {
    type Zipper = NegateZ

    def toZipper(parent: Option[FormulaZ]): NegateZ = new NegateZ(e, parent)
    def subtrees = Seq(e)

    override def toString() = s"-$e"
  }

  sealed trait Argument extends Formula {
    type Zipper <: ArgumentZ
  }
  final case class Constant(n: Int) extends Argument {
    type Zipper = ConstantZ

    def toZipper(parent: Option[FormulaZ]): ConstantZ = new ConstantZ(n, parent)
    def subtrees = Seq()
  }
  final case class BoundVar(name: String) extends Argument {
    type Zipper = BoundVarZ

    def toZipper(parent: Option[FormulaZ]): BoundVarZ = new BoundVarZ(name, parent)
    def subtrees = Seq()
  }

  // Zippers

  sealed trait FormulaZ extends Zipper with ZipperReplaceable {
    type ReplacementValue = Formula

    type RootZipper = FormulaZ
    type Value <: Formula

    type RootValue = Formula

    /*
    def transform(f: (FormulaZ) => FormulaZ): FormulaZ = {     
      val afterChild = child.map(_.transform(f)).getOrElse(this)
      val afterRight = afterChild.right.flatMap(_.transform(f).left).getOrElse(afterChild)
      afterRight.parent.getOrElse(afterRight)
    }
    */
  }

  sealed abstract class AddZ(_l: Formula, _r: Formula) extends FormulaZ {
    type Value = Add

    def l: FormulaZ = _l.toZipper(Some(new AddZ_L(_l, _r, _parent)))
    def r: FormulaZ = _r.toZipper(Some(new AddZ_R(_l, _r, _parent)))

    def copy(l: Formula = _l, r: Formula = _r): AddZ = replace(Add(l, r))
    def value: Value = Add(_l, _r)
    def subtrees = Seq(l, r)

    override def toString() = s"AddZ(${_l}, ${_r})${parentString}"
  }
  final class AddZ_L(_l: Formula, _r: Formula, protected val _parent: Option[FormulaZ]) extends AddZ(_l, _r) {
    def put(v: Formula): AddZ = new AddZ_L(v, _r, _parent)
    protected def toStringAsParent() = s"AddZ([], ${_r})${parentString}"
  }
  final class AddZ_R(_l: Formula, _r: Formula, protected val _parent: Option[FormulaZ]) extends AddZ(_l, _r) {
    def put(v: Formula): AddZ = new AddZ_R(_l, v, _parent)
    protected def toStringAsParent() = s"AddZ(${_l}, [])${parentString}"
  }
  object AddZ {
    def unapply(v: AddZ): Option[(FormulaZ, FormulaZ)] = Some((v.l, v.r))
  }

  final class MulZ(_factors: Seq[Formula], _index: Int, protected val _parent: Option[FormulaZ]) extends FormulaZ {
    type Value = Mul

    val factors: Seq[FormulaZ] = _factors.view.zipWithIndex.map({ case (f, i) =>
      //println(s"$f $i")
      f.toZipper(Some(new MulZ(_factors, i, _parent)))
    })

    def put(v: Formula): MulZ = new MulZ(_factors.updated(_index, v), _index, _parent)

    def copy(factors: Seq[Formula] = _factors): MulZ = replace(Mul(factors))
    def value: Value = Mul(_factors)
    def subtrees = factors

    protected def toStringAsParent() = s"MulZ(${_factors.map(_.toString()).updated(_index, "[]")})${parentString}"
    override def toString() = s"MulZ(${_factors.map(_.toString())})${parentString}"
  }

  final class NegateZ(_e: Argument, protected val _parent: Option[FormulaZ]) extends FormulaZ {
    type Value = Negate

    def e: ArgumentZ = _e.toZipper(Some(this))

    def put(v: Formula): NegateZ = new NegateZ(ArgumentZ.cast(v), _parent)

    def copy(e: Argument = _e): NegateZ = replace(Negate(e))
    def value: Value = Negate(_e)
    def subtrees = Seq(e)

    protected def toStringAsParent() = s"NegateZ([])${parentString}"
    override def toString() = s"NegateZ(${_e})${parentString}"
  }
  object NegateZ {
    def unapply(v: NegateZ): Option[ArgumentZ] = Some(v.e)
  }

  sealed trait ArgumentZ extends FormulaZ {
    type Value <: Argument
  }

  object ArgumentZ {
    @inline
    def cast(v: Formula): Argument = v match {
      case a: Argument => a
      case _ =>
        throw new Error(s"$v provided where Argument expected. This should never happen and is a macro expansion bug.")
    }
  }

  final class ConstantZ(val n: Int, protected val _parent: Option[FormulaZ]) extends ArgumentZ {
    type Value = Constant

    def value: Value = Constant(n)
    def put(v: Formula): ConstantZ = new ConstantZ(n, _parent)
    def subtrees = Seq()

    protected def toStringAsParent() = toString()
    override def toString() = s"ConstantZ(${n})${parentString}"
  }
  object ConstantZ {
    def unapply(v: ConstantZ): Option[Int] = Some(v.n)
  }

  final class BoundVarZ(val name: String, protected val _parent: Option[FormulaZ]) extends ArgumentZ {
    type Value = BoundVar

    def value: Value = BoundVar(name)
    def put(v: Formula): BoundVarZ = new BoundVarZ(name, _parent)
    def subtrees = Seq()

    protected def toStringAsParent() = toString()
    override def toString() = s"BoundVarZ(${name})${parentString}"
  }
  object BoundVarZ {
    def unapply(v: BoundVarZ): Option[String] = Some(v.name)
  }

  def main(args: Array[String]) = {
    val f = Mul(Seq(Add(Constant(1), Negate(BoundVar("a"))), Constant(5)))
    val z: MulZ = f.toZipper(None)
    println(z.factors(0))
    println(z.factors(1))
    println(z.factors(0).replace(BoundVar("test")) : BoundVarZ)
    println(z.factors(1).replace(BoundVar("test")).parent.map(_.value))
    println(z.factors.map(_.replace(BoundVar("test")).root.value).view.force)
    println(z.factors.map(_.replace(BoundVar("test"))).view.force)
    //println(z.factors.map(_ => BoundVar("test")))
    //println(z.copy(factors = z.factors.map(_ => BoundVar("test"))))
    //println(z.copy(factors = z.factors.map(_ => BoundVar("test")).view.force))
    println(z.copy(factors = z.factors.map(v => v.replace(BoundVar("test")).value).view.force).root.value)
    val _: Mul = z.copy(factors = z.factors.map(v => v.replace(BoundVar("test")).value).view.force).value

    val AddZ(_, NegateZ(v)) = z.factors(0)
    println(v)
    println(v.root)
    println(v.value)

    val Add(_, Negate(v1)) = f.factors(0)
    println(v1)

    println(f.subtrees)
    println(z.subtrees.view.force)
  }
}

/*
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/*
 * formula := add | negate | constant | let | variable
 * add := formula + formula
 * negate := - variable
 * constant := Int(n)
 * let := let variable = formula in formula
 * variable := boundvar | unboundvar
 * boundvar := x_1 ...
 * unboundvar := ub_1 ...
 */

/*
 * trait Formula
 * case Add(l: Formula, r: Formula) extends Formula
 * case Negate(e: Variable) extends Formula
 * case Constant(n: Int) extends Formula
 * case Let(x: BoundVar, value: Formula, body: Formula) extends Formula
 * class Variable(name: Option[String]) extends Formula
 * class BoundVar(name: Option[String]) extends Variable
 * class UnboundVar(name: String) extends Variable
 */

trait Formula {
  def transform[T <: Formula : TypeTag : ClassTag, U <: Formula : TypeTag](f: WithContext[T] => U, ctx: List[Binding] = List()): Formula

  def in(ctx: List[Binding]): WithContext[this.type] = {
    WithContext(this, ctx)
  }
}

case class Add(l: Formula, r: Formula) extends Formula {
  def transform[T <: Formula : TypeTag : ClassTag, U <: Formula : TypeTag](f: WithContext[T] => U, ctx: List[Binding]): Formula = {
    Add(l.transform(f, ctx), r.transform(f, ctx)) match {
      case t: T => f(t in ctx)
      case t => t
    }
  }
}
object Add {
  def unapply(v: WithContext[Add]) = Some(v.l in v.ctx, v.r in v.ctx)
  //def unapply(v: Add) = Some(v.l, v.r)
}

case class Negate(e: Variable) extends Formula {
  def transform[T <: Formula : TypeTag : ClassTag, U <: Formula : TypeTag](f: WithContext[T] => U, ctx: List[Binding]): Formula = {
    val newe = if (implicitly[TypeTag[U]].tpe <:< implicitly[TypeTag[Variable]].tpe)
      e.transform(f, ctx).asInstanceOf[Variable]
    else
      e
    Negate(newe) match {
      case t: T => f(t in ctx)
      case t => t
    }
  }
}
case class Constant(n: Int) extends Formula {
  def transform[T <: Formula : TypeTag : ClassTag, U <: Formula : TypeTag](f: WithContext[T] => U, ctx: List[Binding]): Formula = {
    this match {
      case t: T => f(t in ctx)
      case t => t
    }
  }
}
object Constant {
  def unapply(v: WithContext[Formula]): Option[Int] = v match {
    case (c: Constant) in _ => Some(c.n)
  }
}

case class Let(x: BoundVar, value: Formula, body: Formula) extends Formula {
  def transform[T <: Formula : TypeTag : ClassTag, U <: Formula : TypeTag](f: WithContext[T] => U, ctx: List[Binding]): Formula = {
    Let(x, value.transform(f, ctx :+ Binding.Recursive(this in ctx)),
        body.transform(f, ctx :+ Binding.Scope(this in ctx))) match {
      case t: T => f(t in ctx)
      case t => t
    }
  }
}
object Let {
  def unapply(v: WithContext[Formula]) = v match {
    case (c: Let) in ctx => Some(c.x in ctx, c.value in ctx :+ Binding.Recursive(v), c.body in ctx :+ Binding.Scope(v))
  }
}

trait Variable extends Formula

class BoundVar(val name: Option[String]) extends Variable {
  def transform[T <: Formula : TypeTag : ClassTag, U <: Formula : TypeTag](f: WithContext[T] => U, ctx: List[Binding]): Formula = {
    this match {
      case t: T => f(t in ctx)
      case t => t
    }
  }
}
object BoundVar {
  def unapply(v: WithContext[BoundVar]) = v match {
    case (c: BoundVar) in ctx => Some(c.name)
  }
}

class UnboundVar(val name: String) extends Variable {
  def transform[T <: Formula : TypeTag : ClassTag, U <: Formula : TypeTag](f: WithContext[T] => U, ctx: List[Binding]): Formula = {
    this match {
      case t: T => f(t in ctx)
      case t => t
    }
  }
}

case class WithContext[+T <: Formula : ClassTag](e: T, ctx: List[Binding])

object WithContext {
  implicit def removeWithContext[T <: Formula](v: WithContext[T]): T = v.e
}

sealed trait Binding {
  def expr: WithContext[Formula]
}

object Binding {
  case class Scope(expr: WithContext[Formula]) extends Binding
  case class Recursive(expr: WithContext[Formula]) extends Binding
}

object in {
  def unapply[T <: Formula](v: WithContext[T]) = Some((v.e, v.ctx))
}
*/
