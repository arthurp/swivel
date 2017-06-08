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

object ArithSwivel {
  @replacement[Formula]
  @root
  sealed abstract class Formula

  //object Formula

  @leaf
  final case class Add(@child l: Formula, @child r: Formula) extends Formula
  //@leaf
  //final case class Mul(factors: Seq[Formula]) extends Formula
  @leaf
  final case class Negate(@child e: Argument) extends Formula {
    override def toString() = s"-$e"
  }
  @leaf
  final case class Let(x: BoundVar, @child expr: Formula, @child body: Formula) extends Formula

  @branch
  sealed abstract class Argument extends Formula
  @leaf
  final case class Constant(n: Int) extends Argument {
    override def toString() = s"$n"
  }
  @leaf
  final case class BoundVar(name: String) extends Argument {
    override def toString() = name
    
  }
}
 
object ArithSwivelTest {
  import ArithSwivel._
  def main(args: Array[String]) = {
    def f = Let(BoundVar("b"), Add(Constant(1), Negate(BoundVar("a"))), BoundVar("b"))
    val z = f.toZipper()
    val LetZ(x, AddZ(_, NegateZ(v)), b) = z
    
    println(f)
    println(z)
    println(v)
    
    println(x)
    println(b)
   
    
    /*
    val f = Mul(Seq(Add(Constant(1), Negate(BoundVar("a"))), Constant(5)))
    val z: MulZ = f.toZipper(None)
    println(z.factors(0))
    println(z.factors(1))
    println(z.factors(0).replace(BoundVar("test")): BoundVarZ)
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
    */
  }
}

