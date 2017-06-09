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

  object Formula {
    println("test")
  }

  @leaf
  final case class Add(@subtree l: Formula, @subtree r: Formula) extends Formula
  @leaf
  final case class Mul(@subtree factors: Seq[Formula]) extends Formula
  @leaf
  final case class Obj(@subtree fields: Map[Symbol, Argument]) extends Formula
  @leaf
  final case class Negate(@subtree e: Argument) extends Formula {
    override def toString() = s"-$e"
  }
  @leaf
  final case class Let(x: BoundVar, @subtree expr: Option[Formula], @subtree body: Formula) extends Formula

  @branch
  sealed abstract class Argument extends Formula
  @leaf
  final case class Constant(n: Int) extends Argument {
    override def toString() = s"$n"
  }
  @leaf
  final class BoundVar(val name: String) extends Argument {
    override def toString() = s"$name#${hashCode()}"
    def copy(name: String) = new BoundVar(name) 
  }
}
 
object ArithSwivelTest {
  import ArithSwivel._
  def main(args: Array[String]) = {
    {
      val b = new BoundVar("b")
      val f = Let(b, Some(Add(Constant(1), Negate(new BoundVar("v")))), b)
      val z = f.toZipper()
      val LetZ(x, Some(AddZ(_, NegateZ(v))), b1) = z
      
      println(f)
      println(z)
      println(v)
      println(v.replace(Constant(20)))
      println(v.replace(Constant(20)).root)
      
      println(x)
      println(b1)
      println((b, b.toZipper(), b.toZipper().value))
    }   
    {
      val b = new BoundVar("b")
      val f = Let(b, None, b)
      val z = f.toZipper()
      val LetZ(x, o, b1) = z
      
      println(f)
      println(z)
      println(o)
    }   
    
    val f = Mul(Seq(Add(Constant(1), Negate(new BoundVar("a"))), Constant(5)))
    val z: MulZ = f.toZipper()
    println(z.factors(0))
    println(z.factors(1))
    println(z.factors(0).replace(new BoundVar("test")): BoundVarZ)
    println(z.factors(1).replace(new BoundVar("test")).parent.map(_.value))
    println(z.factors.map(_.replace(new BoundVar("test")).root.value).view.force)
    println(z.factors.map(_.replace(new BoundVar("test"))).view.force)
    //println(z.factors.map(_ => BoundVar("test")))
    //println(z.copy(factors = z.factors.map(_ => BoundVar("test"))))
    //println(z.copy(factors = z.factors.map(_ => BoundVar("test")).view.force))
    println(z.copy(factors = z.factors.map(v => v.replace(new BoundVar("test")).value).view.force).root.value)
    val _: Mul = z.copy(factors = z.factors.map(v => v.replace(new BoundVar("test")).value).view.force).value

    val AddZ(_, NegateZ(v)) = z.factors(0)
    println(v : ArgumentZ)
    println(v.root)
    println(v.value)

    val Add(_, Negate(v1)) = f.factors(0)
    println(v1 : Argument)

    println(f.subtrees)
    println(z.subtrees.view.force)
    
    {
      val b = new BoundVar("b")
      val f = Let(b, Some(Obj(scala.collection.immutable.ListMap('b -> b, 'a -> Constant(5)))), b)
      val z = f.toZipper()
      val LetZ(x, Some(o: ObjZ), b1) = z
      
      println(f)
      println(z)
      println(o)
      println(o.fields('a))
      println(o.subtrees)
    }   
  }
}

