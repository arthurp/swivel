//
// ArithSwivel.scala -- Scala object ArithSwivel to test basic swivel functions.
// Project swivel
//
// Created by amp on Jun, 2017.
//
// Copyright (c) 2017 The University of Texas at Austin. All rights reserved.
//
// Use and redistribution of this file is governed by the license terms in
// the LICENSE file found in the project's top-level directory and also found at
// URL: http://orc.csres.utexas.edu/license.shtml .
//

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
  trait Transform extends TransformFunction {
    val onFormula: PartialFunction[Formula.Z, Formula] = {
      case a: Argument.Z => onArgument(a)
    }
    val onArgument: PartialFunction[Argument.Z, Argument] = EmptyFunction

    def apply(f: Formula.Z) = transformWith[Formula.Z, Formula](f)(this, onFormula)
    def apply(f: Argument.Z) = transformWith[Argument.Z, Argument](f)(this, onArgument)
  }

  @replacement[Formula]
  @transform[Transform]
  @root
  sealed abstract class Formula

  object Formula {
    println("test Formula")
    val x = 2
  }

  @leaf @transform
  final case class Add(@subtree l: Formula, @subtree r: Formula) extends Formula
  @leaf @transform
  final case class Mul(@subtree factors: Seq[Formula]) extends Formula
  @leaf @transform
  final case class Obj(@subtree fields: Map[Symbol, Argument]) extends Formula
  @leaf @transform
  final case class Negate(@subtree v: Argument) extends Formula {
    override def toString() = s"-$v"
  }
  @leaf @transform
  final case class Let(x: BoundVar, @subtree expr: Option[Seq[Formula]], @subtree body: Formula) extends Formula

  @branch
  sealed abstract class Argument extends Formula
  object Argument {
    println("test Argument")
  }
  @leaf @transform
  final case class Constant(n: Int) extends Argument {
    override def toString() = s"$n"
  }
  @leaf @transform
  final class BoundVar(val name: String) extends Argument {
    override def toString() = s"$name#${hashCode()}"
    def copy(name: String) = new BoundVar(name)
  }
}

object ArithTransform {
  import ArithSwivel._

  /*
  @transform
  class Transform {
    val onFormula: PartialFunction[Formula.Z, Formula]
    val onArgument: PartialFunction[Argument.Z, Argument]
  }
  */

  /*  
  class Transform {
    def apply(a: Argument.Z): Argument.Z = transform(a)
    def apply(e: Formula.Z): Formula.Z = transform(e)
  
    val onFormula: PartialFunction[Formula.Z, Formula] = EmptyFunction
    val onArgument: PartialFunction[Argument.Z, Argument] = EmptyFunction
  
    def transferMetadata(source: Formula, destination: Formula): Unit = ()
    
    def transform(a: Argument.Z): Argument.Z = {
      val pf = onArgument
      if (pf isDefinedAt a) {
        val a1 = pf(a)
        transferMetadata(a.value, a1)
        a.replace(a1)
      } else {
        a
      }
    }
  
    def transform(f: Formula.Z): Formula.Z = {
      val pf = onFormula
      if (pf isDefinedAt f) {
        // Process all children
        val nSubtrees = f.subtrees.si.Ze
        (0 until nSubtrees).foldRight(f) { (root, i) =>
          val current = root.subtrees(i)
          transform(current)
        }
        val f2 = pf(f1)
        transferMetadata(f.value, f1)
        f.replace(f1)
      } else {
        f
      }
    }
    
  }
  */
}

object ArithSwivelTest {
  import ArithSwivel._

  object PrintFunction extends Transform {
    override val onFormula: PartialFunction[Formula.Z, Formula] = {
      case v =>
        println(v)
        v.value
    }
    override val onArgument: PartialFunction[Argument.Z, Argument] = {
      case v =>
        println(v)
        v.value
    }
  }

  case class ReplaceVariable(o: BoundVar, n: Argument) extends Transform {
    override val onArgument: PartialFunction[Argument.Z, Argument] = {
      case Zipper(`o`) => n
    }
  }

  def main(args: Array[String]): Unit = {
    println(Formula.x)

    {
      val b = new BoundVar("b")
      val f = Let(b, Some(Seq(Add(Constant(1), Negate(new BoundVar("v"))))), b)
      val z = f.toZipper()
      val Let.Z(x, Some(Seq(Add.Z(_, Negate.Z(v)))), b1) = z

      val body: Formula.Z = z.body

      z.subtrees

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
      val Let.Z(x, o, b1) = z

      println(f)
      println(z)
      println(o)
    }

    val f = Mul(Seq(Add(Constant(1), Negate(new BoundVar("a"))), Constant(5)))
    val z: Mul.Z = f.toZipper()
    println(z.factors(0))
    println(z.factors(1))
    println(z.factors(0).replace(new BoundVar("test")): BoundVar.Z)
    println(z.factors(1).replace(new BoundVar("test")).parent.map(_.value))
    println(z.factors.map(_.replace(new BoundVar("test")).root.value).view.force)
    println(z.factors.map(_.replace(new BoundVar("test"))).view.force)
    //println(z.factors.map(_ => BoundVar("test")))
    //println(z.copy(factors = z.factors.map(_ => BoundVar("test"))))
    //println(z.copy(factors = z.factors.map(_ => BoundVar("test")).view.force))
    println(z.copy(factors = z.factors.map(v => v.replace(new BoundVar("test")).value).view.force).root.value)
    val _: Mul = z.copy(factors = z.factors.map(v => v.replace(new BoundVar("test")).value).view.force).value

    val Add.Z(_, Negate.Z(v)) = z.factors(0)
    println(v: Argument.Z)
    println(v.root)
    println(v.value)

    val Add(_, Negate(v1)) = f.factors(0)
    println(v1: Argument)

    println(f.subtrees)
    println(z.subtrees.view.force)

    {
      val b = new BoundVar("b")
      val f = Let(b, Some(Seq(Obj(scala.collection.immutable.ListMap('b -> b, 'a -> Constant(5))))), b)
      val z = f.toZipper()
      val Let.Z(x, Some(Seq(o: Obj.Z)), b1) = z

      println(f)
      println(z)
      println(o)
      println(o.fields('a))
      println(o.fields('a).replace(Constant(10)))
      println(o.fields('a).replace(Constant(10)).root.value)
      println(o.subtrees)

      println(ReplaceVariable(b, Constant(100))(z))

      println()
      PrintFunction(z)
    }

    println()
    val f2 = PrintFunction(z)
    println((f == f2, f eq f2))
  }
}

