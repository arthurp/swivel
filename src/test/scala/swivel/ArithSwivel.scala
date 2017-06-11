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
    val onFormula: PartialFunction[FormulaZ, Formula] = {
      case a: ArgumentZ => onArgument(a)
    }
    val onArgument: PartialFunction[ArgumentZ, Argument] = EmptyFunction
    
    def apply(f: FormulaZ) = transformWith[FormulaZ, Formula](f)(this, onFormula)
    def apply(f: ArgumentZ) = transformWith[ArgumentZ, Argument](f)(this, onArgument)
  }

  @replacement[Formula]
  @transform[Transform]
  @root
  sealed abstract class Formula

  object Formula {
    println("test")
  }

  @leaf @transform
  final case class Add(@subtree l: Formula, @subtree r: Formula) extends Formula
  @leaf @transform
  final case class Mul(@subtree factors: Seq[Formula]) extends Formula
  @leaf @transform
  final case class Obj(@subtree fields: Map[Symbol, Argument]) extends Formula  
  @leaf @transform
  final case class Negate(@subtree e: Argument) extends Formula {
    override def toString() = s"-$e"
  }
  @leaf @transform
  final case class Let(x: BoundVar, @subtree expr: Option[Formula], @subtree body: Formula) extends Formula
  
  @branch
  sealed abstract class Argument extends Formula
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
    val onFormula: PartialFunction[FormulaZ, Formula]
    val onArgument: PartialFunction[ArgumentZ, Argument]
  }
  */

/*  
  class Transform {
    def apply(a: ArgumentZ): ArgumentZ = transform(a)
    def apply(e: FormulaZ): FormulaZ = transform(e)
  
    val onFormula: PartialFunction[FormulaZ, Formula] = EmptyFunction
    val onArgument: PartialFunction[ArgumentZ, Argument] = EmptyFunction
  
    def transferMetadata(source: Formula, destination: Formula): Unit = ()
    
    def transform(a: ArgumentZ): ArgumentZ = {
      val pf = onArgument
      if (pf isDefinedAt a) {
        val a1 = pf(a)
        transferMetadata(a.value, a1)
        a.replace(a1)
      } else {
        a
      }
    }
  
    def transform(f: FormulaZ): FormulaZ = {
      val pf = onFormula
      if (pf isDefinedAt f) {
        // Process all children
        val nSubtrees = f.subtrees.size
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
    override val onFormula: PartialFunction[FormulaZ, Formula] = {
      case v =>
        println(v)
        v.value
    }
    override val onArgument: PartialFunction[ArgumentZ, Argument] =  {
      case v =>
        println(v)
        v.value
    }
  }
  
  case class ReplaceVariable(o: BoundVar, n: Argument) extends Transform {
    override val onArgument: PartialFunction[ArgumentZ, Argument] =  {
      case Zipper(`o`) => n
    }
  }
  
  def main(args: Array[String]): Unit = {
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

