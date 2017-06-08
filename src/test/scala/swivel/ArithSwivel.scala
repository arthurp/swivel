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
  final case class Add(l: Formula, r: Formula) extends Formula
  @leaf
  final case class Mul(factors: Seq[Formula]) extends Formula
  @leaf
  final case class Negate(e: Argument) extends Formula
  @leaf
  final case class Let(@notChild x: BoundVar, expr: Formula, body: Formula) extends Formula

  @branch
  sealed abstract class Argument extends Formula
  @leaf
  final case class Constant(n: Int) extends Argument
  @leaf
  final case class BoundVar(name: String) extends Argument
}

