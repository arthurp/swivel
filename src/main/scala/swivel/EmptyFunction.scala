package swivel
  
object EmptyFunction extends PartialFunction[Any, Nothing] {
  def isDefinedAt(x: Any): Boolean = false
  def apply(x: Any): Nothing = throw new MatchError(x)
}
