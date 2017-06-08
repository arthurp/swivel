package swivel

import scala.annotation.Annotation
import scala.annotation.StaticAnnotation
import scala.annotation.meta.param
import scala.annotation.compileTimeOnly
import scala.language.experimental.macros

/** Mark the root of a heirarchy of AST classes.
  */
@compileTimeOnly("Swivel requires macro paradise")
final class root extends Annotation with StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro swivel.impl.BasicTreeMacros.root
}

/** Mark an intermediate AST abstract class which is under a root and is extended by leafs.
  */
@compileTimeOnly("Swivel requires macro paradise")
final class branch extends Annotation with StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro swivel.impl.BasicTreeMacros.branch
}

/** Mark a final case class leaf of an AST heirarchy.
  */
@compileTimeOnly("Swivel requires macro paradise")
final class leaf extends Annotation with StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro swivel.impl.BasicTreeMacros.leaf
}

/** Specify the class which are statically allowed to replace this class and it's subclasses.
  *
  * Only classes with a superclass with @replacement will have a replace method on their
  * zippers.
  *
  * Additional restrictions are provided at runtime. A subclass of a class with @replacement
  * may not override the replacement type.
  */
// TODO: @compileTimeOnly("Swivel requires macro paradise")
final class replacement[ReplacementValue] extends Annotation with StaticAnnotation

/** Mark a class parameter that should not be included in the zipper and instead be treated as a simple value.
  *
  * This annotation overrides the type-based default.
  */
// TODO: @compileTimeOnly("Swivel requires macro paradise")
@param
final class notChild extends Annotation with StaticAnnotation
