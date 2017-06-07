package swivel

import scala.annotation.Annotation
import scala.annotation.StaticAnnotation
import scala.annotation.meta.param

/** Mark the root of a heirarchy of AST classes.
  */
final class root extends Annotation with StaticAnnotation

/** Mark an intermediate AST abstract class which is under a root and is extended by leafs.
  */
final class branch extends Annotation with StaticAnnotation

/** Mark a final case class leaf of an AST heirarchy.
  */
final class leaf extends Annotation with StaticAnnotation

/** Specify the class which are statically allowed to replace this class and it's subclasses.
  *
  * Only classes with a superclass with @replacement will have a replace method on their
  * zippers.
  *
  * Additional restrictions are provided at runtime. A subclass of a class with @replacement
  * may not override the replacement type.
  */
final class replacement[ReplacementValue] extends Annotation with StaticAnnotation

/** Mark a class parameter that should not be included in the zipper and instead be treated as a simple value.
  *
  * This annotation overrides type information.
  */
@param
final class notChild extends Annotation with StaticAnnotation
