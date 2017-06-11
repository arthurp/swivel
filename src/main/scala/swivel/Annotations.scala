//
// Annotations.scala -- Scala annotations for macro processing
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
@compileTimeOnly("@replacement must be applied to a class also annotated with @root, @branch, or @leaf")
final class replacement[ReplacementValue] extends Annotation with StaticAnnotation

/** Specify that the class should allow transformation of it's subtrees.
  *
  * @tparam TransformFunction
  * ''(Only specified when @transform is applied to a root or branch)'' must be a subclass of
  * TransformFunction with methods named onX for each class X which appears as a parameter type in a leaf.
  *
  * The TransformFunction should be defined something like this:
  * {{{
  *   class Transform extends TransformFunction {
  *     val onExpression: PartialFunction[ExpressionZ, Expression] = EmptyFunction
  *     def apply(e: ExpressionZ) = transformWith[ExpressionZ, Expression](e)(this, onExpression)
  *   }
  * }}}
  */
@compileTimeOnly("@transform must be applied to a class also annotated with @root, @branch, or @leaf")
final class transform[TransformFunction] extends Annotation with StaticAnnotation

/** Mark a class parameter as a child of this node in the tree.
  *
  * Children appear as zipper values and can be traversed and extracted.
  * Non-children are treated as primitive values and are simply passed through without any zipper handling.
  * This applies to both unapply and accessors.
  */
@param
final class subtree extends Annotation with StaticAnnotation

