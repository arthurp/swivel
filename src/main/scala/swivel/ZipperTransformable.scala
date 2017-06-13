//
// ZipperTransformable.scala -- Scala traits used by @transform 
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

/** A mix-in added by @transform to introduce a [[#transformChildren]] method.
  */
trait ZipperTransformable extends Zipper {
  type TranformFunction <: swivel.TransformFunction

  /** Apply f to every child of this [[swivel.Zipper]], but not to this Zipper itself.
    *
    * To transform a zipper directly you will need some boilerplate: See @[[swivel.transform]].
    */
  def transformChildren(f: TranformFunction): Value
}

/** The super trait of all TranformFunctions passed to @[[swivel.transform]].
  */
trait TransformFunction {
  /** Transform the given zipper using the given partial function.
    *
    * This transforms both the provided z and it's subtrees (recursively). This is designed
    * to be used from subclasses to implement easy to use apply methods. See @[[swivel.transform]].
    */
  def transformWith[Z <: ZipperTransformable, V >: Z#Value](z: Z)(tf: z.TranformFunction, f: PartialFunction[Z, V]): V = {
    val v1 = z.transformChildren(tf)
    val z1: Z = v1.toZipper(z.swivel_parent).asInstanceOf[Z]
    if (f.isDefinedAt(z1)) {
      f(z1)
    } else {
      z1.value
    }
  }
}
