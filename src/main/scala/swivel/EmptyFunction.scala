//
// EmptyFunction.scala -- Scala EmptyFunction object
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

/** A partial function which is totally undefined.
  */
object EmptyFunction extends PartialFunction[Any, Nothing] {
  def isDefinedAt(x: Any): Boolean = false
  def apply(x: Any): Nothing = throw new MatchError(x)
}
