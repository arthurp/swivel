//
// Zipper.scala -- Scala traits which are superclasses for generated swivel classes. 
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

/** The base class of swivel values.
  */
trait SwivelValue {
  value =>
  type RootValue >: this.type
  type RootZipper <: swivel.Zipper
  /** '''For internal use only'''
   */
  @deprecated("For internal use only", "all")
  type RootZipperParent <: swivel.ZipperParent
  type Zipper <: RootZipper

  /** '''For internal use only'''
   */
  @deprecated("For internal use only", "all")
  def toZipper(parent: Option[RootZipperParent]): Zipper

  /** Get a Zipper for this object.
    *
    * The returned zipper has no parent.
    */
  def toZipper(): Zipper = toZipper(None)

  /** The subtrees (as defined by @subtree) of this value.
    */
  def subtrees: Seq[RootValue]
}

/** The super trait of all Zippers and ZipperParents.
  */
trait ZipperBase {
  zipper =>
  type SelfType <: RootZipper

  type Value <: SwivelValue {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
    //type Zipper = SelfType
  }
  type RootValue >: Value <: SwivelValue {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
    type Zipper <: RootZipper
  }
  type RootZipper <: swivel.Zipper {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  }
  type RootZipperParent <: swivel.ZipperParent {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  }

  /** '''For internal use only'''
   */
  @deprecated("For internal use only", "all")
  val swivel_parent: Option[RootZipperParent]
  
  /** '''For internal use only'''
   */
  @deprecated("For internal use only", "all")
  protected[this] def swivel_parentString: String = swivel_parent.map(p => s"<in ${p.toString()}>").getOrElse("<root>")
}

/** The super trait of all generated Zippers.
  *
  * Generated zippers will also have accessors for zippers of each field of the value.
  */
trait Zipper extends ZipperBase {
  zipper =>
  type RootZipper >: zipper.type <: swivel.Zipper {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  }

  /** The value at this zipper position.
    *
    * This is the inverse of .toZipper()
    */
  val value: Value

  /** The parent of this Zipper or None if this is the root.
   *  
   *  If this throws IllegalArgumentException it means there is a bug in the swivel macro.
    */
  def parent: Option[RootZipper] = swivel_parent map { p =>
    p.swivel_put(value)
  }
  /** The root above the current zipper.
    */
  def root: RootZipper = parent.map(_.root).getOrElse(this)

  /** Zippers for all subtrees of this zipper.
    */
  def subtrees: Seq[RootZipper]

  override def toString(): String = value.toString() + swivel_parentString
  override def hashCode(): Int = value.hashCode() + swivel_parent.hashCode() * 11
  override def equals(o: Any): Boolean = o match {
    case z: swivel.Zipper =>
      z.value == value && z.parent == parent
    case _ =>
      false
  }
}

/** Pattern matching object to extract the value from any Zipper in a pattern.
  */
object Zipper {
  def unapply(z: Zipper): Option[z.Value] =
    if (z == null)
      None
    else
      Some(z.value)
}

/** The super trait of all ZipperParents.
  *
  * These objects should never be directly visible to the client code.
  */
@deprecated("For internal use only", "all")
trait ZipperParent extends ZipperBase {
  zipper =>
  type RootZipperParent >: zipper.type <: swivel.ZipperParent {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  }

  /** '''For internal use only'''
   */
  @deprecated("For internal use only", "all")
  def swivel_put(v: RootValue): RootZipper
  /** '''For internal use only'''
   */
  @deprecated("For internal use only", "all")
  protected[swivel] def swivel_checkSubtree(v: RootValue): Unit
}

/** A mix-in added by @replaceable to introduce a [[#replace]] method.
  */
trait ZipperReplaceable extends Zipper {
  zipper =>
  type RootValue >: Value <: SwivelValue {
    type RootValue = zipper.RootValue
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
    type Zipper <: RootZipper
  }

  type ReplacementValue >: Value {
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  } <: RootValue {
    type RootZipper = zipper.RootZipper
    type RootZipperParent = zipper.RootZipperParent
  }

  /** Replace the value at this zipper with a new one.
    *
    * @throws IllegalArgumentException if the passed replacement doesn't match the parents required subtree time.
    */
  @throws[IllegalArgumentException]
  def replace(a: ReplacementValue): a.Zipper = {
    swivel_parent.foreach(_.swivel_checkSubtree(a))
    a.toZipper(swivel_parent)
  }
}
