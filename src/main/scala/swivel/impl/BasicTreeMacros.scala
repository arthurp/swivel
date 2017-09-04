//
// BasicTreeMacros.scala -- Scala macro implementation BasicTreeMacros
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

package swivel.impl

import scala.reflect.macros.whitebox.Context
import scala.language.existentials

object BasicTreeMacros {
  class CompileErrorReported() extends Exception
}

class BasicTreeMacros(val c: Context) {
  import BasicTreeMacros._
  import c.universe._

  object Builtins {
    val subtree = c.typecheck(tq"_root_.swivel.subtree", c.TYPEmode)
    val subtreeSym = subtree.symbol
    val replacement = c.typecheck(tq"_root_.swivel.replacement", c.TYPEmode)
    val replacementSym = replacement.symbol
    val transform = c.typecheck(tq"_root_.swivel.transform", c.TYPEmode)
    val transformSym = transform.symbol
    val Zipper = c.typecheck(tq"_root_.swivel.Zipper", c.TYPEmode)
    val ZipperSym = Zipper.symbol
    val ZipperParent = c.typecheck(tq"_root_.swivel.ZipperParent", c.TYPEmode)
    val ZipperParentSym = ZipperParent.symbol
    val ZipperReplaceable = c.typecheck(tq"_root_.swivel.ZipperReplaceable", c.TYPEmode)
    val ZipperReplaceableSym = ZipperReplaceable.symbol
    val ZipperTransformable = c.typecheck(tq"_root_.swivel.ZipperTransformable", c.TYPEmode)
    val ZipperTransformableSym = ZipperTransformable.symbol
    val SwivelValue = c.typecheck(tq"_root_.swivel.SwivelValue", c.TYPEmode)
    val SwivelValueSym = SwivelValue.symbol
  }

  def extractExpandees(inputs: List[Tree]) = {
    val (annottee, expandees) = inputs match {
      case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
      case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
      case _ => (EmptyTree, inputs)
    }
    q"..$expandees"
  }

  def extractClassAndObject(macroName: String, annottees: Seq[Tree]) = {
    val (inCls, inObj) = annottees.toList match {
      case List(c: ClassDef, o: ModuleDef) =>
        (c, Some(o))
      case List(c: ClassDef) =>
        (c, None)
      case _ =>
        c.error(c.enclosingPosition, s"@$macroName may only be applied to classes.")
        throw new CompileErrorReported()
    }
    (inCls, inObj)
  }

  def extractStandardCompanionDefs(defs: Seq[Tree]): (Option[ClassDef], Option[ModuleDef], Seq[Tree]) = {
    var zCls: Option[ClassDef] = None
    var zObj: Option[ModuleDef] = None
    var rest: Seq[Tree] = Seq()
    defs foreach {
      case d @ q"$mods class ${ `zipperName` } extends ..${ supers } { ..${ defs } }" if zCls.isEmpty =>
        zCls = Some(d.asInstanceOf[ClassDef])
      case d @ q"$mods object ${ `zipperNameTerm` } extends ..${ supers } { ..${ defs } }" if zObj.isEmpty =>
        zObj = Some(d.asInstanceOf[ModuleDef])
      case d =>
        rest :+= d
    }
    (zCls, zObj, rest)
  }

  val emptyClassResults = (Modifiers(), TypeName(""), Seq(), Seq())

  def decomposeClassDef(d: Option[ClassDef]): Option[(Modifiers, TypeName, Seq[Tree], Seq[Tree])] = {
    d.flatMap(decomposeClassDef)
  }

  def decomposeClassDef(d: ClassDef): Option[(Modifiers, TypeName, Seq[Tree], Seq[Tree])] = {
    d match {
      case q"$mods class ${ name: TypeName } extends ..${ supers: Seq[Tree] } { ..${ defs: Seq[Tree] } }" =>
        Some((mods, name, supers, defs))
      case _ =>
        None
    }
  }
  
  val emptyModuleResults = (Modifiers(), TermName(""), Seq(), Seq())

  def decomposeModuleDef(d: Option[ModuleDef]): Option[(Modifiers, TermName, Seq[Tree], Seq[Tree])] = {
    d.flatMap(decomposeModuleDef)
  }

  def decomposeModuleDef(d: ModuleDef): Option[(Modifiers, TermName, Seq[Tree], Seq[Tree])] = {
    d match {
      case q"$mods object ${ name: TermName } extends ..${ supers: Seq[Tree] } { ..${ defs: Seq[Tree] } }" =>
        Some((mods, name, supers, defs))
      case _ =>
        None
    }
  }
  
  def mergeSupers(s1: Seq[Tree], s2: Seq[Tree]) = {
    // Drop 1 value since it will be AnyRef
    s1 ++ s2.drop(1).filterNot(s => s1.contains(s))
  }
  
  def mergeWithOption(d1: ModuleDef, d2o: Option[ModuleDef]) = {
    (d1, d2o) match {
      case (q"$mods1 object ${ name1: TermName } extends ..${ supers1: Seq[Tree] } { ..${ defs1: Seq[Tree] } }", 
          Some(q"$mods2 object ${ name2: TermName } extends ..${ supers2: Seq[Tree] } { ..${ defs2: Seq[Tree] } }")) =>
        require(name1 == name2)
        val mods = Modifiers(mods1.flags | mods2.flags, mods1.privateWithin, mods1.annotations ++ mods2.annotations)
        q"$mods object ${ name1 } extends ..${ mergeSupers(supers1, supers2) } { ..${ defs1 ++ defs2 } }"
      case (_, None) =>
        d1
    }
  }

  def mergeWithOption(d1: ClassDef, d2o: Option[ClassDef]) = {
    (d1, d2o) match {
      case (q"$mods1 class ${ name1: TypeName }(..$args1) extends ..${ supers1: Seq[Tree] } { ..${ defs1: Seq[Tree] } }", 
          Some(q"$mods2 class ${ name2: TypeName }(..$args2) extends ..${ supers2: Seq[Tree] } { ..${ defs2: Seq[Tree] } }")) =>
        require(name1 == name2)
        require(args1.isEmpty || args2.isEmpty)
        val mods = Modifiers(mods1.flags | mods2.flags, mods1.privateWithin, mods1.annotations ++ mods2.annotations)
        q"$mods class ${ name1 }(..${args1 ++ args2}) extends ..${ mergeSupers(supers1, supers2) } { ..${ defs1 ++ defs2 } }"
      case (_, None) =>
        d1
    }
  }

  def extractSwivelDirectSuperClassName(supers: Seq[Tree]): TypeName = supers.headOption match {
    case Some(Ident(n @ TypeName(_))) => n
    case n =>
      c.error(n.map(_.pos).getOrElse(c.enclosingPosition), "The class annotated with @branch must have the swivel class as the first superclass.")
      throw new CompileErrorReported()
  }

  def extractDefinition(arg: Tree): ValDef = arg match {
    case d: ValDef =>
      d
    case _ =>
      c.error(arg.pos, s"Must be a simple definition.")
      throw new CompileErrorReported()
  }

  def isSeqRef(d: Tree): Boolean = {
    d match {
      case tq"$prefix.Seq" =>
        true
      case tq"Seq" =>
        true
      case _ =>
        false
    }
  }

  def isMapRef(d: Tree): Boolean = {
    d match {
      case tq"$prefix.Map" =>
        true
      case tq"Map" =>
        true
      case _ =>
        false
    }
  }

  def isOptionRef(d: Tree): Boolean = {
    d match {
      case tq"$prefix.Option" =>
        true
      case tq"Option" =>
        true
      case _ =>
        false
    }
  }

  def isSubtree(d: ValDef): Boolean = {
    val r = d.mods.annotations.exists({
      case v @ q"new ${ Ident(n) }()" if n == Builtins.subtreeSym.name =>
        true
      case v =>
        false
    })
    //println(s"isSubtree: $r $d ${showRaw(d)}")
    r
  }

  def addZ(n: Name) = tq"${n.toTermName}.$zipperName"
  def addZTerm(n: Name) = q"${n.toTermName}.${zipperName.toTermName}"
  def addZP(n: Name) = tq"${n.toTermName}.$zipperParentBase"
  def addSubtreeName(n: TypeName, subName: Name) = TypeName(n.decodedName + "$" + subName.decodedName)

  def getFlag(mods: Modifiers, flag: FlagSet): Boolean = mods match {
    case Modifiers(flags, p, anns) =>
      (flags | flag) == flags
  }

  def addFinal(mods: Modifiers): Modifiers = mods match {
    case Modifiers(flags, p, anns) =>
      Modifiers(flags | Flag.FINAL, p, anns)
  }
  def addCase(mods: Modifiers): Modifiers = mods match {
    case Modifiers(flags, p, anns) =>
      Modifiers(flags | Flag.CASE, p, anns)
  }

  val zipperName = TypeName("Z")
  val zipperNameTerm = TermName("Z")
  val zipperParentBase = TypeName("ZipperParent")

  val castName = TermName("swivel_cast")
  val putName = TermName("swivel_put")
  val checkSubtreeName = TermName("swivel_checkSubtree")
  val parentName = TermName("swivel_parent")
  val underlyingName = TermName("swivel_underlying")
  val indexName = TermName("swivel_index")
  val keyName = TermName("swivel_key")
  val parentStringName = TermName("swivel_parentString")

  val reservedFieldNames = Set(
    "value",
    "toZipper",
    "zipper",
    "subtrees",
    "parent",
    "root",
    "replace",
    checkSubtreeName.toString(),
    putName.toString(),
    castName.toString(),
    parentName.toString(),
    underlyingName.toString()).map(TermName(_))

  def checkFieldName(d: DefTree): Unit = {
    if (reservedFieldNames.contains(d.name.toTermName)) {
      c.error(d.pos, s"The name ${d.name} is reserved by swivel and cannot be used as a member name on swivel values.")
    }
  }

  def buildCastMethods(name: Tree): Seq[Tree] = {
    val v = c.freshName[TermName](TermName("v"))
    val v1 = c.freshName[TermName](TermName("v1"))

    Seq(
      q"""
      @inline
      def $castName($v: ${name}#RootValue): ${name} = $v match {
        case $v1: ${name} => $v1
        case _ =>
          throw new IllegalArgumentException($v + " provided where " + ${name.toString()} + " expected.")
      }
      """,
      q"""
      @inline
      def $checkSubtreeName($v: ${name}#RootValue): Unit = $v match {
        case _: ${name} => ()
        case _ =>
          throw new IllegalArgumentException($v + " provided where " + ${name.toString()} + " expected.")
      }
      """)
  }

  def buildReplacementSupport(mods: Modifiers): (Seq[Tree], Seq[Tree]) = {
    val replacementValueTypes = mods.annotations.collect({
      case v @ q"new ${ Ident(n) }[$replacmentValueType]()" if n == Builtins.replacementSym.name =>
        replacmentValueType
    })

    replacementValueTypes.toSeq match {
      case Seq(ty) =>
        (Seq(tq"${Builtins.ZipperReplaceableSym}"), Seq(q"type ReplacementValue = $ty"))
      case Seq() =>
        (Seq(), Seq())
      case ty +: _ =>
        c.error(ty.pos, s"@replacement may only be specified once.")
        (Seq(), Seq())
    }
  }

  def buildTransformSupportNonleaf(mods: Modifiers): (Seq[Tree], Seq[Tree]) = {
    getTransformAnnotationInfo(mods) match {
      case (Some(EmptyTree), pos) =>
        c.error(pos, "@transform on non-leaf nodes must have a function type.")
        (Seq(), Seq())
      case (Some(transformFunctionType), _) =>
        (Seq(tq"${Builtins.ZipperTransformableSym}"), Seq(q"type TranformFunction = $transformFunctionType"))
      case (None, _) =>
        (Seq(), Seq())
    }
  }

  def hasTransformSupportLeaf(mods: Modifiers): Boolean = {
    getTransformAnnotationInfo(mods) match {
      case (Some(EmptyTree), _) =>
        true
      case (Some(ty), pos) =>
        c.warning(pos, "The argument to @transform on leafs is ignored.")
        true
      case (None, _) =>
        false
    }
  }

  def getTransformAnnotationInfo(mods: Modifiers): (Option[Tree], Position) = {
    val transformValueTypes = mods.annotations.collect({
      case v @ q"new ${ Ident(n) }()" if n == Builtins.transformSym.name =>
        (EmptyTree, v)
      case v @ q"new ${ Ident(n) }[$replacmentValueType]()" if n == Builtins.transformSym.name =>
        (replacmentValueType, v)
    })

    transformValueTypes.toSeq match {
      case Seq((ty, v)) =>
        (Some(ty), v.pos)
      case Seq() =>
        (None, c.enclosingPosition)
      case (ty, v) +: _ =>
        c.error(v.pos, "@transform may only be specified once.")
        (Some(ty), v.pos)
    }
  }

  def cleanedMods(mods: Modifiers): Modifiers = {
    mods match {
      case Modifiers(flags, p, anns) =>
        Modifiers(flags, p, anns.filter({
          case v @ q"new ${ Ident(n) }[${ targs }]()" if n == Builtins.replacementSym.name =>
            false
          case v @ q"new ${ Ident(n) }[${ targs }]()" if n == Builtins.transformSym.name =>
            false
          case v @ q"new ${ Ident(n) }()" if n == Builtins.transformSym.name =>
            false
          case v =>
            true
        }))
    }
  }

  def root(annottees: Tree*): Tree = {
    try {
      val (inCls, inObj) = extractClassAndObject("root", annottees)

      val (mods, name, supers, defs) = inCls match {
        case q"$mods class ${ name: TypeName } extends ..${ supers: Seq[Tree] } { ..${ defs: Seq[Tree] } }" if getFlag(mods, Flag.ABSTRACT) && getFlag(mods, Flag.SEALED) =>
          (mods, name, supers, defs)
        case _ =>
          c.error(c.enclosingPosition, "@root may only be applied to sealed abstract classes.")
          throw new CompileErrorReported()
      }

      val (objMod, _, objSupers, rawObjDefs) = decomposeModuleDef(inObj).getOrElse(emptyModuleResults)
      val (inZCls, inZObj, objDefs) = extractStandardCompanionDefs(rawObjDefs)

      val cMods = cleanedMods(mods)
      def zipperRef = addZ(name)
      def zipperParentRef = addZP(name)
      //val zName = addZ(name)
      //val zpName = addZP(name)

      val outCls = q"""
        $cMods class $name extends ..${supers :+ tq"${Builtins.SwivelValueSym}"} {
          type RootValue = $name
          type RootZipper = $zipperRef
          type RootZipperParent = $zipperParentRef
          type Zipper <: $zipperRef
          
          ..$defs
        }
        """

      val (replacementSuper, replacementDefs) = buildReplacementSupport(mods)
      val (transformSuper, transformDefs) = buildTransformSupportNonleaf(mods)
      val zCls = q"""
        $cMods class $zipperName extends ${Builtins.ZipperSym} with ..$replacementSuper with ..$transformSuper {
          type RootValue = $name
          type RootZipper = $zipperRef
          type RootZipperParent = $zipperParentRef
          type Value <: $name
          type SelfType <: $zipperRef
          
          ..$replacementDefs
          ..$transformDefs
        }
        """

      val zpCls = q"""
        $cMods class $zipperParentBase extends ${Builtins.ZipperParentSym} {
          type RootValue = $name
          type RootZipper = $zipperRef
          type RootZipperParent = $zipperParentRef
          type Value <: $name
        }
        """

      val zObj = q"""
        object ${zipperName.toTermName} {
          @inline
          def $castName(v: ${name}#RootValue): ${name} = v
          @inline
          def $checkSubtreeName(v: ${name}#RootValue): Unit = () 
        }"""

      val outObj = q"""
        $objMod object ${name.toTermName} extends ..$objSupers {
          ${mergeWithOption(zCls, inZCls)}
          ${mergeWithOption(zObj, inZObj)}
          $zpCls
          
          ..$objDefs
        }
        """

      val outputs = Seq(outCls, outObj)

      q"""..$outputs"""
    } catch {
      case _: CompileErrorReported =>
        extractExpandees(annottees.toList)
    }

  }

  def branch(annottees: Tree*): Tree = {
    try {
      val (inCls, inObj) = extractClassAndObject("branch", annottees)

      val (mods, name, supers, self, defs) = inCls match {
        case q"$mods class ${ name: TypeName } extends ..${ supers: Seq[Tree] } { $self => ..${ defs: Seq[Tree] } }" if getFlag(mods, Flag.ABSTRACT) && getFlag(mods, Flag.SEALED) =>
          (mods, name, supers, self, defs)
        case _ =>
          c.error(c.enclosingPosition, "@branch may only be applied to sealed abstract classes.")
          throw new CompileErrorReported()
      }

      val (objMod, _, objSupers, rawObjDefs) = decomposeModuleDef(inObj).getOrElse(emptyModuleResults)
      val (inZCls, inZObj, objDefs) = extractStandardCompanionDefs(rawObjDefs)

      val cMods = cleanedMods(mods)
      val directSuper = extractSwivelDirectSuperClassName(supers)
      val zDirectSuper = addZ(directSuper)
      val zpDirectSuper = addZP(directSuper)
      def zipperRef = addZ(name)
      def zipperParentRef = addZP(name)
      //val zName = addZ(name)
      //val zpName = addZP(name)

      val outCls = q"""
        $cMods class $name extends ..${supers :+ tq"${Builtins.SwivelValueSym}"} {
          $self =>
          type Zipper <: $zipperRef
          
          ..$defs
        }
        """

      val (replacementSuper, replacementDefs) = buildReplacementSupport(mods)
      val (transformSuper, transformDefs) = buildTransformSupportNonleaf(mods)
      val zCls = q"""
        $cMods class $zipperName extends $zDirectSuper with ..$replacementSuper with ..$transformSuper {
          type Value <: $name
          type SelfType <: $zipperRef
          
          ..$replacementDefs
          ..$transformDefs
        }"""

      val zpCls = q"""
        $cMods class $zipperParentBase extends $zpDirectSuper {
          type Value <: $name
        }
        """

      val zObj = q"""
        object ${zipperName.toTermName} {
          ..${buildCastMethods(Ident(name))}
        }"""

      val outObj = q"""
        $objMod object ${name.toTermName} extends ..$objSupers {
          ${mergeWithOption(zCls, inZCls)}
          ${mergeWithOption(zObj, inZObj)}
          $zpCls
          
          ..$objDefs
        }
        """

      val outputs = Seq(outCls, outObj)

      q"""..$outputs"""
    } catch {
      case _: CompileErrorReported =>
        extractExpandees(annottees.toList)
    }
  }

  def leaf(annottees: Tree*): Tree = {
    try {
      val (inCls, inObj) = extractClassAndObject("leaf", annottees)

      val (mods, vName, allArgs, supers, defs) = inCls match {
        case q"${ mods: Modifiers } class ${ name: TypeName }(..${ args: Seq[Tree] }) extends ..${ supers: Seq[Tree] } { ..${ defs: Seq[Tree] } }" if getFlag(mods, Flag.FINAL) && !getFlag(mods, Flag.TRAIT) =>
          (mods, name, args, supers, defs)
        case _ =>
          c.error(c.enclosingPosition, "@leaf may only be applied to final classes.")
          throw new CompileErrorReported()
      }

      val (objMod, _, objSupers, rawObjDefs) = decomposeModuleDef(inObj).getOrElse(emptyModuleResults)
      val (inZCls, inZObj, objDefs) = extractStandardCompanionDefs(rawObjDefs)

      def newValue(args: Seq[Tree]) = {
        q"new ${vName}(..$args)"
      }

      val cMods = cleanedMods(mods)
      val directSuper = extractSwivelDirectSuperClassName(supers)
      val zDirectSuper = addZ(directSuper)
      val zpDirectSuper = addZP(directSuper)
      def zipperRef = tq"${vName.toTermName}.$zipperName"
      def makeZipperParentRef(n: Name): Tree = tq"${vName.toTermName}.${addSubtreeName(zipperParentBase, n)}"
      //val zName = addZ(name)
      //val zpName = addZP(name)

      def parentArg = q"val $parentName: Option[$directSuper#RootZipperParent]"
      def parent = Ident(parentArg.name)
      def indexArg = q"protected val $indexName: Int"
      def index = Ident(indexArg.name)
      def underlyingArg = q"val value: $vName"
      def underlying = Ident(underlyingArg.name)

      var allArgHandlers: Seq[ArgumentHandler] = null

      abstract class ArgumentHandler {
        val originalArg: ValDef

        checkFieldName(originalArg)

        val isSubtree: Boolean = BasicTreeMacros.this.isSubtree(originalArg)

        val name: TermName = originalArg.name
        //val rawValueName: TermName = c.freshName(name)
        def zipperParentRef: Tree = makeZipperParentRef(name).originalArgPos()
        val zipperParentName: TypeName = addSubtreeName(zipperParentBase, name)

        def accessorType: Tree = originalArg.tpt
        def accessorExpr: Tree = q"this.$name"
        def contributeToSubtrees(o: Tree): Tree
        //def arg: DefTree = originalArg

        def accessorTypeZ: Tree
        def accessorDefZMods: Modifiers = {
          if(getFlag(originalArg.mods, Flag.OVERRIDE)) {
            Modifiers(Flag.OVERRIDE, originalArg.mods.privateWithin, originalArg.mods.annotations)
          } else {
            Modifiers(NoFlags, originalArg.mods.privateWithin, originalArg.mods.annotations)
          }
        }
        def accessorExprZ: Tree
        def contributeToSubtreesZ(o: Tree): Tree
        def rawAccessorExprZ: Tree = q"$underlying.$name"
        def accessorDefZ: DefTree
        def copyArgZ: ValDef = ValDef(Modifiers(Flag.PARAM), name, accessorType, rawAccessorExprZ).originalArgPos()
        def copyArgRefZ: Ident = Ident(name.toTermName)

        val transformSubFunction: TermName = TermName("on" + elementType.name.toString())

        def elementType: Ident
        def elementTypeZ: Tree = addZ(elementType.name)
        def elementCompanionZ: Tree = addZTerm(elementType.name)

        def argZP: ValDef = ValDef(Modifiers(Flag.PARAM), name, accessorType, EmptyTree).originalArgPos()
        def accessorExprZP: Tree
        def zpDefinition: Option[ClassDef]

        lazy val transOrigName = c.freshName(name)
        def transIntermediateName: TermName
        def transOrigDefinition = Some(q"""val $transOrigName = ${accessorExprZ}""")
        def transIntermediateDefinition: Option[Tree]

        protected implicit class TreeAdds[T <: Tree](val t: T) {
          def originalArgPos(): t.type = c.internal.setPos(t, originalArg.pos)
        }
      }

      case class NonsubtreeArgument(originalArg: ValDef) extends ArgumentHandler {
        //require(!isSubtree)

        def accessorTypeZ: Tree = accessorType
        def accessorExprZ: Tree = rawAccessorExprZ
        def accessorDefZ: DefTree = q"$accessorDefZMods def $name: $accessorTypeZ = $accessorExprZ".originalArgPos()

        def elementType: Ident = Ident(TypeName("Nothing"))

        def accessorExprZP: Tree = accessorExpr
        def zpDefinition: Option[ClassDef] = None

        def contributeToSubtrees(o: Tree): Tree = o
        def contributeToSubtreesZ(o: Tree): Tree = o

        lazy val transIntermediateName = transOrigName
        def transIntermediateDefinition = None
      }

      abstract class SubtreeArgument extends ArgumentHandler {
        def accessorExprZ: Tree = q"this.$name"

        def zpReconstructSelfArg: Tree
        def zpToStringSelfCode: Seq[Tree]
        def zpArgs: Seq[ValDef]

        def zpReconstructValueArgs = allArgHandlers.map { h =>
          if (name == h.name) {
            zpReconstructSelfArg
          } else {
            h.accessorExprZP
          }
        }
        def zpToStringCode = {
          val argsToString: Seq[Tree] = q"${s"${vName.toString()}.Z("}" +:
            allArgHandlers.map({
              case h if h.name == name => q"${","}" +: zpToStringSelfCode
              case h => Seq(q"${","}", h.accessorExprZP)
            }).flatten.drop(1) :+ q"${")"}" :+ q"this.$parentStringName"
          argsToString.reduceLeft((a, b) => q"$a + $b")
        }

        def zpDefinition: Option[ClassDef] = {
          // TODO: Make this not a case class and add custom generated hashCode and equals.
          Some(
            q"""
            ${addFinal(cMods)} class $zipperParentName(..$zpArgs, $parentArg) extends $zpDirectSuper {
              type Value = $vName
            
              def $putName(v: RootValue): $zipperRef = new $zipperRef(${newValue(zpReconstructValueArgs)}, $parent)
              def $checkSubtreeName(v: RootValue): Unit = $elementCompanionZ.$checkSubtreeName(v)
              override def toString(): String = {
                $zpToStringCode
              }
            }
            """.originalArgPos())
        }

        lazy val transIntermediateName = c.freshName(name)
        def transComputeSingle(v: Tree) = q"""
          { 
            val modified1Value = $v.transformChildren(f)
            val modified1 = modified1Value.toZipper($v.$parentName)
            val modified2 = if (f.${transformSubFunction}.isDefinedAt(modified1)) {
              f.${transformSubFunction}(modified1)
            } else {
              modified1Value
            }
            val vValue = $v.value
            if ((vValue eq modified2) || (vValue == modified2)) {
              vValue
            } else {
              changed = true
              vValue.transferMetadata(modified2)
            }
          }""".originalArgPos()
        def transComputation: Tree
        def transIntermediateDefinition: Option[Tree] = Some(q"val $transIntermediateName = $transComputation".originalArgPos())
      }

      case class ScalarArgument(originalArg: ValDef, elementType: Ident) extends SubtreeArgument {
        require(isSubtree)

        def accessorTypeZ: Tree = addZ(elementType.name)
        def accessorDefZ: DefTree = {
          val zpConstrArgs = allArgHandlers.filterNot(_.name == name).map(_.rawAccessorExprZ)
          q"""$accessorDefZMods def $name: $accessorTypeZ = $rawAccessorExprZ.toZipper(Some(new $zipperParentRef(..$zpConstrArgs, $parent)))""".originalArgPos()
        }

        def contributeToSubtrees(o: Tree): Tree = q"$o :+ $accessorExpr".originalArgPos()
        def contributeToSubtreesZ(o: Tree): Tree = q"$o :+ $accessorExprZ".originalArgPos()

        def zpArgs = {
          allArgHandlers.filter(_.name != name).map(_.argZP)
        }
        def zpReconstructSelfArg = q"$elementCompanionZ.$castName(v)"
        def zpToStringSelfCode = Seq(q"${"[]"}")
        def accessorExprZP: Tree = q"this.$name"

        def transComputation: Tree = transComputeSingle(Ident(transOrigName))
      }

      case class SeqArgument(originalArg: ValDef, containerType: Tree, elementType: Ident) extends SubtreeArgument {
        require(isSubtree)

        def accessorTypeZ: Tree = tq"$containerType[$elementTypeZ]"
        def accessorDefZ: DefTree = {
          val zpConstrArgs = allArgHandlers.map(h => {
            if (name != h.name) {
              h.rawAccessorExprZ
            } else {
              q"${rawAccessorExprZ}.updated(i, null)".originalArgPos()
            }
          })
          q"""
            $accessorDefZMods val $name: $accessorTypeZ = $rawAccessorExprZ.view.zipWithIndex.map({ p =>
              val f = p._1
              val i = p._2
              f.toZipper(Some(new $zipperParentRef(..$zpConstrArgs, i, $parent)))
            })
            """.originalArgPos()
        }

        def contributeToSubtrees(o: Tree): Tree = q"$o ++ $accessorExpr".originalArgPos()
        def contributeToSubtreesZ(o: Tree): Tree = q"$o ++ $accessorExprZ".originalArgPos()

        def zpArgs = {
          allArgHandlers.map(_.argZP) :+ indexArg
        }
        def zpReconstructSelfArg = q"$accessorExprZP.updated($index, $elementCompanionZ.$castName(v))".originalArgPos()
        //             println((getClass(), v)) 
        def zpToStringSelfCode = Seq(q"""
          $accessorExprZP.updated($index, "[]").map({ v =>
            v.toString()
          }).toString()""".originalArgPos())
        def accessorExprZP: Tree = q"this.$name"

        override def transOrigDefinition = Some(q"""val $transOrigName = ${accessorExprZ}.view.force""".originalArgPos())
        def transComputation: Tree = q"$transOrigName.map(v => ${transComputeSingle(q"v")})".originalArgPos()
      }

      case class OptionSeqArgument(originalArg: ValDef, containerType1: Tree, containerType2: Tree, elementType: Ident) extends SubtreeArgument {
        require(isSubtree)

        def accessorTypeZ: Tree = tq"$containerType1[$containerType2[$elementTypeZ]]"
        def accessorDefZ: DefTree = {
          val zpConstrArgs = allArgHandlers.map(h => {
            if (name != h.name) {
              h.rawAccessorExprZ
            } else {
              q"${rawAccessorExprZ}.map(_.updated(i, null))".originalArgPos()
            }
          })
          q"""
            $accessorDefZMods val $name: $accessorTypeZ = $rawAccessorExprZ.map(_.view.zipWithIndex.map({ p =>
              val f = p._1
              val i = p._2
              f.toZipper(Some(new $zipperParentRef(..$zpConstrArgs, i, $parent)))
            }))
            """.originalArgPos()
        }

        def contributeToSubtrees(o: Tree): Tree = q"$o ++ $accessorExpr.toSeq.flatten".originalArgPos()
        def contributeToSubtreesZ(o: Tree): Tree = q"$o ++ $accessorExprZ.toSeq.flatten".originalArgPos()

        def zpArgs = {
          allArgHandlers.map(_.argZP) :+ indexArg
        }
        def zpReconstructSelfArg = q"$accessorExprZP.map(_.updated($index, $elementCompanionZ.$castName(v)))".originalArgPos()
        def zpToStringSelfCode = Seq(q"""
          $accessorExprZP.map(_.updated($index, "[]").map({ v =>
            v.toString()
          })).toString()""".originalArgPos())
        def accessorExprZP: Tree = q"this.$name"

        override def transOrigDefinition = Some(q"""val $transOrigName = ${accessorExprZ}.map(_.view.force)""".originalArgPos())
        def transComputation: Tree = q"$transOrigName.map(_.map(v => ${transComputeSingle(q"v")}))".originalArgPos()
      }

      case class OptionArgument(originalArg: ValDef, containerType: Tree, elementType: Ident) extends SubtreeArgument {
        require(isSubtree)

        def accessorTypeZ: Tree = tq"$containerType[$elementTypeZ]"
        def accessorDefZ: DefTree = {
          val zpConstrArgs = allArgHandlers.filterNot(_.name == name).map(_.rawAccessorExprZ)
          q"""$accessorDefZMods def $name: $accessorTypeZ = $rawAccessorExprZ.map(_.toZipper(Some(new $zipperParentRef(..$zpConstrArgs, $parent))))""".originalArgPos()
        }

        def contributeToSubtrees(o: Tree): Tree = q"$o ++ $accessorExpr".originalArgPos()
        def contributeToSubtreesZ(o: Tree): Tree = q"$o ++ $accessorExprZ".originalArgPos()

        def zpArgs = {
          allArgHandlers.filter(_.name != name).map(_.argZP)
        }
        def zpReconstructSelfArg = q"Some($elementCompanionZ.$castName(v))"
        def zpToStringSelfCode = Seq(q"${"[]"}")
        def accessorExprZP: Tree = q"this.$name"

        def transComputation: Tree = q"$transOrigName.map(v => ${transComputeSingle(q"v")})".originalArgPos()
      }

      case class MapArgument(originalArg: ValDef, containerType: Tree, keyType: Tree, elementType: Ident) extends SubtreeArgument {
        require(isSubtree)

        def accessorTypeZ: Tree = tq"$containerType[$keyType, $elementTypeZ]"
        def accessorDefZ: DefTree = {
          val zpConstrArgs = allArgHandlers.map(h => {
            if (name != h.name) {
              h.rawAccessorExprZ
            } else {
              q"${h.rawAccessorExprZ} - k".originalArgPos()
            }
          })
          q"""
            $accessorDefZMods val $name: $accessorTypeZ = $rawAccessorExprZ.map({ p =>
              val k = p._1
              val v = p._2
              (k, v.toZipper(Some(new $zipperParentRef(..$zpConstrArgs, k, $parent))))
            })
            """.originalArgPos()
        }

        def contributeToSubtrees(o: Tree): Tree = q"$o ++ $accessorExpr.values".originalArgPos()
        def contributeToSubtreesZ(o: Tree): Tree = q"$o ++ $accessorExprZ.values".originalArgPos()

        def keyArg = q"protected val $keyName: $keyType".originalArgPos()
        def key = keyArg.name
        def zpArgs = {
          allArgHandlers.map(_.argZP) :+ keyArg
        }
        def zpReconstructSelfArg = q"$accessorExprZP.updated($key, $elementCompanionZ.$castName(v))".originalArgPos()
        def zpToStringSelfCode = Seq(q"""$accessorExprZP.mapValues(_.toString()).updated($key, "[]").toString()""".originalArgPos())
        def accessorExprZP: Tree = q"this.$name"

        override def transOrigDefinition = Some(q"""val $transOrigName = ${accessorExprZ}""".originalArgPos())
        def transComputation: Tree = q"$transOrigName.mapValues(v => ${transComputeSingle(q"v")}).view.force".originalArgPos()
      }

      object ArgumentHandler {
        def apply(d: ValDef): ArgumentHandler = {
          d.tpt match {
            case tpt if !isSubtree(d) =>
              NonsubtreeArgument(d)
            case typ @ Ident(_) =>
              ScalarArgument(d, typ)
            case tq"$prefix[${ elemType @ Ident(_) }]" if isSeqRef(prefix) =>
              SeqArgument(d, prefix, elemType)
            case tq"$prefix[${ elemType @ Ident(_) }]" if isOptionRef(prefix) =>
              OptionArgument(d, prefix, elemType)
            case tq"$prefix1[$prefix2[${ elemType @ Ident(_) }]]" if isOptionRef(prefix1) && isSeqRef(prefix2) =>
              OptionSeqArgument(d, prefix1, prefix2, elemType)
            case tq"$prefix[$keyType, ${ valType @ Ident(_) }]" if isMapRef(prefix) =>
              MapArgument(d, prefix, keyType, valType)
            case tpt =>
              c.error(d.pos, "Unsupported field type for @subtree annotation.")
              NonsubtreeArgument(d)
          }
        }
      }

      allArgHandlers = allArgs.map(ArgumentHandler(_))

      val outCls = q"""
        ${addFinal(cMods)} class $vName(..$allArgs) extends ..${supers} {
          type Zipper = $zipperRef
          def toZipper(parent: Option[RootZipperParent]): $zipperRef = new $zipperRef(this, parent)
          def subtrees: Seq[RootValue] = ${allArgHandlers.foldRight(q"Seq()")(_ contributeToSubtrees _)}
          
          ..$defs
        }
       """

      // TODO: Fix value implementation to use inObj.apply if it exists and similarly for case classes.
      //       This is needed so that the value created goes through normal channels and the real constructor can be
      //       private to force such things as hash-consing.

      val (replacementSuper, replacementDefs) = buildReplacementSupport(mods)
      val hasTransform = hasTransformSupportLeaf(mods)

      val transformMethod = if (hasTransform) {
        val transformMethod = q"""
          def transformChildren(f: TranformFunction): $vName = {
            var changed: Boolean = false
            ..${allArgHandlers.flatMap(_.transOrigDefinition)}
            ..${allArgHandlers.flatMap(_.transIntermediateDefinition)}
            if (!changed) {
              this.value
            } else {
              copy(..${allArgHandlers.map(h => h.transIntermediateName)}).value
            }
          }
          """
        Some(transformMethod)
      } else {
        None
      }

      val zCls = q"""
        final class $zipperName($underlyingArg, $parentArg) extends $zDirectSuper with ..$replacementSuper {
          type Value = $vName
          type SelfType = $zipperRef
          
          ..${allArgHandlers.map(_.accessorDefZ)}
          def subtrees: Seq[RootZipper] = ${allArgHandlers.foldRight(q"Seq()")(_ contributeToSubtreesZ _)}
          
          def copy(..${allArgHandlers.map(_.copyArgZ)}): $zipperRef = new $zipperRef(${newValue(allArgHandlers.map(_.copyArgRefZ))}, ${parentArg.name})
                    
          ..$replacementDefs
          ..${transformMethod.toSeq}
        }
        """

      val zClsParents = allArgHandlers.flatMap(_.zpDefinition)

      val zTypes = allArgHandlers.map(_.accessorTypeZ)
      val zUnapplyValues = allArgHandlers.map(h => q"v.${h.name}")
      val unapplyMethod = if (allArgHandlers.isEmpty) {
        q"""
          def unapply(v: $zipperRef): Boolean = { 
            v != null
          }
        """
      } else {
        q"""
          def unapply(v: $zipperRef): Option[(..$zTypes)] = { 
            if (v == null)
              None
            else           
              Some((..$zUnapplyValues))
          }
        """
      }
      val zObj = q"""
        object ${zipperName.toTermName} {
          $unapplyMethod
          ..${buildCastMethods(Ident(vName))}
        }
        """

      val outObj = q"""
        $objMod object ${vName.toTermName} extends ..$objSupers {
          ${mergeWithOption(zCls, inZCls)}
          ${mergeWithOption(zObj, inZObj)}
          ..$zClsParents
          
          ..$objDefs
        }
        """

      val outputs = Seq(outCls, outObj)

      q"""..$outputs"""
    } catch {
      case _: CompileErrorReported =>
        extractExpandees(annottees.toList)
    }
  }
}