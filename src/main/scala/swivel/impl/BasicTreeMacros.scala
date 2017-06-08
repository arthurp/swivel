package swivel.impl

import scala.reflect.macros.whitebox.Context 

object BasicTreeMacros {
  class CompileErrorReported() extends Exception
}

class BasicTreeMacros(val c: Context) {
  import BasicTreeMacros._
  import c.universe._
  
  object Builtins {
    val child = c.typecheck(tq"_root_.swivel.child", c.TYPEmode)
    val childSym = child.symbol
    val Zipper = c.typecheck(tq"_root_.swivel.Zipper", c.TYPEmode)
    val ZipperSym = Zipper.symbol
    val ZipperParent = c.typecheck(tq"_root_.swivel.ZipperParent", c.TYPEmode)
    val ZipperParentSym = ZipperParent.symbol
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
  
  def extractSwivelDirectSuperClassName(supers: Seq[Tree]): TypeName = supers.headOption match {
    case Some(Ident(n@TypeName(_))) => n
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
  
  def isChild(d: ValDef): Boolean = {
    println(s"isChild: $d ${showRaw(d)}")
    val r = d.mods.annotations.exists({
      case v@q"new child()" =>
        println(s"isChild is: $v ${showRaw(v)}")
        true
      case v =>
        println(s"isChild not: $v ${showRaw(v)}")
        false
    })
    println(s"isChild: $r $d ${showRaw(d)}")
    r
  }
      
  def addZ(n: TypeName) = TypeName(n.decodedName + "Z")
  def addZP(n: TypeName) = TypeName(n.decodedName + "Z$P")
  def addSubtreeName(n: TypeName, subName: Name) = TypeName(n.decodedName + "$" + subName.decodedName)
  
  def addFinal(mods: Modifiers): Modifiers = mods match {
    case Modifiers(flags, p, anns) =>
      Modifiers(flags | Flag.FINAL, p, anns)
  }
  def addCase(mods: Modifiers): Modifiers = mods match {
    case Modifiers(flags, p, anns) =>
      Modifiers(flags | Flag.CASE, p, anns)
  }
  
  val castName = TermName("swivel_cast")
  val putName = TermName("swivel_put")
  val checkChildName = TermName("swivel_checkChild")
  
  val reservedFieldNames = Set(
      "value",
      "toZipper",
      "subtrees",
      "toStringAsParent",
      checkChildName.toString(),
      putName.toString(),
      castName.toString()
      ).map(TermName(_))
  
  def checkFieldName(d: DefTree): Unit = {
    if (reservedFieldNames.contains(d.name.toTermName)) {
      c.error(d.pos, s"The name ${d.name} is reserved by swivel. (The full list of reserved names is: ${reservedFieldNames.mkString(", ")})")
    }
  }
  
  def buildCastMethods(name: TypeName): Seq[Tree] = {
    val v = c.freshName[TermName](TermName("v"))
    val v1 = c.freshName[TermName](TermName("v1"))
    
    Seq(
    q"""
      @inline
      def $castName($v: ${name}#RootValue): ${name} = $v match {
        case $v1: ${name} => $v1
        case _ =>
          throw new Error($v + " provided where " + ${name.decoded} + " expected. (Sorry about the dynamic typing.)")
      }
      """,
    q"""
      @inline
      def $checkChildName($v: ${name}#RootValue): Unit = $v match {
        case _: ${name} => ()
        case _ =>
          throw new Error($v + " provided where " + ${name.decoded} + " expected. (Sorry about the dynamic typing.)")
      }
      """
    )
  }

  def root(annottees: Tree*): Tree = {
    try {
      val (inCls, inObj) = extractClassAndObject("root", annottees)
      
      val (mods, name, supers) = inCls match {
        case q"$mods class ${name: TypeName} extends ..${supers: Seq[Tree]}" =>
          (mods, name, supers)
        case _ =>
          c.error(c.enclosingPosition, "@root may only be applied to sealed abstract classes.")
          throw new CompileErrorReported()
      }
      
      val zName = addZ(name)
      val zpName = addZP(name)
      
      val outCls = q"""
        $mods class $name extends ..${supers :+ tq"${Builtins.SwivelValueSym}"} {
          type RootValue = $name
          type RootZipper = $zName
          type RootZipperParent = $zpName
          type Zipper <: $zName
        }
        """
      
      val zpCls = q"""
        $mods class $zpName extends ${Builtins.ZipperParentSym} {
          type RootValue = $name
          type RootZipper = $zName
          type RootZipperParent = $zpName
          type Value <: $name
        }
        """
      
      val zCls = q"""
        $mods class $zName extends ${Builtins.ZipperSym} {
          type RootValue = $name
          type RootZipper = $zName
          type RootZipperParent = $zpName
          type Value <: $name
        }
        """
      
      val zObj = q"""
        object ${zName.toTermName} {
          @inline
          def $castName(v: ${name}#RootValue): ${name} = v
          @inline
          def $checkChildName(v: ${name}#RootValue): Unit = () 
        }"""
      
      val outputs = Seq(outCls, zCls, zpCls, zObj) ++ inObj
      
      q"""..$outputs"""
    } catch {
      case _: CompileErrorReported =>
        extractExpandees(annottees.toList)
    }
    
  }
  
  def branch(annottees: Tree*): Tree = {
    try {
      val (inCls, inObj) = extractClassAndObject("branch", annottees)
      
      val (mods, name, supers) = inCls match {
        case q"$mods class ${name: TypeName} extends ..${supers: Seq[Tree]}" =>
          (mods, name, supers)
        case _ =>
          c.error(c.enclosingPosition, "@branch may only be applied to sealed abstract classes.")
          throw new CompileErrorReported()
      }
      
      val directSuper = extractSwivelDirectSuperClassName(supers)
      val zDirectSuper = addZ(directSuper)
      val zpDirectSuper = addZP(directSuper)
      val zName = addZ(name)
      val zpName = addZP(name)
      
      val outCls = q"""
        $mods class $name extends ..${supers :+ tq"${Builtins.SwivelValueSym}"} {
          type Zipper <: $zName
        }
        """
          
      val zCls = q"""
        $mods class $zName extends $zDirectSuper {
          type Value <: $name
        }"""
      
      val zpCls = q"""
        $mods class $zpName extends $zpDirectSuper {
          type Value <: $name
        }
        """
      
      val zObj = q"""
        object ${zName.toTermName} {
          ..${buildCastMethods(name)}
        }"""
      
      val outputs = Seq(outCls, zCls, zpCls, zObj) ++ inObj
      
      q"""..$outputs"""
    } catch {
      case _: CompileErrorReported =>
        extractExpandees(annottees.toList)
    }
  }
  
  def leaf(annottees: Tree*): Tree = {
    try {
      val (inCls, inObj) = extractClassAndObject("leaf", annottees)
      
      val (mods, name, allArgs, supers, defs) = inCls match {
        case q"${mods: Modifiers} class ${name: TypeName}(..${args: Seq[Tree]}) extends ..${supers: Seq[Tree]} { ..${defs: Seq[Tree]} }" =>
          (mods, name, args, supers, defs)
        case _ =>
          c.error(c.enclosingPosition, "@leaf may only be applied to final case classes.")
          throw new CompileErrorReported()          
      }
      
      // TODO: Implement special handling for Seq[_], Set[_], and Map[_, _] fields
      
      //val childArgs = allArgs.filter(isChild(_))
      
      val directSuper = extractSwivelDirectSuperClassName(supers)
      val zDirectSuper = addZ(directSuper)
      val zpDirectSuper = addZP(directSuper)
      val zName = addZ(name)
      val zpName = addZP(name)

      val inClsArgNames = allArgs.map(a => extractDefinition(a).name.toTermName)
      
      val outCls = q"""
        ${addFinal(mods)} class $name(..$allArgs) extends ..${supers} {
          type Zipper = $zName
          def toZipper(parent: Option[RootZipperParent]): $zName = new $zName(..${inClsArgNames}, parent)
          def subtrees = Seq(..${(allArgs zip inClsArgNames).filter(p => isChild(p._1)).map(_._2)})
          
          ..$defs
        }
       """

      val zClsArgs = allArgs.map(a => {
        val d = extractDefinition(a)
        val n = c.freshName[TermName](d.name)
        ValDef(Modifiers(Flag.PARAM), n, d.tpt, EmptyTree)
      })
      val zClsArgNames = zClsArgs.map(a => a.name)
      val parentName = TermName("_parent")
      val parentArg = q"protected val $parentName: Option[$directSuper#RootZipperParent]"
      val argsToString = zClsArgNames.map(Ident(_): Tree)
            .reduceOption((a, b) => q"$a.toString() + ${", "} + $b")
            .getOrElse(q"${""}")
      val zClsCopyArgs = (allArgs zip zClsArgNames).map(p => {
        val (a, zArgName) = p
        val d = extractDefinition(a)
        ValDef(Modifiers(Flag.PARAM), d.name, d.tpt, Ident(zArgName))
      })

      val subtreeDefs = (allArgs zip zClsArgNames).map(p => {
        val (a, zArgName) = p
        val d = extractDefinition(a)
        d.tpt match {
          case tpt if !isChild(d) =>
            q"""
              def ${d.name}: $tpt = $zArgName
              """
          case Ident(typName) =>
            q"""
              def ${d.name}: ${addZ(typName.toTypeName)} = 
                 $zArgName.toZipper(Some(new ${addSubtreeName(zpName, d.name)}(
                   ..${zClsArgNames.filterNot(_ == zArgName)}, ${parentArg.name})))
              """
          /*case tq"$prefix.Seq[${Ident(typName)}]" => */
        }
      })

      val zCls = q"""
        final class $zName(..$zClsArgs, $parentArg) extends $zDirectSuper {
          type Value = $name
          
          ..$subtreeDefs
          def subtrees: Seq[RootZipper] = Seq(..${(allArgs zip inClsArgNames).filter(p => isChild(p._1)).map(_._2)})
          def value: Value = ${name.toTermName}(..$zClsArgNames)
          
          def copy(..${zClsCopyArgs}): $zName = new $zName(..${zClsCopyArgs.map(_.name)}, ${parentArg.name})
          
          override def toString(): String = ${zName.encoded} + "(" + $argsToString + ")" + parentString
        }
        """

      val zClsParents = (allArgs zip zClsArgNames).map(p => {
          val (a, zArgName) = p
          val d = extractDefinition(a)
          if(isChild(d)) {
            val parentClsName = addSubtreeName(zpName, d.name)
            val zClsParentArgs = zClsArgs.filterNot(_.name == zArgName)
            val vArg = c.freshName(TermName("v"))
            val argsToString: Seq[Tree] = q"${s"${zName.encoded}("}" +:
              zClsArgs.map({
                case ValDef(_, `zArgName`, _, _) => Seq(q"${", "}", q"${"[]"}")
                case d => Seq(q"${", "}", Ident(d.name))
              }).flatten.tail :+ q"${")"}" :+ q"this.parentString"
            val toStringCode = argsToString
              .reduceLeft((a, b) => q"$a + $b")
            val newArgsForPut = zClsArgs
              .map({
                case ValDef(_, `zArgName`, Ident(tpt: TypeName), _) => q"${addZ(tpt).toTermName}.$castName($vArg)"
                case d@ValDef(_, `zArgName`, tpt, _) =>
                  c.error(d.pos, s"Fields with type $tpt are not supported by Swivel.")
                  q"???"
                case d => Ident(d.name)
              })
            val newArgTpt = zClsArgs.collect({
                case ValDef(_, `zArgName`, Ident(tpt: TypeName), _) => tpt
              }).headOption.getOrElse(TypeName("???"))
            q"""
              ${addCase(addFinal(mods))} class $parentClsName(..$zClsParentArgs, $parentArg) extends $zpDirectSuper {
                type Value = $name
              
                def $putName($vArg: RootValue): $zName = new $zName(..$newArgsForPut, ${parentArg.name})
                def $checkChildName($vArg: RootValue): Unit = ${addZ(newArgTpt).toTermName}.$checkChildName($vArg)
                override def toString(): String = $toStringCode
              }
              """
          } else {
            EmptyTree
          }
        }).filterNot(_ == EmptyTree)

      val zTypes = allArgs.map(a => {
        val d = extractDefinition(a)
        d.tpt match {
          case tpt if !isChild(d) =>
            tpt
          case Ident(typName) =>
            Ident(addZ(typName.toTypeName))
          /*case tq"$prefix.Seq[${Ident(typName)}]" => */
        }
      })
      val zUnapplyValues = allArgs.map(a => {
        val d = extractDefinition(a)
        q"v.${d.name}"
      })

      val zObj = q"""
        object ${zName.toTermName} {
          def unapply(v: $zName): Option[(..$zTypes)] = { 
            if (v == null)
              None
            else           
              Some((..$zUnapplyValues))
          }
          ..${buildCastMethods(name)}
        }
        """
      
      val outputs = Seq(outCls, zCls, zObj) ++ zClsParents ++ inObj
      
      q"""..$outputs"""
    } catch {
      case _: CompileErrorReported =>
        extractExpandees(annottees.toList)
    }
  }
}