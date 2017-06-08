package swivel.impl

import scala.reflect.macros.whitebox.Context 

object BasicTreeMacros {
  class CompileErrorReported() extends Exception
}

class BasicTreeMacros(val c: Context) {
  import BasicTreeMacros._
  import c.universe._
  
  object Builtins {
    val Zipper = c.typecheck(tq"_root_.swivel.Zipper", c.TYPEmode)
    val ZipperSym = Zipper.symbol
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
      
  def addZ(n: TypeName) = TypeName(n.decodedName + "_Z")
  def addSubtreeName(n: TypeName, subName: Name) = TypeName(n.decodedName + "_" + subName.decodedName)
  
  val castName = TermName("swivel$$cast")
  
  val reservedFieldNames = Set(
      "value",
      "toZipper",
      "subtrees",
      "put",
      "toStringAsParent",
      castName.toString()
      ).map(TermName(_))
  
  def checkFieldName(d: DefTree): Unit = {
    if (reservedFieldNames.contains(d.name.toTermName)) {
      c.error(d.pos, s"The name ${d.name} is reserved by swivel. (The full list of reserved names is: ${reservedFieldNames.mkString(", ")})")
    }
  }
  
  def buildCastMethod(name: TypeName) = {
    val v = c.freshName[TermName](TermName("v"))
    val v1 = c.freshName[TermName](TermName("v1"))
    
    q"""
      @inline
      def $castName($v: ${name}#RootValue): ${name} = $v match {
        case $v1: ${name} => $v1
        case _ =>
          throw new Error($v + " provided where " + ${name.decoded} + " expected. (Sorry about the dynamic typing.)")
      }
      """
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
      
      val outCls = q"""
        $mods class $name extends ..${supers :+ tq"${Builtins.SwivelValueSym}"} {
          type RootValue = $name
          type RootZipper = $zName
          type Zipper <: $zName
        }
        """
      
      val zCls = q"""
        $mods class $zName extends ${Builtins.ZipperSym} {
          type RootValue = $name
          type RootZipper = $zName
          type Value <: $name
        }
        """
      
      val zObj = q"""
        object ${zName.toTermName} {
          @inline
          def $castName(v: ${name}#RootValue): ${name} = v
        }"""
      
      val outputs = Seq(outCls, zCls, zObj) ++ inObj
      
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
      val zName = addZ(name)
      
      val outCls = q"""
        $mods class $name extends ..${supers :+ tq"${Builtins.SwivelValueSym}"} {
          type Zipper <: $zName
        }
        """
          
      val zCls = q"""
        $mods class $zName extends $zDirectSuper {
          type Value <: $name
        }"""
      
      val zObj = q"""
        object ${zName.toTermName} {
          ${buildCastMethod(name)}
        }"""
      
      val outputs = Seq(outCls, zCls, zObj) ++ inObj
      
      q"""..$outputs"""
    } catch {
      case _: CompileErrorReported =>
        extractExpandees(annottees.toList)
    }
  }
  
  def extractDeclaration(arg: Tree): ValDef = arg match {
    case d: ValDef => 
      d
    case _ =>
      c.error(arg.pos, s"Must be a simple definition.")
      throw new CompileErrorReported()
  }
  /*
  def extractDeclaredVariableSwivelClass(arg: Tree): Option[Name] = arg match {
    case d: ValDef =>
      println((showRaw(d.tpt), d.tpt.symbol))
      Some(d.name)
    case _ =>
      c.error(arg.pos, s"Must be a simple definition.")
      throw new CompileErrorReported()
  }
  */
  
  def leaf(annottees: Tree*): Tree = {
    try {
      val (inCls, inObj) = extractClassAndObject("leaf", annottees)
      
      val (mods, name, args, supers, defs) = inCls match {
        case q"${mods: Modifiers} class ${name: TypeName}(..${args: Seq[Tree]}) extends ..${supers: Seq[Tree]} { ..${defs: Seq[Tree]} }" =>
          (mods, name, args, supers, defs)
        case _ =>
          c.error(c.enclosingPosition, "@leaf may only be applied to final case classes.")
          throw new CompileErrorReported()          
      }
      
      // TODO: Implement special handling for Seq[_], Set[_], and Map[_, _] fields
      
      val directSuper = extractSwivelDirectSuperClassName(supers)
      val zDirectSuper = addZ(directSuper)
      val zName = addZ(name)

      /*
        def toZipper(parent: Option[FormulaZ]): AddZ = new AddZ_L(l, r, parent)
        def subtrees = Seq(l, r)
       */
      val inClsArgNames = args.map(a => extractDeclaration(a).name.toTermName)
      //val inClsSwivelArgNames = args.flatMap(extractDeclaredVariableSwivelClass)
      
      val outCls = q"""
        $mods class $name(..$args) extends ..${supers} {
          type Zipper = $zName
          def toZipper(parent: Option[RootZipper]): $zName = new $zName(..${inClsArgNames}, parent)
          def subtrees = Seq(..${inClsArgNames})
          
          ..$defs
        }
       """

      val zClsArgs = args.map(a => {
        val d = extractDeclaration(a)
        val n = c.freshName[TermName](d.name)
        ValDef(Modifiers(Flag.PARAM), n, d.tpt, EmptyTree)
      })
      val zClsArgNames = zClsArgs.map(a => a.name)
      val parentName = TermName("_parent")
      val parentArg = q"protected val $parentName: Option[$directSuper#RootZipper]"
           //ValDef(Modifiers(Flag.PARAM), parentName, tq"Option[$directSuper#RootZipper]", EmptyTree)
      val argsToString = zClsArgNames.foldRight(q"${""}": Tree)((a, b) => q"$a.toString() + $b")
      val zClsCopyArgs = (args zip zClsArgNames).map(p => {
        val (a, zArgName) = p
        val d = extractDeclaration(a)
        ValDef(Modifiers(Flag.PARAM), d.name, d.tpt, Ident(zArgName))
      })

      val subtreeDefs = (args zip zClsArgNames).map(p => {
        val (a, zArgName) = p
        val d = extractDeclaration(a)
        val Ident(typName) = d.tpt
        q"""
          def ${d.name}: ${addZ(typName.toTypeName)} = 
             $zArgName.toZipper(Some(new ${addSubtreeName(zName, d.name)}(
               ..${zClsArgNames.filterNot(_ == zArgName)}, ${parentArg.name})))
          """
      })

      val zCls = q"""
        sealed case class $zName(..$zClsArgs, $parentArg) extends $zDirectSuper {
          type Value = $name
          
          ..$subtreeDefs
          def subtrees: Seq[RootZipper] = Seq(..$inClsArgNames)
          def value: Value = ${name.toTermName}(..$zClsArgNames)
          
          def copy(..${zClsCopyArgs}): $zName = new $zName(..${zClsCopyArgs.map(_.name)}, ${parentArg.name})
          
          override def toString(): String = ${zName.encoded} + "(" + $argsToString + ")" + parentString
        }
        """

      val zClsParents = (args zip zClsArgNames).map(p => {
          val (a, zArgName) = p
          val d = extractDeclaration(a)
          val parentClsName = addSubtreeName(zName, d.name)
          val zClsParentArgs = zClsArgs.filterNot(_.name == zArgName)
          val vArg = c.freshName(TermName("v"))
          val argsToString = zClsArgs
            .map({
              case ValDef(_, `zArgName`, _, _) => q"${"[]"}"
              case d => Ident(d.name)
            })
            .foldRight(q"${""}": Tree)((a, b) => q"$a.toString() + $b")
          val newArgsForPut = zClsArgs
            .map({
              case ValDef(_, `zArgName`, Ident(tpt: TypeName), _) => q"${addZ(tpt).toTermName}.$castName($vArg)"
              case d@ValDef(_, `zArgName`, tpt, _) =>
                c.error(d.pos, s"Fields with type $tpt are not supported by Swivel.")
                q"???"
              case d => Ident(d.name)
            })
          q"""
            final case class $parentClsName(..$zClsParentArgs, $parentArg) extends $zDirectSuper {
              type Value = $name
            
              def put($vArg: RootValue): $zName = new $zName(..$newArgsForPut, ${parentArg.name})
              protected def toStringAsParent() = {
                ${zName.encoded} + "(" + $argsToString + ")" + parentString
              }
            }
            """
        })

      val zTypes = args.map(a => {
        val d = extractDeclaration(a)
        val Ident(typName) = d.tpt
        addZ(typName.toTypeName)
      })
      val zUnapplyValues = args.map(a => {
        val d = extractDeclaration(a)
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
          ${buildCastMethod(name)}
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