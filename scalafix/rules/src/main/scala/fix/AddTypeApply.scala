package fix

import metaconfig.Configured
import scalafix.patch.Patch
import scalafix.util.TokenOps
import scalafix.v1.{SemanticType, _}

import scala.meta._
import scala.meta.contrib.Trivia
import scala.meta.internal.pc.ScalafixGlobal
import scala.meta.internal.proxy.GlobalProxy
import scala.meta.internal.trees.Quasi
import scala.meta.tokens.Token
import scala.meta.transversers.SimpleTraverser
import scala.tools.nsc.reporters.StoreReporter
import scala.util.{Failure, Success, Try}

class AddTypeApply(global: ScalafixGlobal) extends SemanticRule("AddTypeApply") {
  override def description: String = "infer type apply"
  def this() = this(null)

  override def withConfiguration(config: Configuration): Configured[Rule] = {
    if (config.scalacClasspath.isEmpty) {
      Configured.error(s"config.scalacClasspath should not be empty")
    } else {
      val global = CompilerUtils.newGlobal(config.scalacClasspath, config.scalacOptions)
      global match {
        case Success(settings) => Configured.ok(new AddTypeApply(new ScalafixGlobal(settings, new StoreReporter, Map())))
        case Failure(exception) => Configured.error(exception.getMessage)
      }
    }
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    import global._

    println(doc.synthetics.toList)

    doc.synthetics.collect {
      case synt@TypeApplyTree(function: SemanticTree, typeArguments: List[SemanticType]) =>
        val original = getTree(function).get
//        println(s"original.structure = ${original.structure}")
//        println(s"original = ${original}")
//        println(s" original.tokens.last = ${original.tokens.last}")

        println(s"original.pos.start = ${original.pos.start}")
        val treeInGlobal: global.analyzer.global.Tree = getTreeInGlobal(original.tokens.last).get
        val symbols = typeArguments.flatMap(ta => getSymbol(ta))


        //        println(s"symbols = ${symbols}")
//        println(s"original = ${original}")
//        println(s"treeInGlobal = ${treeInGlobal}")
//        println(s"treeInGlobal.show = ${treeInGlobal.tpe}")
//        println(s" = ${showRaw(treeInGlobal)}")

//        println(s"global.showRaw(treeInGlobal) = ${global.showRaw(treeInGlobal)}")

        println(s"treeInGlobal.tpe = ${treeInGlobal.tpe}")
        treeInGlobal match {
          case t @ Apply(f, tree) =>
            println(s"synt $synt")
            println(s"f = ${f.toString()}")
            println(s"tree.head.tpe = ${tree.map(_.tpe)}")

          case t =>   println(s"args je ne sais pas")
        }


        val gsymbols = symbols.flatMap { sym =>
          global.inverseSemanticdbSymbols(sym.toString())
            .find(s => global.semanticdbSymbol(s) == sym.value)
        }
        if (gsymbols.length < typeArguments.length) Patch.empty
        else {
          val types = gsymbols.map(gsym => {
            if(gsym.isLocalToBlock) gsym.decodedName else gsym.fullName

          })
          (for {
            originalTree <- getTree(synt)
            replace <- if (synt.toString.startsWith("*.apply")) Some(".apply")
            else if (synt.toString.startsWith("*[")) Some("")
            else None
          } yield Patch.addRight(originalTree, s"${replace}[${types.mkString(", ")}]")
            ).getOrElse(Patch.empty)
        }

    }.toList.asPatch
  }

  def getTree(semanticTree: SemanticTree): Option[Tree] = {
    semanticTree match {
      case IdTree(info) => None
      case SelectTree(qualifier, id) => getTree(qualifier)
      case ApplyTree(function, arguments) =>getTree(function)
      case TypeApplyTree(function, typeArguments) => getTree(function)

      case FunctionTree(parameters, body) => getTree(body)
      case LiteralTree(constant) => None
      case MacroExpansionTree(beforeExpansion, tpe) => None
      case OriginalSubTree(tree) => Some(tree)
      case OriginalTree(tree) => Some(tree)
      case NoTree => None
    }

  }
  def getName(t: Tree): Option[Term.Name] = {
    t match {
      case t@Term.Select(fun , name) => Some(name)
      case t: Term.Name => Some(t)
      case _ => None
    }
  }

  def getSymbol(tpe: SemanticType): Option[Symbol] = {
    tpe match {
      case TypeRef(prefix, symbol, typeArguments) =>
        Some(symbol)

      case SingleType(prefix, symbol) =>
        Some(symbol)
      case ThisType(symbol) => Some(symbol)
      case SuperType(prefix, symbol) => Some(symbol)
      case ConstantType(constant) => None
      case IntersectionType(types) => None
      case UnionType(types) => None
      case WithType(types) => None
      case StructuralType(tpe, declarations) => None
      case AnnotatedType(annotations, tpe) => None
      case ExistentialType(tpe, declarations) => None
      case UniversalType(typeParameters, tpe) => None
      case ByNameType(tpe) => None
      case RepeatedType(tpe) => None
      case NoType => None
    }
  }


  private def fixDefinition(defn: Defn, name: Term.Name, body: Term)(implicit doc: SemanticDocument): Patch = {
    (for {
      (replace, spaces) <- getReplaceAndSpaces(defn, body)
      explicitType <- getTypeAsSeenFromGlobal(name)
      filteredType <- filterType(explicitType)
      //      _ = println(s"filteredType.prefixString = ${filteredType.prefix}")
    } yield Patch.addRight(replace, s"$spaces: ${filteredType.finalResultType}")
      ).getOrElse(Patch.empty)
  }

  private def getTypeAsSeenFromGlobal(name: Term.Name)(implicit doc: SemanticDocument): Option[global.Type] = {
    for {
      context <- getContext(name)
      finalType = context.tree.symbol.info
    } yield finalType
  }

  private def filterType(finalType: global.Type): Option[global.Type] = {
    finalType match {
      case f if f.isInstanceOf[scala.reflect.runtime.universe.ConstantType] =>
        None // don't annotate ConstantTypes
      case f if f.toString().contains("#") && f.toString().contains(".type") =>
        None // don't annotate types that look like fix.WidenSingleType#strings.type
      //Todo: add a special case for structural type: remove implicit and replace lazy val by a def
      //Todo: deal with the root prefix to avoid cyclical types
      //Todo: remove super types: we don't infer them
      case f => Some(f)
    }
  }


  private def getReplaceAndSpaces(defn: Defn, body: Term)(implicit doc: SemanticDocument): Option[(Token, String)] = {
    val tokens = doc.tokenList
    import tokens._

    for {
      start <- defn.tokens.headOption
      end <- body.tokens.headOption
      lhsTokens = slice(start, end)
      replace <- lhsTokens.reverseIterator.find(x =>
        !x.is[Token.Equals] && !x.is[Trivia]
      )
      space = {
        if (TokenOps.needsLeadingSpaceBeforeColon(replace)) " "
        else ""
      }
    } yield (replace, space)
  }

  private def getTreeInGlobal(name: Token)(implicit doc: SemanticDocument): Option[global.Tree] = {
    val unit = global.newCompilationUnit(doc.input.text, doc.input.syntax)
    val gpos = unit.position(name.pos.end)
    println(s"name.pos.end = ${name.pos.end}")
    val gtree = GlobalProxy.typedTreeAt(global, gpos)
    println(s"global.showRaw(gtree) = ${global.showRaw(gtree)}")
    val context = global.doLocateContext(gpos)
    val gtree2 = GlobalProxy.typedTreeAt(global, context.tree.pos)
    println(s"global.showRaw(gtree2) = ${global.showRaw(gtree2)}")
    Option(gtree2.asInstanceOf[global.Tree])
  }

  private def getContext(name: Term)(implicit doc: SemanticDocument): Option[global.Context] = {
    val unit = global.newCompilationUnit(doc.input.text, doc.input.syntax)
    val gpos = unit.position(name.pos.start)
    GlobalProxy.typedTreeAt(global, gpos)
    Try(global.doLocateContext(gpos)).toOption
  }
  
  private def getContext(name: global.Symbol)(implicit doc: SemanticDocument): Option[global.Context] = {
    val unit = global.newCompilationUnit(doc.input.text, doc.input.syntax)
    val gpos = unit.position(name.pos.start)
    GlobalProxy.typedTreeAt(global, gpos)
    Try(global.doLocateContext(gpos)).toOption
  }
}