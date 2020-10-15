package fix

import metaconfig.Configured
import scalafix.patch.Patch
import scalafix.util.TokenOps
import scalafix.v1._

import scala.meta._
import scala.meta.contrib.Trivia
import scala.meta.internal.pc.ScalafixGlobal
import scala.meta.internal.proxy.GlobalProxy
import scala.meta.tokens.Token
import scala.tools.nsc.reporters.StoreReporter
import scala.util.{Failure, Success, Try}

class MigrationRule(global: ScalafixGlobal) extends SemanticRule("MigrationRule") {
  override def description: String = "infer types and show synthetics"

  def this() = this(null)

  override def withConfiguration(config: Configuration): Configured[Rule] = {
    if (config.scalacClasspath.isEmpty) {
      Configured.error(s"config.scalacClasspath should not be empty")
    } else {
      val global = CompilerUtils.newGlobal(config.scalacClasspath, config.scalacOptions)
      global match {
        case Success(settings) => Configured.ok(new MigrationRule(new ScalafixGlobal(settings, new StoreReporter, Map())))
        case Failure(exception) => Configured.error(exception.getMessage)
      }
    }
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    //    println(s"doc.synthetics.toList = ${doc.synthetics.toList}")
    //    doc.synthetics.collect {
    //      case t@IdTree(p) => println(s"IdTree(p) = ${IdTree(p)}")
    //      case t@SelectTree(qualifier, id) => println(s"IdTree(p) = ${SelectTree(qualifier, id)}")
    //      case t@ApplyTree(function: SemanticTree, arguments: List[SemanticTree]) =>
    //      case t@TypeApplyTree(function: SemanticTree, typeArguments: List[SemanticType]) => println(s"TypeApplyTree(function, typeArguments) = ${TypeApplyTree(function, typeArguments)}")
    //      case t@FunctionTree(parameters: List[IdTree], body: SemanticTree) => println(s"FunctionTree(parameters, body) = ${FunctionTree(parameters, body)}")
    //      case t@LiteralTree(constant: Constant) => println(s"LiteralTree(constant) = ${LiteralTree(constant)}")
    //      case t@MacroExpansionTree(beforeExpansion: SemanticTree, tpe: SemanticType) => println(s"MacroExpansionTree(beforeExpansion, tpe) = ${MacroExpansionTree(beforeExpansion, tpe)}")
    //      case t@OriginalSubTree(tree: scala.meta.Tree) => println(s"OriginalSubTree(tree) = ${OriginalSubTree(tree)}")
    //      case t@OriginalTree(tree: scala.meta.Tree) => println(s"OriginalTree(tree) = ${OriginalTree(tree)}")
    //      case t@ NoTree => println(s"t NoTree = ${t}")
    //      case t => println(s"t ici et encore = ${t}")
    //    }.toList

    val patch1 = doc.tree.collect {
      case t@Defn.Val(mods, Pat.Var(name) :: Nil, None, body) => {
        fixDefinition(t, name, body)
      }
      case t@Defn.Var(mods, Pat.Var(name) :: Nil, None, Some(body)) =>
        fixDefinition(t, name, body)

      case t@Defn.Def(mods, name, _, _, None, body) =>
        fixDefinition(t, name, body)
    }.asPatch

    val patch2 = addSynthetics()
    patch1 + patch2
  }

  private def addSynthetics()(implicit doc: SemanticDocument): Patch = {
    doc.synthetics.foreach(t => println(s"t.productPrefix = $t => ${t.structure} "))
    doc.synthetics.collect {
      case t@ApplyTree(function: SemanticTree, arguments: List[SemanticTree]) if(!t.toString().contains("$"))=>
        function match {
          case f@OriginalTree(tree) => SyntheticHelper.buildPatch(tree, List(t))
          case value => Patch.empty
        }
      case t@TypeApplyTree(function: SemanticTree, typeArguments: List[SemanticType]) =>
//        println(s"typeArguments = ${typeArguments}")
        function match {
          case f @OriginalTree(sub) =>
            if (sub.isInstanceOf[Term.ApplyInfix]) Patch.empty
            else SyntheticHelper.buildPatch(f.tree, List(t))
          case s@SelectTree(qualifier, id) =>
            qualifier match {
              case OriginalTree(subtree) =>
                SyntheticHelper.buildPatch(subtree, List(t))
              case _ => Patch.empty
            }
          case value =>
            Patch.empty
        }
      case t@SelectTree(qualifier: SemanticTree, id: IdTree)  =>
        qualifier match {
          case f@OriginalTree(tree) => SyntheticHelper.buildPatch(tree, List(t))
          case value =>
            Patch.empty
        }
      case t =>
        Patch.empty


      //      case t@FunctionTree(parameters: List[IdTree], body: SemanticTree) => println(s"FunctionTree(parameters, body) = ${FunctionTree(parameters, body)}")
      //      case t@LiteralTree(constant: Constant) => println(s"LiteralTree(constant) = ${LiteralTree(constant)}")
      //      case t@MacroExpansionTree(beforeExpansion: SemanticTree, tpe: SemanticType) => println(s"MacroExpansionTree(beforeExpansion, tpe) = ${MacroExpansionTree(beforeExpansion, tpe)}")
      //      case t@OriginalSubTree(tree: scala.meta.Tree) => println(s"OriginalSubTree(tree) = ${OriginalSubTree(tree)}")
      //      case t@OriginalTree(tree: scala.meta.Tree) => println(s"OriginalTree(tree) = ${OriginalTree(tree)}")
      //      case t@NoTree => println(s"t NoTree = ${t}")
    }.toList.asPatch
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

  private def getContext(name: Term.Name)(implicit doc: SemanticDocument): Option[global.Context] = {
    val unit = global.newCompilationUnit(doc.input.text, doc.input.syntax)
    val gpos = unit.position(name.pos.start)
    GlobalProxy.typedTreeAt(global, gpos)
    Try(global.doLocateContext(gpos)).toOption
  }
}