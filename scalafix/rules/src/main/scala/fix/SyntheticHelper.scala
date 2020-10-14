package fix

import scalafix.Patch
import scalafix.v1.{OriginalTree, SemanticDocument, SemanticTree}

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.meta.Tree
import scalafix.v1._
import scala.meta._


object SyntheticHelper {
  def buildPatch(tree: Tree, s: List[SemanticTree])(implicit doc: SemanticDocument): Patch = {

    filterSynthetics(s) match {
      case Some(semanticTree) =>     val place = Place.where('*', semanticTree)
        val semanticTreeString = semanticTree.toString(250)
        place match {
          case Place.Left => {
            val value = semanticTreeString.drop(1)
            Patch.addRight(tree, value)
          }
          case Place.Right =>
            val value = semanticTreeString.dropRight(1)
            Patch.addLeft(tree, value)

          case Place.Middle(pos) =>
            val (left, right) = semanticTreeString.splitAt(pos)
            Patch.addRight(tree, left).+(Patch.addLeft(tree, right)) // won't work. We need to create only one patch
          case Place.NoWhere => Patch.empty
        }
      case None => Patch.empty
    }
  }

  def buildPatch(tree: Tree, s: SemanticTree): Patch =
    filterSynthetics(List(s)) match {
      case Some(value) => Patch.addRight(tree, value.toString())
      case None => Patch.empty
    }

  private def filterSynthetics(s: List[SemanticTree]): Option[SemanticTree] = {
    assert(s.length == 1 , s"Synthetics should always be a one element list, but it's not: $s")
    val semanticTree = s.head
    if (semanticTree.toString().contains("forSome")) None // forSome is not handled by dotty
    else Some(semanticTree)
  }
}
