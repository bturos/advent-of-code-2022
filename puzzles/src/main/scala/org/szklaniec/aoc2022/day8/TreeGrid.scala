package org.szklaniec.aoc2022.day8

import org.szklaniec.aoc2022.day8.TreeGrid._

case class TreeGrid(
    trees: Map[TreeCoordinates, Tree],
    numberOfRows: Int,
    numberOfCols: Int
) {

  def createXPath(x: Int): List[Tree] = {
    trees.values
      .filter(_.coords.x == x)
      .toList
      .sorted(xPathOrdering)
  }

  def createYPath(y: Int): List[Tree] = {
    trees.values
      .filter(_.coords.y == y)
      .toList
      .sorted(yPathOrdering)
  }

  def isOutsideTree(tree: Tree): Boolean = {
    tree.coords.x == 0 ||
    tree.coords.x == numberOfCols - 1 ||
    tree.coords.y == 0 ||
    tree.coords.y == numberOfRows - 1
  }

}

object TreeGrid {
  val xPathOrdering: Ordering[Tree] = (x: Tree, y: Tree) => x.coords.y.compare(y.coords.y)
  val yPathOrdering: Ordering[Tree] = (x: Tree, y: Tree) => x.coords.x.compare(y.coords.x)
}
