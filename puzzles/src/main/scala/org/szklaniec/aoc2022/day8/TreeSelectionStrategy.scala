package org.szklaniec.aoc2022.day8

import scala.util.{Success, Try}

private[day8] sealed trait TreeSelectionStrategy {
  def countTrees(treeGrid: TreeGrid): Try[Int]
}

private[day8] case object SelectVisibleTreesStrategy extends TreeSelectionStrategy {

  override def countTrees(treeGrid: TreeGrid): Try[Int] = {
    Try {
      val xPaths = Range(0, treeGrid.numberOfCols)
        .map(treeGrid.createXPath)
        .toList

      val yPaths = Range(0, treeGrid.numberOfRows)
        .map(treeGrid.createYPath)
        .toList

      treeGrid.trees.values
        .filter { tree =>
          isTreeVisibleOnPath(tree, xPaths(tree.coords.x)) ||
          isTreeVisibleOnPath(tree, yPaths(tree.coords.y))
        }
        .toList
        .length
    }
  }

  private[this] def isTreeVisibleOnPath(tree: Tree, paths: List[Tree]) = {
    val treeIndexInPath = paths.indexOf(tree)
    val (firstPath, secondPath) = paths.splitAt(treeIndexInPath)
    isTreeVisibleOnDirection(tree, firstPath) ||
    isTreeVisibleOnDirection(tree, secondPath)
  }

  private[this] def isTreeVisibleOnDirection(tree: Tree, treePath: List[Tree]): Boolean = {
    treePath.filterNot(_ == tree) match {
      case Nil                                                  => true
      case elements if elements.exists(_.height >= tree.height) => false
      case _                                                    => true
    }
  }

}

private[day8] case object SelectAllTreesStrategy extends TreeSelectionStrategy {
  override def countTrees(treeGrid: TreeGrid): Try[Int] =
    Success(treeGrid.trees.values.toList.length)
}

private[day8] case object SelectTreeWithHighestScenicScoreStrategy extends TreeSelectionStrategy {
  override def countTrees(treeGrid: TreeGrid): Try[Int] = Try {
    val xPaths = Range(0, treeGrid.numberOfCols)
      .map(treeGrid.createXPath)
      .toList

    val yPaths = Range(0, treeGrid.numberOfRows)
      .map(treeGrid.createYPath)
      .toList

    treeGrid.trees.values.map { tree =>
      calculateScenicScoreForTree(tree, xPaths(tree.coords.x), yPaths(tree.coords.y))
    }.max
  }

  private[this] def calculateScenicScoreForTree(tree: Tree, xPath: List[Tree], yPath: List[Tree]): Int = {
    calculateScenicScoreForPath(tree, xPath) * calculateScenicScoreForPath(tree, yPath)
  }

  private[this] def calculateScenicScoreForPath(tree: Tree, path: List[Tree]): Int = {
    val treeIndexInPath = path.indexOf(tree)
    val (firstPath, secondPath) = path.splitAt(treeIndexInPath)
    calculateScenicViewForDirection(tree, firstPath.reverse) * calculateScenicViewForDirection(tree, secondPath)
  }

  private[this] def calculateScenicViewForDirection(tree: Tree, path: List[Tree]): Int = {
    path.filterNot(_ == tree) match {
      case Nil => 0
      case treesOnPath =>
        treesOnPath.find(_.height >= tree.height) match {
          case Some(blockingTree) => treesOnPath.indexOf(blockingTree) + 1
          case None               => treesOnPath.length
        }
    }
  }
}
