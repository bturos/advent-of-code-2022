package org.szklaniec.aoc2022.day8

import cats.syntax.all._
import org.szklaniec.aoc2022.LineWithNumber

import scala.util.{Failure, Try}

private[day8] trait TreeFinder extends {

  def findTrees(lines: List[LineWithNumber], selectionStrategy: TreeSelectionStrategy): Try[Int] = {
    for {
      expectedNumberOfTreesInRow <- Try(lines.head.content.length)
      treesGrid <- convertLinesToTrees(lines, expectedNumberOfTreesInRow)
      selectedTreesCount <- selectionStrategy.countTrees(treesGrid)
    } yield selectedTreesCount
  }

  private[this] def convertLinesToTrees(lines: List[LineWithNumber], expectedNumberOfTreesInRow: Int): Try[TreeGrid] = {
    lines
      .traverse { line =>
        line.content match {
          case treeHeights if treeHeights.length == expectedNumberOfTreesInRow =>
            treeHeights.toCharArray.toList.zipWithIndex
              .traverse { case (char, x) =>
                Try {
                  val coords = TreeCoordinates(x, line.number - 1)
                  coords -> Tree(coords, char.toString.toInt)
                }
              }
              .map(_.toMap)

          case unsupportedLine => Failure(new IllegalArgumentException(s"Unsupported line format: $unsupportedLine"))
        }
      }
      .map(_.foldLeft(Map.empty[TreeCoordinates, Tree]) { case (acc, treesRow) =>
        acc ++ treesRow
      })
      .map(TreeGrid(_, lines.length, expectedNumberOfTreesInRow))
  }

}
