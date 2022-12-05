package org.szklaniec.aoc2022.day5

import cats.syntax.all._
import org.szklaniec.aoc2022.LineWithNumber

import scala.util.{Failure, Success, Try}

private[day5] trait WarehouseReader {

  private[day5] def loadWarehouse(lines: List[LineWithNumber]): Try[Warehouse] = {
    lines.reverse match {
      case indexLine :: allStackLines =>
        for {
          stackIndices <- readIndexLine(indexLine.content)
          warehouse <- readWarehouse(stackIndices.length, allStackLines)
        } yield warehouse
      case _ => Failure(new IllegalArgumentException(s"Invalid warehouse definition:\n${lines.mkString("\n")}"))
    }
  }

  private[this] def readIndexLine(indexLine: String): Try[List[Int]] = {
    Try {
      indexLine.trim
        .split(" ")
        .filter(_.nonEmpty)
        .map(_.toInt)
        .toList
    }
  }

  private[this] def readWarehouse(expectedMaximumCratesPerLevel: Int, cratesLevelLines: List[LineWithNumber]): Try[Warehouse] = {
    val warehouse = Warehouse(
      cratesLevelLines
        .map(line => readCratesLevel(line.content))
        .foldLeft(Map.empty[Int, List[Crate]]) { case (stacksSoFar, cratesOnLevel) =>
          val cratesOnLevelByStack = cratesOnLevel.zipWithIndex
            .filterNot(_._1 == Crate.empty)
            .map(crateWithIndex => crateWithIndex._2 -> crateWithIndex._1)
            .toMap

          (stacksSoFar.keySet ++ cratesOnLevelByStack.keySet).map { stackIndex =>
            (cratesOnLevelByStack.get(stackIndex), stacksSoFar.get(stackIndex)) match {
              case (None, None)                           => stackIndex -> List.empty
              case (Some(newCrate), None)                 => stackIndex -> List(newCrate)
              case (None, Some(crates))                   => stackIndex -> crates
              case (Some(newCrate), Some(previousCrates)) => stackIndex -> (newCrate :: previousCrates)
            }
          }.toMap
        }
    )

    warehouse.stacks.values.toList
      .traverse { stack =>
        if (stack.length > expectedMaximumCratesPerLevel) Failure(new IllegalArgumentException("Invalid number of crates on level!"))
        else Success(stack)
      }
      .map(_ => warehouse)

  }

  private[this] def readCratesLevel(cratesLevelAsString: String): List[Crate] = {
    cratesLevelAsString.toList
      .grouped(4)
      .map(_.take(3)(1))
      .map(Crate(_))
      .toList
  }

}
