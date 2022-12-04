package org.szklaniec.aoc2022.day3

import org.szklaniec.aoc2022.{ErrorWithLineNumberOps, LineWithNumber}

import scala.util.{Failure, Success, Try}

private[day3] trait RucksackContentPriorityCalculator {

  def collectCommonItemsPriorities(lines: List[LineWithNumber]): Try[Int] = {
    lines.foldLeft(Try(0)) { case (sumOfPriorities, LineWithNumber(line, lineNumber)) =>
      (for {
        sumOfPrioritiesSoFar <- sumOfPriorities
        (firstCompartment, secondCompartment) <- getCompartments(line.trim)
        commonItems = firstCompartment.intersect(secondCompartment)
        commonItemsTypes = commonItems.toCharArray.distinct
        sumOfRucksackPriorities = commonItemsTypes.map(getPriorityForItem).sum
      } yield sumOfPrioritiesSoFar + sumOfRucksackPriorities)
        .recoverWithLineNumber(lineNumber)
    }
  }

  private[this] def getCompartments(rucksackContent: String): Try[(String, String)] = {
    rucksackContent match {
      case line if line.nonEmpty && line.length % 2 == 0 =>
        Success((line.take(line.length / 2), line.drop(line.length / 2)))
      case _ => Failure(new IllegalArgumentException(s"Unsupported rucksack content: $rucksackContent"))
    }
  }

}
