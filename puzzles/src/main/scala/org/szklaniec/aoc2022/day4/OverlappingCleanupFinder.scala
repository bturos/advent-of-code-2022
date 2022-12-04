package org.szklaniec.aoc2022.day4

import scala.util.{Failure, Success, Try}

trait OverlappingCleanupFinder {

  def findFullOverlaps(lines: List[String], overlapStrategy: OverlapStrategy): Try[Int] = {
    lines.foldLeft(Try(0)) { (totalOverlapCounter, line) =>
      for {
        overlapCounter <- totalOverlapCounter
        (firstSectionsString, secondSectionsString) <- line.split(",") match {
          case Array(firstList, secondList) => Success((firstList, secondList))
          case _                            => Failure(new IllegalArgumentException(s"Unsupported line format: $line"))
        }
        firstRange <- getSectionsRange(firstSectionsString)
        secondRange <- getSectionsRange(secondSectionsString)
        overlapCondition = overlapStrategy.matches(firstRange, secondRange)
      } yield if (overlapCondition) overlapCounter + 1 else overlapCounter
    }
  }

  private[this] def getSectionsRange(sectionsString: String): Try[Range.Inclusive] = {
    for {
      sectionRangeEnds <- Try(sectionsString.split("-"))
      (rangeStart, rangeEnd) <- sectionRangeEnds match {
        case Array(rangeStartString, rangeEndString) => Try((rangeStartString.toInt, rangeEndString.toInt))
        case _ => Failure(new IllegalArgumentException(s"Unsupported sections string: $sectionsString"))
      }
      range <-
        if (rangeStart < 1 || rangeEnd < 1 || rangeEnd < rangeStart)
          Failure(new IllegalArgumentException(s"Invalid sections range expression: $sectionsString"))
        else Success(Range.inclusive(rangeStart, rangeEnd))
    } yield range
  }

}

sealed trait OverlapStrategy {
  def matches(firstRange: Range, secondRange: Range): Boolean
}

case object TotalOverlapStrategy extends OverlapStrategy {
  override def matches(firstRange: Range, secondRange: Range): Boolean = {
    val intersection = firstRange.intersect(secondRange)
    intersection == firstRange || intersection == secondRange
  }
}

case object AnyOverlapStrategy extends OverlapStrategy {
  override def matches(firstRange: Range, secondRange: Range): Boolean = {
    firstRange.intersect(secondRange).nonEmpty
  }
}
