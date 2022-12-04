package org.szklaniec.aoc2022.day3

import cats.syntax.all._
import org.szklaniec.aoc2022.LineWithNumber

import scala.util.{Failure, Success, Try}

private[day3] trait ElfBadgeFinder {

  case class BadgeGroup(
      first: String,
      second: String,
      third: String
  )

  def calculateBadgePriorities(lines: List[LineWithNumber]): Try[Int] = {
    for {
      _ <- validateGroupsCount(lines.map(_.content))
      badgeGroups <- createBadgeGroups(lines.map(_.content))
      commonGroupItems = badgeGroups.map { case BadgeGroup(first, second, third) =>
        first.intersect(second.intersect(third))
      }
      sumOfAllBadgePriorities = commonGroupItems
        .flatMap(groupCommonItems => groupCommonItems.toCharArray.distinct.map(getPriorityForItem).toList)
        .sum

    } yield sumOfAllBadgePriorities
  }

  private[this] def validateGroupsCount(lines: List[String]): Try[Int] = {
    if (lines.length % 3 == 0) Success(0)
    else Failure(new IllegalArgumentException(s"Number of elves is not divisible by 3: ${lines.length}"))
  }

  private[this] def createBadgeGroups(lines: List[String]) = {
    lines
      .grouped(3)
      .toList
      .traverse(groupLines => Try(BadgeGroup(groupLines.head, groupLines(1), groupLines(2))))
  }

}
