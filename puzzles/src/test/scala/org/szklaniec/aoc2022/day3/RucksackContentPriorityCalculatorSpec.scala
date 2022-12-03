package org.szklaniec.aoc2022.day3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.szklaniec.aoc2022.PuzzleSolvingSupport

import scala.util.{Failure, Success}

class RucksackContentPriorityCalculatorSpec extends AnyFlatSpec with Matchers with RucksackContentPriorityCalculator with ElfBadgeFinder {

  it should "find sum of priorities for common items in rucksacks" in {
    // given
    val filename = "rucksacks.txt"

    // when
    val sumOfPoints = PuzzleSolvingSupport.solvePuzzleUsingFile(filename, collectCommonItemsPriorities)

    // then
    sumOfPoints match {
      case Success(priorityPoints) =>
        priorityPoints shouldBe 8298
      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

  it should "find sum of priorities for group badges in rucksacks" in {
    // given
    val filename = "rucksacks.txt"

    // when
    val sumOfPoints = PuzzleSolvingSupport.solvePuzzleUsingFile(filename, calculateBadgePriorities)

    // then
    sumOfPoints match {
      case Success(priorityPoints) =>
        priorityPoints shouldBe 2708
      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

}
