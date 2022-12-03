package org.szklaniec.aoc2022.day3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.szklaniec.aoc2022.PuzzleSolvingSupport

import scala.util.{Failure, Success}

class RucksackContentPriorityCalculatorSpec extends AnyFlatSpec with Matchers with RucksackContentPriorityCalculator {

  it should "find sum of priorities for common items in rucksacks" in {
    // given
    val filename = "rucksacks.txt"

    // when
    val sumOfPoints = PuzzleSolvingSupport.solvePuzzleUsingFile(filename, collectCommonItemsPriorities)

    // then
    sumOfPoints match {
      case Success(calories) =>
        calories shouldBe 8298
      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

}
