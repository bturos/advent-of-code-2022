package org.szklaniec.aoc2022.day1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.szklaniec.aoc2022.PuzzleSolvingSupport

import scala.util.{Failure, Success}

class ElfCaloriesFinderSpec extends AnyFlatSpec with Matchers with ElfCaloriesFinder {

  it should "find maximum calories across all elves" in {
    // given
    val filename = "elvishCalories.txt"

    // when
    val caloriesByElf = PuzzleSolvingSupport.solvePuzzleUsingFile(filename, collectElvishCalories)

    // then
    caloriesByElf match {
      case Success(calories) =>
        calories.size shouldBe 255
        calories.max shouldBe 66487
      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

  it should "find top 3 elves with maximum calories" in {
    // given
    val filename = "elvishCalories.txt"

    // when
    val caloriesByElf = PuzzleSolvingSupport.solvePuzzleUsingFile(filename, collectElvishCalories)

    // then
    caloriesByElf match {
      case Success(calories) =>
        calories.size shouldBe 255
        calories.sorted.reverse.take(3) should contain only (66487, 65413, 65401)
        calories.sorted.reverse.take(3).sum shouldBe 197301
      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

}
