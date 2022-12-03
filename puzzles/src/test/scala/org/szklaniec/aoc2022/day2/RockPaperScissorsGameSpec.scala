package org.szklaniec.aoc2022.day2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.szklaniec.aoc2022.PuzzleSolvingSupport

import scala.util.{Failure, Success}

class RockPaperScissorsGameSpec extends AnyFlatSpec with Matchers with RockScissorPaperGame {

  it should "find score using always-win strategy" in {
    // given
    val filename = "rock-scissor-paper.txt"

    // when
    val sumOfPoints = PuzzleSolvingSupport.solvePuzzleUsingFile(filename, findSumOfPoints(_, SelectActionToWinRoundGameStrategy))

    // then
    sumOfPoints match {
      case Success(calories) =>
        calories shouldBe 13484
      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

  it should "find score using expected round outcome strategy" in {
    // given
    val filename = "rock-scissor-paper.txt"

    // when
    val sumOfPoints = PuzzleSolvingSupport.solvePuzzleUsingFile(filename, findSumOfPoints(_, SelectActionToAchieveRoundOutcomeGameStrategy))

    // then
    sumOfPoints match {
      case Success(calories) =>
        calories shouldBe 13433
      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

}
