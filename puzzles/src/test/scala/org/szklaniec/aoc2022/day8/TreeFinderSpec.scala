package org.szklaniec.aoc2022.day8

import com.typesafe.scalalogging.StrictLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.szklaniec.aoc2022.PuzzleSolvingSupport

import scala.util.{Failure, Success}

class TreeFinderSpec extends AnyFlatSpec with Matchers with TreeFinder with StrictLogging {

  it should "find all visible trees in the grid" in {
    // given
    val filename = "trees.txt"

    // when
    val visibleTreesCount = PuzzleSolvingSupport.executeLogicWithFile(filename, findTrees(_, SelectVisibleTreesStrategy))

    // then
    visibleTreesCount match {
      case Success(count) =>
        count shouldBe 1807
      case Failure(error) => fail("Unexpected error", error)
    }
  }

  it should "find all trees in the grid" in {
    // given
    val filename = "trees.txt"

    // when
    val allTreesCount = PuzzleSolvingSupport.executeLogicWithFile(filename, findTrees(_, SelectAllTreesStrategy))

    // then
    allTreesCount match {
      case Success(count) =>
        count shouldBe 9801
      case Failure(error) => fail("Unexpected error", error)
    }
  }

  it should "find highest scenic view score grid" in {
    // given
    val filename = "trees.txt"

    // when
    val highestScenicScore = PuzzleSolvingSupport.executeLogicWithFile(filename, findTrees(_, SelectTreeWithHighestScenicScoreStrategy))

    // then
    highestScenicScore match {
      case Success(count) =>
        count shouldBe 480000
      case Failure(error) => fail("Unexpected error", error)
    }
  }

}
