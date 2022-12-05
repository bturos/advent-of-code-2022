package org.szklaniec.aoc2022.day4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.szklaniec.aoc2022.PuzzleSolvingSupport

import scala.util.{Failure, Success}

class OverlappingSectionsFinderSpec extends AnyFlatSpec with Matchers with OverlappingCleanupFinder {

  it should "find number of total overlaps in elf pairs" in {
    // given
    val filename = "section-cleanup-pairs.txt"

    // when
    val totalOverlapCounter = PuzzleSolvingSupport.executeLogicWithFile(filename, findFullOverlaps(_, TotalOverlapStrategy))

    // then
    totalOverlapCounter match {
      case Success(totalOverlaps) =>
        totalOverlaps shouldBe 490
      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

  it should "find number of any overlaps in elf pairs" in {
    // given
    val filename = "section-cleanup-pairs.txt"

    // when
    val totalOverlapCounter = PuzzleSolvingSupport.executeLogicWithFile(filename, findFullOverlaps(_, AnyOverlapStrategy))

    // then
    totalOverlapCounter match {
      case Success(totalOverlaps) =>
        totalOverlaps shouldBe 921
      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

}
