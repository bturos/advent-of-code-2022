package org.szklaniec.aoc2022.day7

import com.typesafe.scalalogging.StrictLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.szklaniec.aoc2022.PuzzleSolvingSupport

import scala.util.{Failure, Success}

class DirectoryTreeAnalyzerSpec extends AnyFlatSpec with Matchers with DirectoryTreeAnalyzer with StrictLogging {

  it should "find total size of directories not bigger than 10k" in {
    // given
    val filename = "directory-traverse.txt"

    // when
    val calculatedSize = PuzzleSolvingSupport.executeLogicWithFile(filename, calculateSize(_, AtMost10k))

    // then
    calculatedSize match {
      case Success(size) =>
        size shouldBe 1517599
      case Failure(error) => fail("Unexpected error", error)
    }
  }

  it should "find the smallest directory big enough to free 30M of space" in {
    // given
    val filename = "directory-traverse.txt"

    // when
    val calculatedSize = PuzzleSolvingSupport.executeLogicWithFile(filename, calculateSize(_, SmallestAndBiggerThan30MB))

    // then
    calculatedSize match {
      case Success(size) =>
        size shouldBe 2481982
      case Failure(error) => fail("Unexpected error", error)
    }
  }

}
