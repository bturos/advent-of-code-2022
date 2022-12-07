package org.szklaniec.aoc2022.day6

import com.typesafe.scalalogging.StrictLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.szklaniec.aoc2022.PuzzleSolvingSupport

import scala.util.{Failure, Success}

class StartOfPacketDetectorSpec extends AnyFlatSpec with Matchers with StartOfPacketDetector with StrictLogging {

  it should "find the start of packet marker" in {
    // given
    val filename = "message-markers.txt"

    // when
    val markerPosition = PuzzleSolvingSupport.executeLogicWithFile(filename, findStartOfEntity(_, 4))

    // then
    markerPosition match {
      case Success(position) =>
        position shouldBe 1790
      case Failure(error) => fail("Unexpected error", error)
    }
  }

  it should "find the start of message marker" in {
    // given
    val filename = "message-markers.txt"

    // when
    val markerPosition = PuzzleSolvingSupport.executeLogicWithFile(filename, findStartOfEntity(_, 14))

    // then
    markerPosition match {
      case Success(position) =>
        position shouldBe 2837
      case Failure(error) => fail("Unexpected error", error)
    }
  }

}
