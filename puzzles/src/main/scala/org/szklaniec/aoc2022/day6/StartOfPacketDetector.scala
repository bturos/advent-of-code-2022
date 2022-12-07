package org.szklaniec.aoc2022.day6

import org.slf4j.{Logger, LoggerFactory}
import org.szklaniec.aoc2022.LineWithNumber

import scala.util.{Failure, Success, Try}

private[day6] trait StartOfPacketDetector {

  private val logger: Logger = LoggerFactory.getLogger(classOf[StartOfPacketDetector])

  def findStartOfEntity(lines: List[LineWithNumber], markerLength: Int): Try[Int] = {
    validateInput(lines)
      .flatMap(input =>
        findMarkerEndIndex(input, markerLength) match {
          case Some(position) => Success(position)
          case None           => Failure(new IllegalArgumentException("Could not find start of packet position!"))
        }
      )
  }

  private[this] def validateInput(lines: List[LineWithNumber]): Try[String] = {
    lines match {
      case List(theLine) => Success(theLine.content.trim)
      case _ :: _        => Failure(new IllegalArgumentException("Unsupported file format with multiple lines"))
      case Nil           => Failure(new IllegalArgumentException("Unsupported empty file"))
    }
  }

  private[this] def findMarkerEndIndex(input: String, markerLength: Int): Option[Int] = {
    input
      .sliding(markerLength, 1)
      .zipWithIndex
      .find { case (markerCandidate, _) =>
        markerCandidate.toSet.size == markerLength
      }
      .map { case (marker, position) =>
        logger.debug(s"Found start of packet marker [$marker] at position [$position]")
        position
      }
  }
}
