package org.szklaniec.aoc2022

import scala.io.Source
import scala.util.{Try, Using}

object PuzzleSolvingSupport {

  def solvePuzzleUsingFile[T](filename: String, puzzleSolvingFn: List[LineWithNumber] => Try[T]): Try[T] = {
    Using(Source.fromResource(filename)) { source =>
      val lines = source.getLines().toList
      val linesWithNumbers = lines
        .zip(Range.inclusive(1, lines.size))
        .map { case (content, number) =>
          LineWithNumber(content, number)
        }
      puzzleSolvingFn(linesWithNumbers)
    }.flatten
  }

}

case class LineWithNumber(content: String, number: Int)
