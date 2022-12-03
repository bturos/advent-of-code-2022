package org.szklaniec.aoc2022

import scala.io.Source
import scala.util.{Try, Using}

object PuzzleSolvingSupport {

  def solvePuzzleUsingFile[T](filename: String, puzzleSolvingFn: List[String] => Try[T]): Try[T] = {
    Using(Source.fromResource(filename)) { source =>
      puzzleSolvingFn(source.getLines().toList)
    }.flatten
  }

}
