package org.szklaniec.aoc2022.day1

import scala.io.Source
import scala.util.{Try, Using}

object ElfCaloriesFinder {

  case class ElfCalories(
      caloriesSoFar: Int
  )

  def collectElvishCalories(fileName: String): Try[List[Int]] = {
    Using(Source.fromResource(fileName)) { source =>
      source.getLines().foldLeft((List.empty[Int], List.empty[Int])) { case ((allCaloriesList, currentElfCaloriesList), line) =>
        line.trim match {
          case ""                  => (currentElfCaloriesList.sum :: allCaloriesList, List.empty)
          case caloriesStringValue => (allCaloriesList, caloriesStringValue.toInt :: currentElfCaloriesList)
        }
      }
    }.map { case (allCaloriesList, currentElfCalories) =>
      currentElfCalories.sum :: allCaloriesList
    }
  }
}
