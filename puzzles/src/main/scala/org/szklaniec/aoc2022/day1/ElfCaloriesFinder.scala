package org.szklaniec.aoc2022.day1

import scala.util.Try

private[day1] trait ElfCaloriesFinder {

  def collectElvishCalories(caloriesPerElf: List[String]): Try[List[Int]] = {
    Try {
      val (allCaloriesList, lastElfCalories) = caloriesPerElf.foldLeft((List.empty[Int], List.empty[Int])) {
        case ((allCaloriesList, currentElfCaloriesList), line) =>
          line.trim match {
            case ""                  => (currentElfCaloriesList.sum :: allCaloriesList, List.empty)
            case caloriesStringValue => (allCaloriesList, caloriesStringValue.toInt :: currentElfCaloriesList)
          }
      }

      lastElfCalories.sum :: allCaloriesList
    }
  }
}
