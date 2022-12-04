package org.szklaniec.aoc2022.day2

import org.szklaniec.aoc2022.{ErrorWithLineNumberOps, LineWithNumber}

import scala.util.{Failure, Success, Try}

/*
Score (Shape + Outcome)
Shape: 1 for Rock, 2 for Paper, 3 for Scissors
Outcome: 0 lost, 3 draw, 6 won
 */
private[day2] trait RockScissorPaperGame {

  protected[this] def findSumOfPoints(fileLines: List[LineWithNumber], gameStrategy: GameStrategy): Try[Int] = {
    fileLines.foldLeft(Try(0)) { case (sumOfPoints, LineWithNumber(line, lineNumber)) =>
      (for {
        sumOfPointsSoFar <- sumOfPoints
        actionStrings <- Try(line.split(" "))
        (first, second) <- actionStrings match {
          case Array(first, second) => Success((first, second))
          case _                    => Failure(new IllegalArgumentException(s"Unsupported line format: $line"))
        }
        (firstAction, secondAction) <- gameStrategy.selectActions(first, second)
        newSumOfPoints = sumOfPointsSoFar + calculateRoundPoints(firstAction, secondAction)
      } yield newSumOfPoints)
        .recoverWithLineNumber(lineNumber)
    }
  }

  private[this] def calculateRoundPoints(firstAction: Action, secondAction: Action): Int = {
    val shapePoints = secondAction match {
      case Rock     => 1
      case Paper    => 2
      case Scissors => 3
    }

    val outcomePoints = (firstAction, secondAction) match {
      case (Rock, Rock)         => 3
      case (Rock, Paper)        => 6
      case (Rock, Scissors)     => 0
      case (Paper, Rock)        => 0
      case (Paper, Paper)       => 3
      case (Paper, Scissors)    => 6
      case (Scissors, Rock)     => 6
      case (Scissors, Paper)    => 0
      case (Scissors, Scissors) => 3
    }

    shapePoints + outcomePoints
  }

}

private[day2] sealed trait Action
private[day2] case object Rock extends Action
private[day2] case object Paper extends Action
private[day2] case object Scissors extends Action

private[day2] sealed trait GameStrategy {

  protected[this] def stringToAction(name: String): Try[Action] = name match {
    case "A" | "X" => Success(Rock)
    case "B" | "Y" => Success(Paper)
    case "C" | "Z" => Success(Scissors)
    case _         => Failure(new IllegalArgumentException(s"Unsupported action name: $name"))
  }

  def selectActions(firstActionName: String, secondActionName: String): Try[(Action, Action)]

}

/*
First player
A - Rock
B - Paper
C - Scissors

Second player
X - Rock
Y - Paper
Z - Scissors
 */
private[day2] object SelectActionToWinRoundGameStrategy extends GameStrategy {

  def selectActions(firstActionName: String, secondActionName: String): Try[(Action, Action)] = {
    for {
      firstAction <- stringToAction(firstActionName)
      secondAction <- stringToAction(secondActionName)
    } yield (firstAction, secondAction)
  }

}

private[day2] sealed trait Outcome
private[day2] case object Loose extends Outcome
private[day2] case object Draw extends Outcome
private[day2] case object Win extends Outcome

/*
First player
A - Rock
B - Paper
C - Scissors

Second player
X - you should loose
Y - you should draw
Z - you should win
 */
private[day2] object SelectActionToAchieveRoundOutcomeGameStrategy extends GameStrategy {

  def selectActions(firstActionName: String, secondActionName: String): Try[(Action, Action)] = {
    for {
      firstAction <- stringToAction(firstActionName)
      expectedOutcome <- stringToOutcome(secondActionName)
      secondAction = expectedOutcome match {
        case Loose =>
          firstAction match {
            case Rock     => Scissors
            case Paper    => Rock
            case Scissors => Paper
          }
        case Draw =>
          firstAction match {
            case Rock     => Rock
            case Paper    => Paper
            case Scissors => Scissors
          }
        case Win =>
          firstAction match {
            case Rock     => Paper
            case Paper    => Scissors
            case Scissors => Rock
          }
      }
    } yield (firstAction, secondAction)
  }

  protected[this] def stringToOutcome(name: String): Try[Outcome] = name match {
    case "X" => Success(Loose)
    case "Y" => Success(Draw)
    case "Z" => Success(Win)
    case _   => Failure(new IllegalArgumentException(s"Unsupported action name: $name"))
  }

}
