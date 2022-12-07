package org.szklaniec.aoc2022.day5

import cats.syntax.all._
import org.szklaniec.aoc2022.LineWithNumber

import scala.util.{Failure, Success, Try}

private[day5] trait CraneOperationsExecutor {

  def moveCratesAround(
      lines: List[LineWithNumber],
      initialWarehouse: Warehouse,
      crateMovementStrategy: CrateMovementStrategy
  ): Try[Warehouse] = {
    for {
      craneOperations <- lines.traverse(convertLineToOperations)
      updatedWarehouse <- executeCraneOperations(craneOperations, initialWarehouse, crateMovementStrategy)
    } yield updatedWarehouse
  }

  private[this] def convertLineToOperations(lineWithNumber: LineWithNumber): Try[CraneOperation] = {
    Try {
      val operationDescriptors = lineWithNumber.content.split(" ")
      CraneOperation(operationDescriptors(1).toInt, operationDescriptors(3).toInt - 1, operationDescriptors(5).toInt - 1)
    }.recoverWithLineNumber(lineWithNumber.number)
  }

  private[this] def executeCraneOperations(
      craneOperations: List[CraneOperation],
      warehouse: Warehouse,
      crateMovementStrategy: CrateMovementStrategy
  ): Try[Warehouse] =
    craneOperations
      .foldLeft(Try(warehouse.stacks)) { case (stacks, operation) =>
        for {
          existingStacks <- stacks
          fromStack <- getStack(existingStacks, operation.fromStackIndex)
          toStack <- getStack(existingStacks, operation.toStackIndex)

          movedCrates = fromStack.take(operation.numberOfCrates)
          updatedFromStack = fromStack.drop(operation.numberOfCrates)
          updatedToStack = crateMovementStrategy.moveCrates(movedCrates) ++ toStack

          updatedStacks = existingStacks ++ Map(operation.fromStackIndex -> updatedFromStack, operation.toStackIndex -> updatedToStack)
        } yield updatedStacks
      }
      .map(Warehouse(_))

  private[this] def getStack(stacks: Map[Int, List[Crate]], stackIndex: Int): Try[List[Crate]] = {
    stacks.get(stackIndex) match {
      case Some(stack) => Success(stack)
      case None        => Failure(new IllegalArgumentException(s"Could not find index $stackIndex!"))
    }
  }

}

private case class CraneOperation(numberOfCrates: Int, fromStackIndex: Int, toStackIndex: Int)

private[day5] sealed trait CrateMovementStrategy {
  def moveCrates(movedCrates: List[Crate]): List[Crate]
}

private[day5] case object OneCrateAtTime extends CrateMovementStrategy {
  override def moveCrates(movedCrates: List[Crate]): List[Crate] = {
    movedCrates.reverse
  }
}
private[day5] case object MultipleCratesAtTime extends CrateMovementStrategy {
  override def moveCrates(movedCrates: List[Crate]): List[Crate] = {
    movedCrates
  }
}
