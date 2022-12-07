package org.szklaniec.aoc2022.day5

import com.typesafe.scalalogging.StrictLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.szklaniec.aoc2022.PuzzleSolvingSupport

import scala.util.{Failure, Success, Try}

class CraneOperationsExecutorSpec extends AnyFlatSpec with Matchers with WarehouseReader with CraneOperationsExecutor with StrictLogging {

  it should "load the warehouse layout properly" in {
    // given
    val layoutFilename = "crates-layout.txt"
    val craneOperationsFilename = "crane-operations.txt"

    // when
    val warehouses = for {
      initialWarehouse <- PuzzleSolvingSupport.executeLogicWithFile(layoutFilename, loadWarehouse)
      _ <- Try(logger.info(s">>> Initial warehouse $initialWarehouse"))
      updatedWarehouseWithCrane9000 <- PuzzleSolvingSupport.executeLogicWithFile(
        craneOperationsFilename,
        moveCratesAround(_, initialWarehouse, OneCrateAtTime)
      )
      updatedWarehouseWithCrane9001 <- PuzzleSolvingSupport.executeLogicWithFile(
        craneOperationsFilename,
        moveCratesAround(_, initialWarehouse, MultipleCratesAtTime)
      )
    } yield (initialWarehouse, updatedWarehouseWithCrane9000, updatedWarehouseWithCrane9001)

    // then
    warehouses match {
      case Success((initialWarehouse, updatedWarehouseWithCrane9000, updatedWarehouseWithCrane9001)) =>
        logger.info(s">>> Updated warehouse with crane 9000 $updatedWarehouseWithCrane9000")
        logger.info(s">>> Updated warehouse with crane 9001 $updatedWarehouseWithCrane9001")

        initialWarehouse.cratesCount shouldBe updatedWarehouseWithCrane9000.cratesCount
        initialWarehouse.cratesCount shouldBe updatedWarehouseWithCrane9001.cratesCount

        initialWarehouse.stacks.size shouldBe 9
        initialWarehouse.stacks(6) should contain only ('R', 'M', 'G', 'H', 'D')

        updatedWarehouseWithCrane9000.stacks.size shouldBe 9
        updatedWarehouseWithCrane9001.stacks.size shouldBe 9
        updatedWarehouseWithCrane9000.getTopCrates.mkString shouldBe "TDCHVHJTG"
        updatedWarehouseWithCrane9001.getTopCrates.mkString shouldBe "NGCMPJLHV"

      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

}
