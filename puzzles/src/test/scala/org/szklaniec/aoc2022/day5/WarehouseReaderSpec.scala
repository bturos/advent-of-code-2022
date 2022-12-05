package org.szklaniec.aoc2022.day5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.szklaniec.aoc2022.PuzzleSolvingSupport

import scala.util.{Failure, Success}

class WarehouseReaderSpec extends AnyFlatSpec with Matchers with WarehouseReader {

  it should "load the warehouse layout properly" in {
    // given
    val filename = "crates-layout.txt"

    // when
    val warehouse = PuzzleSolvingSupport.executeLogicWithFile(filename, loadWarehouse)

    // then
    warehouse match {
      case Success(warehouse) =>
        println(warehouse)
        warehouse.stacks.size shouldBe 9
        warehouse.stacks(6) should contain only ('R', 'M', 'G', 'H', 'D')
      case Failure(exception) => fail("Unexpected error", exception)
    }
  }

}
