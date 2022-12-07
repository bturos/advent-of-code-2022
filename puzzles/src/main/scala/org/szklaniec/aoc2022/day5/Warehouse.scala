package org.szklaniec.aoc2022.day5

private[day5] case class Warehouse(stacks: Map[Int, List[Crate]]) {

  override def toString: String = {
    stacks.toList
      .sortBy { case (index, _) => index }
      .map { case (index, stack) =>
        s"\nStack $index: $stack"
      }
      .mkString
  }

  def cratesCount: Int = stacks.values.flatten.size

  def getTopCrates: List[Crate] = stacks.toList
    .sortBy { case (index, _) =>
      index
    }
    .map { case (_, stack) =>
      stack.head
    }

}

object Warehouse {
  def empty: Warehouse = Warehouse(Map.empty)
}
