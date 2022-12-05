package org.szklaniec.aoc2022.day5

private[day5] case class Warehouse(stacks: Map[Int, List[Crate]]) {

  override def toString: String = {
    stacks.toList
      .sortBy { case (stackIndex, _) =>
        stackIndex
      }
      .map { case (stackIndex, stack) =>
        s"$stackIndex: $stack"
      }
      .mkString("\n")
  }

}

object Warehouse {
  def empty: Warehouse = Warehouse(Map.empty)
}
