package org.szklaniec.aoc2022

package object day3 {

  private[day3] def getPriorityForItem(item: Char): Int = if (item.isLower) item - 'a' + 1 else item - 'A' + 27

}
