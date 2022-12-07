package org.szklaniec.aoc2022

package object day5 {
  type Crate = Char

  object Crate {
    val empty: Crate = Crate(' ')

    def apply(c: Char): Crate = c
  }
}
