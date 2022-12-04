package org.szklaniec

import scala.util.{Failure, Try}

package object aoc2022 {

  implicit class ErrorWithLineNumberOps[T](tryInstance: Try[T]) {

    def recoverWithLineNumber(lineNumber: Int): Try[T] = {
      tryInstance.recoverWith {
        case errorWithLineNumber @ ErrorWithLineNumber(_, _) => Failure(errorWithLineNumber)
        case otherError                                      => Failure(ErrorWithLineNumber(otherError, lineNumber))
      }
    }

  }

}
