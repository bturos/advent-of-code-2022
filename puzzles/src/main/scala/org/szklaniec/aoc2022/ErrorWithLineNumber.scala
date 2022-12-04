package org.szklaniec.aoc2022

case class ErrorWithLineNumber(originalError: Throwable, lineNumber: Int)
    extends RuntimeException(s"${originalError.getMessage}. Line: $lineNumber", originalError)
