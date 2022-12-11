package org.szklaniec.aoc2022.day7

import cats.syntax.all._
import com.typesafe.scalalogging.StrictLogging
import org.szklaniec.aoc2022.LineWithNumber
import org.szklaniec.aoc2022.day7.DirectoryTreeAnalyzer._

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

private[day7] trait DirectoryTreeAnalyzer {

  def calculateSize(lines: List[LineWithNumber], selectionStrategy: DirectorySelectionStrategy): Try[Int] = {
    for {
      outputs <- lines2Outputs(lines)
      pathTargets = converseCommands2Paths(outputs)
      calculatedSize <- selectionStrategy.findDirectorySizes(pathTargets)
    } yield calculatedSize
  }

  private[this] def lines2Outputs(lines: List[LineWithNumber]): Try[List[Output]] = {
    lines.traverse { case LineWithNumber(content, index) =>
      (content match {
        case LsCommandExpression() => Success(ListDirectoryCommand)
        case CdCommandExpression(path) =>
          path match {
            case "/"           => Success(GoToRootCommand)
            case ".."          => Success(GoUpCommand)
            case directoryName => Success(GoDownCommand(directoryName))
          }
        case ListItemDirectoryExpression(directoryName) => Success(DirectoryListItem(directoryName))
        case ListItemFileExpression(fileSize, fileName) => Try(FileListItem(fileName, fileSize.toInt))
        case lineContent                                => Failure(new IllegalArgumentException(s"Unsupported line format: $lineContent"))
      }).recoverWithLineNumber(index)
    }
  }

  private[this] def converseCommands2Paths(outputs: List[Output]): Map[Path, PathTarget] = {
    val (knownDirectoriesMap, knownFilesMap, _) =
      outputs
        .foldLeft((Map.empty[Path, DirectoryTarget], Map.empty[Path, FileTarget], Path.root)) {
          case ((knownDirectoriesMap, knownFilesMap, currentPath), output) =>
            output match {
              case GoToRootCommand =>
                (knownDirectoriesMap, knownFilesMap, Path.root)
              case GoDownCommand(directoryName) =>
                val newPath = currentPath.moveDown(directoryName)
                val directory = DirectoryTarget(directoryName, newPath, List.empty)
                (knownDirectoriesMap + (newPath -> directory), knownFilesMap, newPath)
              case GoUpCommand =>
                val newPath = currentPath.moveUp
                val directory = knownDirectoriesMap.getOrElse(newPath, DirectoryTarget(newPath.pathElements.head, newPath, List.empty))
                (knownDirectoriesMap + (newPath -> directory), knownFilesMap, newPath)
              case ListDirectoryCommand =>
                (knownDirectoriesMap, knownFilesMap, currentPath)
              case FileListItem(fileName, fileSize) =>
                val directory =
                  knownDirectoriesMap.getOrElse(currentPath, DirectoryTarget(currentPath.pathElements.head, currentPath, List.empty))
                val filePath = currentPath.moveDown(fileName)
                val file = FileTarget(fileName, fileSize)
                val directoryWithFile = directory.copy(items = filePath :: directory.items)
                (knownDirectoriesMap + (currentPath -> directoryWithFile), knownFilesMap + (filePath -> file), currentPath)
              case DirectoryListItem(directoryName) =>
                val subDirectoryPath = currentPath.moveDown(directoryName)
                val subDirectory =
                  knownDirectoriesMap.getOrElse(subDirectoryPath, DirectoryTarget(directoryName, subDirectoryPath, List.empty))
                val directory =
                  knownDirectoriesMap.getOrElse(currentPath, DirectoryTarget(currentPath.pathElements.head, currentPath, List.empty))
                val currentDirectoryWithSubDirectory = directory.copy(items = subDirectoryPath :: directory.items)
                (
                  knownDirectoriesMap + (currentPath -> currentDirectoryWithSubDirectory) + (subDirectoryPath -> subDirectory),
                  knownFilesMap,
                  currentPath
                )
            }
        }

    knownDirectoriesMap ++ knownFilesMap
  }

}

private[day7] object DirectoryTreeAnalyzer {

  val CdCommandExpression: Regex = """\$\scd\s(\S+)""".r
  val LsCommandExpression: Regex = """\$\sls""".r
  val ListItemDirectoryExpression: Regex = """dir\s([\w\d.]+)""".r
  val ListItemFileExpression: Regex = """(\d+)\s([\w\d.]+)""".r

  sealed trait Output

  sealed trait Command extends Output
  case object ListDirectoryCommand extends Command
  case object GoToRootCommand extends Command
  case class GoDownCommand(directoryName: String) extends Command
  case object GoUpCommand extends Command

  sealed trait ListItem extends Output
  case class DirectoryListItem(directoryName: String) extends ListItem
  case class FileListItem(fileName: String, fileSize: Int) extends ListItem

  case class Path(pathElements: List[String]) {
    def moveUp: Path = Path(pathElements.drop(1))
    def moveDown(directoryName: String): Path = Path(directoryName :: pathElements)
  }
  object Path {
    val root: Path = Path("/".pure[List])
  }

  sealed trait PathTarget
  case class DirectoryTarget(name: String, path: Path, items: List[Path]) extends PathTarget
  object DirectoryTarget {
    val root: DirectoryTarget = DirectoryTarget("/", Path.root, List.empty)
  }
  case class FileTarget(name: String, fileSize: Int) extends PathTarget {
    override def toString: String = s"file: [$name] [$fileSize]"
  }

  case class PathWithSize(
      path: Path,
      size: Int
  )

}

private[day7] sealed trait DirectorySelectionStrategy {

  def findDirectorySizes(paths: Map[Path, PathTarget]): Try[Int]

  protected[this] def calculateSizesForAllDirectories(paths: Map[Path, PathTarget]): Try[Map[Path, Int]] = {
    paths.values
      .collect { case pathTarget: DirectoryTarget => pathTarget }
      .toList
      .traverse(directory => evaluateSize(directory.path, paths).map(directory.path -> _))
      .map(_.toMap)
  }

  protected[this] def evaluateSize(path: Path, paths: Map[Path, PathTarget]): Try[Int] = {
    paths.get(path) match {
      case Some(FileTarget(_, size))          => Success(size)
      case Some(DirectoryTarget(_, _, items)) => items.traverse(subPath => evaluateSize(subPath, paths)).map(_.sum)
      case None                               => Failure(new IllegalArgumentException(s"Could not find path target for path [$path]"))
    }
  }
}

case object AtMost10k extends DirectorySelectionStrategy {

  val MaxAllowedSize: Int = 100000

  override def findDirectorySizes(paths: Map[Path, PathTarget]): Try[Int] = {
    calculateSizesForAllDirectories(paths)
      .map(_.filter(_._2 <= MaxAllowedSize))
      .map(_.values.sum)
  }

}

case object SmallestAndBiggerThan30MB extends DirectorySelectionStrategy with StrictLogging {

  val TotalSpace: Int = 70000000
  val RequiredFreeSpace: Int = 30000000

  override def findDirectorySizes(paths: Map[Path, PathTarget]): Try[Int] = {
    for {
      directorySizes <- calculateSizesForAllDirectories(paths)
      totalUsedSpace <- directorySizes.get(Path.root) match {
        case Some(size) => Success(size)
        case None       => Failure(new IllegalStateException("Could not find root path!"))
      }
    } yield {
      logger.info(s"Total used space is $totalUsedSpace")

      val currentFreeSpace = TotalSpace - totalUsedSpace
      logger.info(s"Current free space: $currentFreeSpace")

      val requiredFreeSpace = RequiredFreeSpace - currentFreeSpace
      logger.info(s"Required free space is $requiredFreeSpace")
      val directorySizesBiggerThanRequiredFreeSpace = directorySizes.values.filter(_ >= requiredFreeSpace)

      directorySizesBiggerThanRequiredFreeSpace.toList.min
    }
  }

}
