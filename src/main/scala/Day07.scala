import scala.collection.immutable.HashMap

object Day07 extends IDay {
  type FileSysItem = Either[String, (String, Int)]
  type FileSys = (String, HashMap[String, List[FileSysItem]])

  abstract class Command
  case class ChangeDir(target: String) extends Command
  case class ListDir() extends Command
  case class CreateFile(fileSize: Int, name: String) extends Command
  case class CreateDir(name: String) extends Command

  override def execute(input: String): (Int, Int) = {
    val commands = Helper.readLines(input, readCommand)
    val fileSys = buildFileSystem(commands)
    (part1(fileSys), part2(fileSys))
  }

  def readCommand(line: String): Command = {
    val cdPattern = "\\$ cd ([.\\w/]+)".r
    val lsPattern = "\\$ ls".r
    val createFilePattern = "(\\d+) ([.\\w]+)".r
    val dirPattern = "dir (\\w+)".r

    line match {
      case cdPattern(dir: String) => ChangeDir(dir)
      case lsPattern() => ListDir();
      case createFilePattern(fileSize: String, name: String) => CreateFile(fileSize.toInt, name)
      case dirPattern(name: String) => CreateDir(name)
    }
  }

  def buildFileSystem(commands: Iterable[Command]): FileSys = {
    var fileSys = ("/", new HashMap[String, List[FileSysItem]]() + ("/" -> List()))
    commands.foreach((c: Command) => fileSys = activateCommand(fileSys)(c))
    fileSys
  }

  def activateCommand(state: FileSys)(command: Command): FileSys = {
    command match {
      case ChangeDir("/") => ("/", state._2)
      case ChangeDir("..") => (directoryMoveBack(state._1), state._2)
      case ChangeDir(nextDir: String) => (state._1 + nextDir + "/", state._2)
      case ListDir() => state
      case CreateFile(fileSize: Int, name: String) =>
        val dirContents: List[FileSysItem] = Right((state._1 + name, fileSize)) :: state._2(state._1)
        (state._1, state._2 + (state._1 -> dirContents))
      case CreateDir(name) =>
        val dirContents: List[FileSysItem] = Left(name) :: state._2(state._1)
        (state._1, (state._2 + (state._1 + name + "/" -> List())) + (state._1 -> dirContents))
      case _ => sys.error("unknown command " + command)
    }
  }

  def directoryMoveBack(currDir: String): String = {
    currDir.reverse.drop(1).dropWhile(_ != '/').reverse
  }

  def listDirRec(dir: String, state: FileSys): List[(String, Int)] = {
    state._2(dir).flatMap {
      case Left(dirName: String) => listDirRec(dir + dirName + "/", state)
      case Right(file: (String, Int)) => List(file)
    }
  }

  def directorySizes(state: FileSys): Iterable[Int] = {
    state._2.map(c => listDirRec(c._1, state).map(_._2).sum)
  }

  def part1(fileSys: FileSys): Int = {
    directorySizes(fileSys).filter(_ <= 100000).sum
  }

  def part2(fileSys: FileSys): Int = {
    val directorySizeList = directorySizes(fileSys)
    val requiredDeletion = directorySizeList.max - 40000000
    directorySizeList.filter(_ >= requiredDeletion).min
  }
}
