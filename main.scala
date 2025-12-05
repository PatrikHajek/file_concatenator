//> using scala 3.7.4
//> using toolkit default

import scala.util.Try
import scala.util.chaining._

case class File(path: os.Path, body: String)

// TODO: Option for prefix before the file name to easily make it a comment.
// TODO: Add flags.

@main
def main(path: String): Unit =
  val target = os.pwd / os.RelPath(path)
  read_files(target) match
    case Some(files) =>
      files.map(f => {
        val name = f.path.toString.replaceAll(os.pwd.toString + "/", "")
        s"""---------- $name ----------
        |${f.body}""".stripMargin.pipe(println)
      })
    case None => println("No content.. something went wrong")

def read_files(path: os.Path): Option[List[File]] =
  os.stat(path).fileType match
    case os.FileType.File | os.FileType.SymLink =>
      Try(
        File(path, body = os.read(path)).pipe(List(_))
      ).toOption
    case os.FileType.Dir =>
      Try(
        os.list(path).map(path => File(path, body = os.read(path))).toList
      ).toOption
    case os.FileType.Other => None
