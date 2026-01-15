//> using scala 3.7.4
//> using toolkit default

import scala.util.Try
import scala.util.chaining._

case class File(path: os.Path, body: String)

// TODO: Option for prefix before the file name to easily make it a comment.
// TODO: Add flags.
// TODO: Glob ignore pattern. Maybe even a default ignore pattern for things like
// binary files and images.

@main
def main(paths: String*): Unit =
  paths
    .flatMap(path => read_files(os.pwd / os.RelPath(path)))
    .flatten
    .distinct
    .foreach(file => {
      val name = file.path.toString.replaceAll(os.pwd.toString + "/", "")
      s"""---------- $name ----------
        |${file.body}""".stripMargin.pipe(println)
    })

def read_files(path: os.Path): Option[List[File]] =
  Try {
    os.stat(path).fileType match
      case os.FileType.File | os.FileType.SymLink =>
        Try(
          File(path, body = os.read(path)).pipe(List(_))
        ).toOption
      case os.FileType.Dir =>
        Try(
          os.list(path).flatMap(read_files).flatten.toList
        ).toOption
      case os.FileType.Other => None
  }.toOption.flatten
