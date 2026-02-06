#!/usr/bin/env -S scala-cli shebang
//> using scala 3.7.4
//> using toolkit default

import scala.util.Try
import scala.util.chaining._
import scala.util.Failure
import scala.util.Success

case class File(path: os.Path, body: String)

// TODO: Option for prefix before the file name to easily make it a comment.
// TODO: Add flags.
// TODO: Glob ignore pattern. Maybe even a default ignore pattern for things like
// binary files and images.

main(args.toSeq)

def main(paths: Seq[String]): Unit =
  if paths.isEmpty then
    System.err.println("Missing arguments: <path1>, <path2>, ... ")
    System.exit(1)

  paths
    .map(path => read_files(os.pwd / os.RelPath(path)))
    .flatMap(_ match
      case Failure(e)     => List(e)
      case Success(files) => files)
    .distinct
    .sortWith((a, b) => a.isInstanceOf[Throwable])
    .map(_ match
      case e: Throwable     => e.toString
      case File(path, body) => {
        val name = path.toString.replaceAll(os.pwd.toString + "/", "")
        s"""
        |---------- $name ----------
        |${body}""".stripMargin
      })
    .reduce(_ + "\n" + _)
    .pipe(println)

def read_files(path: os.Path): Try[List[File]] =
  Try {
    os.stat(path).fileType match
      case os.FileType.File | os.FileType.SymLink =>
        File(path, body = os.read(path).trim).pipe(List(_))
      case os.FileType.Dir =>
        os.list(path).flatMap(read_files(_).get).toList
      case os.FileType.Other => List()
  }
