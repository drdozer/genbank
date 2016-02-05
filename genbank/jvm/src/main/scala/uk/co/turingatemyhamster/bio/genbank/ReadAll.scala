package uk.co.turingatemyhamster.bio.genbank

import java.io._
import java.util.zip._
import scala.io

object ReadAll {
  def main(args: Array[String]): Unit = {
    for(arg <- args) {
      if(arg.endsWith(".gz")) {
        process(io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(arg))).mkString)
      } else {
        process(io.Source.fromFile(arg).mkString)
      }
    }
  }

  def process(allInput: String): Unit = {
    import fastparse.core.Parsed._

    GenbankParser.GenbankDatabaseFile.parse(allInput) match {
      case s : Success[(GenbankHeader, Seq[GenbankRecord])] => println(s)
      case f : Failure => println(f.extra.traced.trace)
    }
  }

}
