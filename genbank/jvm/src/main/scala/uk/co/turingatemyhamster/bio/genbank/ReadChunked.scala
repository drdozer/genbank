package uk.co.turingatemyhamster.bio.genbank

import java.io._
import java.nio.charset.CodingErrorAction
import java.nio.file.FileSystems
import java.util.zip._

import scala.io
import scala.io.Codec

object ReadChunked {
  def main(args: Array[String]): Unit = {
    val fs = FileSystems.getDefault()

    for(arg <- args.par) {
      println(s"Processing $arg")

      if(arg.endsWith(".gz")) {
        process(arg, io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(arg)))(
          Codec.UTF8.onMalformedInput(CodingErrorAction.IGNORE))) // some genbank files are broken e.g. gbbct205.seq.gz
      } else {
        process(arg, io.Source.fromFile(arg)(
          Codec.UTF8.onMalformedInput(CodingErrorAction.IGNORE))) // some genbank files are broken e.g. gbbct205.seq.gz
      }
      println(s"Done $arg")
    }
  }

  def process(fileName: String, src: io.Source): Unit = {
    import fastparse.all._
    import fastparse.core.Parsed._
    import GenbankParser._

    val lines = src.getLines

    def nextChunk = {
      val record = new StringBuilder
      
      if(lines.hasNext) {
        var n = ""
        do {
          n = lines.next()
          record ++= n
          record ++= "\n"
        } while(lines.hasNext && n != "//")
      }

      record.toString
    }

    val firstP = GenbankHeader ~ GenbankRecord ~ End
    val rest = GenbankRecord ~ End

    {
      val c = nextChunk

      try {
        firstP.parse(c) match {
          case f : Failure =>
            println(s"Parse failed for $fileName\n${f.extra.traced.trace}\n${c.take(200)}")
          case _ =>
        }
      } catch {
        case t: Throwable =>
          println(s"Parse failed for $fileName\n${c.take(200)}")
          t.printStackTrace(System.out)
      }
    }

    while(lines.hasNext) {
      val c = nextChunk
      try {
        rest.parse(c) match {
          case f : Failure =>
            println(s"Parse failed for $fileName\n${f.extra.traced.trace}\n${c.take(200)}")
          case _ =>
        }
      } catch {
        case t : Throwable =>
          println(s"Parse failed for $fileName\n${c.take(200)}")
          t.printStackTrace(System.out)
      }
    }
  }

}
