package js

import java.io.{FileReader, FileInputStream}

import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader

/**
 * Created by Kamil on 24.09.2015.
 */
object Js {

  def main(args: Array[String]): Unit = {
    val inputs = if(args.length >= 1) {
      args.toSeq
    } else Seq("test.js")
    for(input <- inputs) {
//      val lexer = new Lexer
//      val scan = new lexer.Scanner(new PagedSeqReader(PagedSeq.fromReader(new FileReader(input))))
//      Console.err.print(Stream.iterate(scan)(_.rest).takeWhile(!_.atEnd).map(_.first).mkString(","))
      NewParser.parseAll(NewParser.program, new FileReader(input)) match {
        case NewParser.Success(ast, _) =>
          //        System.err println s"Parsing successful!\n$ast"
          val transformed = ast map Transformer.transform
          //        System.err println s"transformed ast: \n$transformed"
          println(transformed map Formatter.write mkString ";")
        case x => System.err println s"Something went wrong: $x"
      }
    }
  }

}
