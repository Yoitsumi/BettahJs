package js

import java.io.Writer

/**
 * Created by Kamil on 2015-05-01.
 */
class PrettyPrinter(val out: Writer) {

  private var indentation = 0

  def indented(body: => Unit): Unit = {
    indentation += 1
    body
    indentation -= 1
  }

  def writeln(str: String) = {
    write(str + "\n")
  }

  def write(str: String) = {
    out.write("\t" * indentation)
    out.write(str)
  }

  def writeInline(str: String) = {
    out.write(str)
  }

  def flush: Unit = out.flush()

}
