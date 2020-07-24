package utils
import scala.xml._
import java.io.File

/**
 * Checks identity of non-whitespace between two XML files
 * Application: does a tagger/tokenizer preserve this?
 */
object ComparePlainText {
  val context = 30
  def compare(in: String, out: String): Unit =
  {
    if (!(new File(out).exists()))
      return;
    val inPlain = XML.load(in).text
    val outPlain = XML.load(out).text
    val inNonWhite = inPlain.replaceAll("\\s+", "")
    val outNonWhite = outPlain.replaceAll("\\s+", "")
    if (inNonWhite != outNonWhite) {
      val lcp = EditDistance.longestCommonPrefix(inNonWhite, outNonWhite)
      val before = inNonWhite.substring(Math.max(0, lcp.length - context), lcp.length)
      val inAfter = inNonWhite.substring(lcp.length, Math.min(inNonWhite.length, lcp.length + context))
      val outAfter = outNonWhite.substring(lcp.length, Math.min(outNonWhite.length, lcp.length + context))

      println(
        s"""###Gedoe in $in!!
           |<$before>
           |        <$inAfter>
           |        <$outAfter>
           |""".stripMargin)
    } else {
      println(s"$in is OK!")
    }
  }

  def main(args: Array[String]): Unit = {
    ProcessFolder.processFolder(new File(args(0)), new File(args(1)), compare)
  }
}
