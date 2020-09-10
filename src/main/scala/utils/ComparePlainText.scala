package utils
import scala.xml._
import java.io.File

/**
 * Checks identity of non-whitespace between two XML files
 * Application: does a tagger/tokenizer preserve this?
 */
object ComparePlainText {
  val context = 30


  def getTopTextNodes(e: Node):Seq[Node] =  
    if (e.label == "text") Seq(e) else e.child.flatMap(getTopTextNodes)

  def compareUntaggedTagged(in: String, xin: Elem, xout: Elem)
  {
     val inPlain = getTopTextNodes(xin).text // nee dit is niet goed.... neem buitenste tekst alleen
     val outPlain = xout.descendant.filter(d => d.label == "w" || d.label == "pc").text
     compareText(in, inPlain, outPlain, "untagged-tagged")
  }

  def compareText(in: String, inPlain: String, outPlain: String, typ: String)
  {
    val inNonWhite = inPlain.replaceAll("\u00a0", " ").replaceAll("\\s+", "")
    val outNonWhite = outPlain.replaceAll("\u00a0", " ").replaceAll("\\s+", "")
    if (inNonWhite != outNonWhite) {
      val lcp = EditDistance.longestCommonPrefix(inNonWhite, outNonWhite)
      val before = inNonWhite.substring(Math.max(0, lcp.length - context), lcp.length)
      val inAfter = inNonWhite.substring(lcp.length, Math.min(inNonWhite.length, lcp.length + context))
      val outAfter = outNonWhite.substring(lcp.length, Math.min(outNonWhite.length, lcp.length + context))

      println(
        s"""###Gedoe ($typ) in $in!!
           |<$before>
           |        <$inAfter>
           |        <$outAfter>
           |""".stripMargin)
    } else {
      println(s"$in is OK ($typ)!")
    }
  } 

  def compare(in: String, out: String): Unit =
  {
    if (!(new File(out).exists()))
      return;
    val xIn = XML.load(in)
    val xOut = XML.load(out)

    val inPlain = xIn.text
    val outPlain = xOut.text
    compareText(in,inPlain,outPlain,"all")
    compareUntaggedTagged(in,xIn,xOut)
  }

  def main(args: Array[String]): Unit = {
    ProcessFolder.processFolder(new File(args(0)), new File(args(1)), compare)
  }
}
