package clariah.standoffmarkup
import utils.PostProcessXML

import scala.collection.immutable
import scala.xml._

trait Annotation {
  val begin : Int
  val length : Int
  val end : Int = begin + length
  val typ: String
  val value: Any
  val content: String
}

case class NafToken(node: Node, text: String) extends Annotation
{
  val begin = (node \ "@offset").text.toInt
  val length = (node \ "@length").text.toInt
  override val end =  begin + length
  val id = (node \ "@id").text
  val typ = "token"
  val content = text.substring(begin,end)
  override val value = node.text
  override def toString: String = s"$typ $begin:$end $id ($content)"
}


case class TextUnit(node: Node, text: String) extends Annotation
{
  val begin = (node \ "@offset").text.toInt
  val length = (node \ "@length").text.toInt
  override val end =  begin + length
  val id = (node \ "@id").text
  val typ = node.label
  val content = text.substring(begin,end)
  override val value = node.label
  override def toString: String = s"$typ $begin:$end $id {$content}"
}

case class Term(id: String, wordform: String, lemma: String, pos: String, sentenceId: Option[String] = None)

case class TermWithOffsets(term: Term, offset: Int, length: Int, naf: NAF, id: String="") {
  val s = term.sentenceId.map(Text(_))
  def nafToken = <wf id={term.id} offset={offset.toString} sent={s} length={length.toString}>{term.wordform}</wf>
  def nafTerm =  <term id={id} lemma={term.lemma} pos={term.pos}><span><target id={term.id}/></span></term>

  def wordformInNaf = naf.rawText.substring(offset, offset+length)
  override def toString = s"$offset:$length => N:<$wordformInNaf>, T:<${term.wordform}>"
}

case class NAF(document: Elem) {
  lazy val id = (document \ "nafHeader" \ "public" \ "@publicId").text

  lazy val rawText = (document \ "raw").text
  lazy val rawTextNoWhite = rawText.replaceAll("\\s+","")
  lazy val tokens = ((document \ "text").head \ "wf").map(NafToken(_,rawText)).toList
  lazy val textUnits = (document \\ "tunit").map(TextUnit(_,rawText))

  lazy val tUnitMap = textUnits.map(x => x.id ->x).toMap

  // println(tokens)
  def getTextUnit(id: String) = tUnitMap.get(id)

  def tokensIn(tu: TextUnit): immutable.Seq[NafToken] = tokens.filter(
    t => t.begin >= tu.begin && t.end <= tu.end
  )

  /*
      Er gaat iets mis in de BaBtagger bij losstaande punten - die lijken te verdwijnen
  */

  def termsCoverText(t: Stream[TermWithOffsets]): Boolean =
  {
    val nonMatchingTerms = t.filter(!termOffsetsMatch(_))
    if (nonMatchingTerms.nonEmpty)
      Console.err.println(nonMatchingTerms.toList)
    val txt = t.map(_.term.wordform).mkString("").replaceAll("\\s+","").replaceAll("\\.","")
    val ok = txt == rawTextNoWhite.replaceAll("\\.","")
    if (!ok)
      {
        val pref = utils.EditDistance.longestCommonPrefix(txt,rawTextNoWhite)
        val l = pref.length

        val d1 = txt.substring(l, Math.min(txt.length, l+10))
        val d2 = rawTextNoWhite.substring(l, Math.min(txt.length, l+10))
        Console.err.println(s"No good ${txt.length} ${rawTextNoWhite.length} <$d1> <$d2>")
      }
    ok
  }

  def termOffsetsMatch(t: TermWithOffsets) =
    rawText.substring(t.offset, t.offset + t.length).replaceAll("\\s+","")== t.term.wordform

  def integrateTermLayer(terms: Iterator[Term], id: String="externalTermLayer")  =  {

    def addTerm(s: Stream[TermWithOffsets], t: Term) = {
      val startFrom = if (s.isEmpty) 0 else {
        val b = s.head
        b.offset + b.length
      }
      val (offset,end) = NAF.locateString(rawText, t.wordform, startFrom)
      Stream.cons(TermWithOffsets(t, offset, end - offset, this), s)
    }

    val termsWithOffsets = terms.toStream
      .foldLeft(Stream.empty[TermWithOffsets])(addTerm)
      .zipWithIndex
      .map{case (t,i) => t.copy(id= "t" + i)}
      .reverse
    // TODO check validity
    if (!termsCoverText(termsWithOffsets)) {
      Console.err.println(s"Bah! external terms have different plaintext for ${this.id}")
    } else {
      Console.err.println(s"Ok for ${this.id}")
    }
    val textLayer =  <text id={id  + ".tokens"}>{termsWithOffsets.map(_.nafToken)}</text>
    val termLayer =  <terms id={id + ".terms"}>{termsWithOffsets.map(_.nafTerm)}</terms>
    this.copy(document=this.document.copy(child =
      this.document.child.filter(_.label != "text") ++ Seq(textLayer, termLayer)))
  }

  def save(f: String) = {
    lazy val txt = rawText
    lazy val cdata =  scala.xml.Unparsed("<![CDATA[" + txt   + "]]>")
    val d1 = PostProcessXML.updateElement(document, _.label == "raw", e => e.copy(child=cdata))
    XML.save(f, d1, "UTF-8")
  }
}

object NAF {
  def locateString(haystack: String, needle: String, from: Int) = { // dit is nog niet goed want werkt ECHT alleen maar als alle nonwhite klopt en needle nooit white bevat
    val base = (haystack.indexOf(needle(0), from), haystack.indexOf(needle(0), from)+1)
    def findNext(I: (Int,Int), c: Char) = {  val i1 = haystack.indexOf(c.toString,I._2); (I._1, i1+1) }
    val range = needle.tail.foldLeft(base)(findNext)
    if (false && needle.contains("laastgem.")) {
      Console.err.println(s"$needle $from Found: $range CHECK: <${haystack.substring(range._1, range._2)}>")
    }
    range
  }

  def main(args: Array[String]): Unit = {
    val haystack = "laastgem ."
    val (f,t)  = locateString(haystack, "laastgem.", 0)
    println("<" + haystack.substring(f,t) + ">")
  }
}
