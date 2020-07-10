package clariah.standoffmarkup
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

case class Term(id: String, wordform: String, lemma: String, pos: String)

case class TermWithOffsets(term: Term, offset: Int, length: Int, id: String="") {
  def nafToken = <wf id={term.id} offset={offset.toString} length={length.toString}>{term.wordform}</wf>
  def nafTerm =  <term id={id} lemma={term.lemma} pos={term.pos}><span><target id={term.id}/></span></term>
}

case class NAF(document: Elem) {
  lazy val rawText = (document \ "raw").text
  lazy val tokens = ((document \ "text").head \ "wf").map(NafToken(_,rawText)).toList
  lazy val textUnits = (document \\ "tunit").map(TextUnit(_,rawText))

  lazy val tUnitMap = textUnits.map(x => x.id ->x).toMap

  // println(tokens)
  def getTextUnit(id: String) = tUnitMap.get(id)

  def tokensIn(tu: TextUnit): immutable.Seq[NafToken] = tokens.filter(
    t => t.begin >= tu.begin && t.end <= tu.end
  )

  def integrateTermLayer(terms: Iterator[Term], id: String="externalTermLayer")  =  {
    def addTerm(s: Stream[TermWithOffsets], t: Term) = {
      if (s.isEmpty) Stream(TermWithOffsets(t,0, t.wordform.length))
      else {
        val b = s.head
        val next = NAF.locateString(rawText, t.wordform, b.offset + b.length)._1// rawText.indexOf(t.wordform, b.offset + b.length)
        //Console.err.println(s"$b --> $next")
        Stream.cons(TermWithOffsets(t, next, t.wordform.length), s)
      }
    }
    val termsWithOffsets = terms.toStream.foldLeft(Stream.empty[TermWithOffsets])(addTerm).zipWithIndex.map{case (t,i) => t.copy(id= "t" + i)}.reverse
    val textLayer =  <text id={id  + ".tokens"}>{termsWithOffsets.map(_.nafToken)}</text>
    val termLayer =  <terms id={id + ".terms"}>{termsWithOffsets.map(_.nafTerm)}</terms>
    this.copy(document=this.document.copy(child = this.document.child ++ Seq(textLayer, termLayer)))
  }

  def save(f: String) = {

  }
}

object NAF {
  def locateString(haystack: String, needle: String, from: Int) = { // dit is nog niet goed want werkt ECHT alleen maar als alle nonwhite klopt en needle nooit white bevat
    val base = (haystack.indexOf(needle(0), from), haystack.indexOf(needle(0), from)+1)
    def findNext(I: (Int,Int), c: Char) = {  val i1 = haystack.indexOf(c.toString,I._2); (I._1, i1+1) }
    val range = needle.tail.foldLeft(base)(findNext)
    range
  }

  def main(args: Array[String]): Unit = {
    val haystack = "ik we e t he t n i et meer"
    val (f,t)  = locateString(haystack, "ethet", 5)
    println("<" + haystack.substring(f,t) + ">")
  }

}
