package clariah.standoffmarkup

import scala.util.matching.Regex.Match
import scala.xml.{Elem, Node, Text}

/**
  * Dit tokenizeert niet echt, maar splitst alleen op whitespace en op tags waarop woorden
  * altijd gesplitst moeten worden
  */
object StandoffTokenizing {

  case class Token(word: String, start:Int, end: Int)  {}

  val wordPattern = "\\S+".r

  private def extendWithMatch(tokens: Seq[Token], lastTokenClosed: Boolean, m: Match, startOffset: Int, textLength: Int): (Seq[Token], Boolean) = {
    if (tokens.isEmpty || lastTokenClosed || m.start > 0)
    {
      val newT = new Token(m.toString, startOffset + m.start, startOffset +  m.end)
      (tokens :+ newT) -> (m.end < textLength)
    } else {
      val tLast = tokens.last
      val tNewLast = tLast.copy(word=tLast.word + m.toString, end=startOffset + m.end)
      (tokens.dropRight(1) :+ tNewLast) -> (m.end < textLength)
    }
  }

  private def extendWithNode(tokens: Seq[Token], lastTokenClosed: Boolean, n: NodeWithOffsets, isTokenBoundary: Node => Boolean = z => true): (Seq[Token], Boolean)  = {
    n.node match {
      case Text(t) =>
        val words = wordPattern.findAllMatchIn(t).toList
        if (words.isEmpty)
          (tokens, t.nonEmpty)
        else
          words.foldLeft(tokens -> lastTokenClosed){case ((t1, b),m) =>
            extendWithMatch(t1,b,m, n.start, t.length)}
      case e: Elem =>
        val b0 = lastTokenClosed || isTokenBoundary(e)
        val (foldedToks, closed) =  n.children.foldLeft(tokens -> b0){case ((t, b), n1) =>
          extendWithNode(t,b,n1,isTokenBoundary)}
        foldedToks -> (closed || isTokenBoundary(e))
    }
  }

  def tokenize(n: NodeWithOffsets, isTokenBoundary: Node => Boolean = z => true): (Seq[Token], Boolean) = {
    extendWithNode(Seq(), true, n, isTokenBoundary)
  }
}
