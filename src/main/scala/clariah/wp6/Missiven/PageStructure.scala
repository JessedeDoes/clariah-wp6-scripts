package clariah.wp6.Missiven

import utils.PostProcessXML
import utils.PostProcessXML.{updateElement, updateElement2}

import scala.xml._

case class Box(uly: Int, ulx: Int, lry: Int, lrx: Int)


object PageStructure {
  def zoneX(p: Elem, b: Node) = {
    val facs = (b \ "@facs").text.replaceAll("#","")
    //println(facs)
    val zone = p.descendant.filter(z => z.label=="zone" && getId(z).get == facs).head
    def ai(s: String): Int = (zone \ s).text.toInt
    Box(ai("@uly"), ai("@ulx"), ai("@lry"), ai("@lrx"))
  }

  def zone(page: Elem)(b: Node) = {
    if (b.label == "p" && (b \ "@facs").isEmpty)
      zoneX(page, (b \\ "lb").head)
    else
      zoneX(page, b)
  }

  def getId(n: Node): Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  def italicity(e: Elem) = {
    val c1 = ((e \\ "hi").filter(x => (x \ "@rend").text.contains("italic")) \\ "w").size
    val c2 = ((e \\ "hi").filter(x => !(x \ "@rend").text.contains("italic")) \\ "w").size
    if (c1 + c2 == 0) 0.0 else c1 / (c1 + c2).asInstanceOf[Double]
  }

  def uppercaseity(e: Node) = {
    val t = e.text
    val u = t.count(_.isUpper)
    val l = t.count(_.isLower)
    if (u + l == 0) 0.0 else (u / (u+l).asInstanceOf[Double])
  }

  def parseRend(r: String): Map[String, String] = {
    //Console.err.println(r)
    r.split("\\s*;\\s*").filter(x => x.trim.nonEmpty && x.contains(":")).map(
      pv => {
        val a = pv.split("\\s*:\\s*")
        a(0) -> a(1)
      }
    ).toMap
  }

  def css(m: Map[String,String]) = m.map({case (k,v) => s"$k: $v"}).mkString("; ")

  def handleLineBreaks(p: Elem) = {
    val content = p.toString
    val c1 = content.replaceAll("([^<> \\s]+)[¬-]<lb/>([^<> \\s]+)","$1$2<lb/>").replaceAll("([^<> \\s]+)<lb/>([^<> \\s]+)","$1<lb/> $2")
    //println(c1)
    XML.loadString(c1)
  }

  def simplifyOneElement(p: Elem): NodeSeq = {
    val r = p.label match {
      case "w" => Text(p.text + (if ((p \ "@type").text == "last") "" else ""))
      case "editionStmt" => Seq()
      case "sourceDoc" => Seq()
      case "pb" => if ((p \ "@unit").text == "external") p else Seq()
      case "fw" => p.copy(child = Text(p.text.trim))
      case _ =>
        val ital = if (p.label == "p") PageStructure.italicity(p) else 0

        val c = p.child.flatMap({
          case e: Elem => simplifyOneElement(e)
          case x => x
        } )

        val convertToNote = ital > 0.7 // deze moet eigenlijk niet hier want is nogal missiven-specifiek
        val label = if (convertToNote) "note" else p.label
        val newAtts = if (convertToNote) p.attributes
          .append(new UnprefixedAttribute("place", "inline", Null))
          .append(new UnprefixedAttribute("type", "editorial-summary", Null))
          .append(new UnprefixedAttribute("resp", "editor", Null)) else p.attributes

        if (p.label == "div") (if ((p \\ "w").nonEmpty) c else Seq())
        else p.copy(child=c, label=label, attributes = newAtts.filter(x => p.label == "pb" || x.key != "facs"))
    }
    r
  }

  def simplifyRendition(e: Elem) = {
    val rends = (e \ "hi").map(_ \ "@rend").map(_.text).groupBy(x => x).mapValues(_.size)
    val mfr = rends.toList.sortBy(_._2).last._1

    //System.err.println(s"Renditions: ${rends.size} : $rends")
    if (true)
    {
      val m1 = PageStructure.parseRend(mfr).filter(_._1 != "font-family")
      val m2 =  PageStructure.parseRend((e \ "@rend").text).filter(_._1 != "font-family")
      val newRend =  PageStructure.css(m1 ++ m2)

      val newChild = e.child.flatMap(c => c match {
        case h:Elem if h.label == "hi" && ((h \ "@rend").text == mfr)=> h.child
        case _ => Seq(c)}
      )
      val newAtts = e.attributes.filter(_.key != "rend")
        .append(new UnprefixedAttribute("rend", newRend, Null))
      e.copy(child = newChild, attributes = newAtts)
    } else e
  }

  def findLastWordIn(p: Elem) = {
    if  ((p \\ "w").nonEmpty) {
      val lastWord = (p \\ "w").last
      updateElement(p, _.label=="w", w => (if (w==lastWord) w.copy(attributes = w.attributes.append(new UnprefixedAttribute("type", "last", Null))) else w))
    } else p
  }

  def simplifyPreTEI(d: Elem):Elem = {
    // whitespace handling

    val d0 = updateElement(d, _.label == "p", findLastWordIn)
    val d1 = updateElement2(d0, e=> true, simplifyOneElement).head.asInstanceOf[Elem]
    val d2 = updateElement(d1, e => (e \ "hi").nonEmpty, simplifyRendition)
    val d3 = PostProcessXML.updateElement(d2, x => x.label == "p" || x.label == "note" || x.label == "head", PageStructure.handleLineBreaks)

    // dit is niet zo handig zo, maar laat toch maar even lopen


      d3
  }

  def main(args: Array[String]): Unit = {
    val testP = <p>Hallo<x/>me¬<lb/>neer ik<x/>weet<lb/>het wel</p>
    println(handleLineBreaks(testP))
  }
}
