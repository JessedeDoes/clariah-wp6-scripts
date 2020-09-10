package clariah.standoffmarkup

import scala.xml._




object TestMissivenNAFConversion {

  def getId(n: Node):String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption.getOrElse("no_id_found")

  import DataLocations._

  val theIds = new java.io.File(TEIFolder6).listFiles().map(f => f.getName.replaceAll(".xml",""))


 def getParagraphText(p: Elem) = {
   val p1 = utils.PostProcessXML.updateElement(p, x => x.label=="xfw" || x.label == "xnote", x => <fw/>)
   p1.text
 }

  def testje(theId: String) = {
    val TEIFile = s"$TEIFolder6/$theId.xml"

    val nafFile = s"$nafFolder/$theId.naf"
    val nafFromTEI = s"$tei2nafFolder6/$theId.xml"

    Console.err.println(theId)
    try {
      val TEI = XML.loadFile(TEIFile)

      val naf = NAF(XML.loadFile(nafFile))
      val naftei = NAF(XML.loadFile(nafFromTEI))
      // val txtOld = scala.io.Source.fromFile(txtFile).getLines().map(_.replaceAll(",(\\S)",", $1")).map(_.trim).filter(_.nonEmpty).mkString("\n").trim
      val nafTxt = naf.rawText

      //val so = clariah.standoffmarkup.StandoffMarkup.createStandoffMarkup(TEI)

      val textNode = (TEI \\ "text").head

      val okNodes = Set("p", "head")


      val paragraphsTEI = textNode.descendant.filter(x => x.label == "p" || x.label == "head").map(p =>
        getId(p) -> p).toMap

      paragraphsTEI.foreach({ case (id, p) =>
        val pNAF = naf.getTextUnit(id)

        if (pNAF.isDefined) {

          val pnaftei: Option[TextUnit] = naftei.getTextUnit(id)
          val teiTxt = getParagraphText(p.asInstanceOf[Elem])
          val nafTxt = pNAF.get.content

          val teiNowhite = teiTxt.replaceAll("\\s+", "")
          val nafNoWhite = nafTxt.replaceAll("\\s+", "")


          val p1 = StandoffMarkup.createStandoffMarkup(p)

          //println(teiTokenized)
          val nafTokens = nafTxt.trim.split("\\s+").toList.take(20)
          val nafteiTokens = naftei.tokensIn(pnaftei.get).map(_.content).take(20)

          // println(nafteiTokens)
          val check = (teiNowhite == nafNoWhite && (nafteiTokens == nafTokens))
          if (!check) {
            val firstOff = (0 to nafteiTokens.size).find(i => nafTokens(i) != nafteiTokens(i))
            val info = s"${nafteiTokens(firstOff.get)} ${nafTokens(firstOff.get)}"
            println(s"""\n#### Mismatch for $id!!! [$info] ${teiTxt.length} ${nafTxt.length}
              TEI:$teiTxt
              NAF from TEI:${pnaftei.get.content} $pnaftei
              NAF:$nafTxt
              NAF from TEI tokens:$nafteiTokens
              NAF tokens:$nafTokens\n####""")
          }
        } else {
          Console.err.println(s"$id is missing in XMI!!!")
        }
      })


    } catch {
      case e:Exception => Console.err.println(s"No luck for $theId: $e")
    }
  }

  def main(args: Array[String]): Unit = {
    //println(txtFile)
    theIds.sorted.foreach(testje)
  }
}

