package clariah.wp6.Missiven

import PageStructure._

import utils.PostProcessXML

import scala.xml._


case class MissivenPageStructure(findZone: Elem => Box, page: Elem) {

  val peetjes = (page \\ "p").zipWithIndex
  val statjes =
    s"""
       |##################################################################################
       |P's: ${peetjes.size}
       |Divs: ${(page \\ "div").size}
       |Words: ${(page \\ "w").size}
     """.stripMargin


  def basicPageStructuring() = {

    //Console.err.println(statjes)

    val candidateHeads = peetjes.filter({ case (p, i) => p.text.trim.nonEmpty &&  p.text.trim.length > 5 && uppercaseity(p) >= 0.8 }).take(1).map(_._1)

    if (candidateHeads.nonEmpty) {
      //Console.err.println("\nHEADER\n" + candidateHeaders.head.text)
    }

    val candidateKopregels1 = (page \\ "div").take(2).filter(ab => {
      val b = findZone(ab.asInstanceOf[Elem])
      //Console.err.println(s"Div zone? : $b")
      b.uly < 300 && b.lry < 600
    })

    val candidateKopregels2 = (page \\ "p").filter(p => {
      if ((p \ "@facs").isEmpty)
      {
        // Console.err.println(s"No facs fo $p!!!")
        false
      } else {
        val b = findZone(p.asInstanceOf[Elem])
        //Console.err.println(s"p zone for  : $b $p")
        b.uly < 250 && b.lry < 600
      }
    }).take(2)

    val candidateKopregels = if (candidateKopregels1.nonEmpty) candidateKopregels1 else candidateKopregels2
    val bottomPagePageNumber = (page \\ "p").lastOption.filter(p => p.text.trim.matches("[0-9]+"))
    //candidateKopregels.foreach(kr => Console.err.println("Pagina FW: " + kr.text + " " + findZone(kr.asInstanceOf[Elem])))

    val possibleFolieringen = (page \\ "p").filter(p => p.text.trim.startsWith("Fol."))

    val p2 = PostProcessXML.updateElement3(page, x => x.label == "p" || x.label == "div",
      p => if (candidateKopregels.contains(p))
        p.copy(label = "fw",
           attributes = p.attributes
             .append(new UnprefixedAttribute("type", if (p.text.trim.size < 6) "page-number" else "head", Null))
             .append(new UnprefixedAttribute("place", "top", Null)))
      else p)


    val p3 = PostProcessXML.updateElement(p2, _.label == "p",
      p => if (candidateHeads.contains(p)) p.copy(label = "head") else p)

    val p4 =  PostProcessXML.updateElement(p3, _.label == "p",
      p => if (possibleFolieringen.contains(p)) <pb unit="folio" n={p.text.trim.replaceAll("^Fol(\\.*)\\s*","").trim}/> else p)

    val p5 = PostProcessXML.updateElement3(p4, _.label == "p", p => if (bottomPagePageNumber.contains(p)) <fw type="bottom-page-number">{p.child}</fw>else p)
    // extra rule: anything between preceding first head should be fw
    p5
  }
}
