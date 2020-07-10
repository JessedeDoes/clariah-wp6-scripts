package clariah.standoffmarkup

import java.io.PrintWriter

import clariah.standoffmarkup.TEI2NAF.tei2naf

import scala.xml.XML

object NAFTestje {

  val mini = <TEI>
    <text>
      <w>Hallo</w> <w>meneer</w>
    </text>
  </TEI>

  val missivenTEIDir = "/data/CLARIAH/WP6/generalemissiven/6/"
  val exampleFile = missivenTEIDir + "INT_fa840fc6-9b64-3966-b53d-8a4c79bbec9b.xml" // "data/CRM/Metadata/0001.tei.xml"
  lazy val exampleTEI0 = XML.loadFile(exampleFile)


  private def testTokens(exampleTEI: _root_.scala.xml.Elem) = {
    val nafje = XML.loadFile("/tmp/nafje.xml")

    val naf_txt: String = (nafje \\ "raw").text

    val w_tei = (exampleTEI \\ "w").map(_.text)
    val pw = new PrintWriter("/tmp/txt.txt")
    pw.print(naf_txt)
    pw.close()

    val w_naf = (nafje \\ "element").filter(e => (e \ "@name").text == "w").map(w => {
      val offset = (w \ "@offset").text.toInt
      val length = (w \ "@length").text.toInt
      val id = (w \ "@id").text
      s"$id($offset,$length)=" + naf_txt.substring(offset, offset + length).trim
    })
    //w1.foreach(println)
    w_naf.take(50).zip(w_tei.take(50)).foreach(println)
  }

  def main(args: Array[String]): Unit = {
    val exampleTEI = if (args.size > 0) XML.loadFile(args(0)) else exampleTEI0

    val nafje0 = tei2naf(exampleTEI).get

    XML.save("/tmp/nafje.xml", nafje0, "UTF-8")

    testTokens(exampleTEI)
    //println(nafje)
  }
}
