package utils

import java.io._

import javax.xml.transform._
import javax.xml.transform.stream.{StreamResult, StreamSource}


case class XSLT(xsl: String) {

  //val xslSource: Source = new StreamSource(myXsl)
  val xslSource: Source = new StreamSource(new FileReader(xsl))

  val transformer = buildTransformer(xslSource)

  def buildTransformer(xslSource: Source): Transformer = {
    val transformerFactory = TransformerFactory.newInstance()
    transformerFactory.newTransformer(xslSource)
  }

  def transform(inFile: String, outFile: String) =
  {
    val outputStream = new FileOutputStream(outFile)
    val xmlSource: Source = new StreamSource(new FileReader(inFile))
    val result: Result = new StreamResult(outputStream)
    transformer.transform(xmlSource, result)
    outputStream.close()
  }

  def transformString(in: String): String  = {
    val out = new StringWriter()
    val result: Result = new StreamResult(out)
    val xmlSource: Source = new StreamSource(new StringReader(in))
    transformer.transform(xmlSource, result)
    out.getBuffer.toString
  }
}

object XSLT
{
  def main(args: Array[String]): Unit = {
    val xsl = XSLT(args(0))
    val rename: String => String = if (args.size > 4) x => { val y = x.replaceAll(args(3), args(4));  y } else identity[String]

    //ProcessFolder.processFolder(new File(args(1)), new File(args(2)), (f1,f2) => xsl.transform(f1,f2), rename)
  }
}


