package clariah.wp6.Missiven

import java.io.File
import scala.xml._

object Settings {
  val XMLDirectory = "data/Missiven"
  val rootAtHome = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/"

  val inputDirectoryatHome = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/TestConversion/"
  val inputDirectoryatWork = "/mnt/Nederlab/Corpusdata/2-PreTEI/generalemissiven/"

  val outputDirectoryatHome = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/Simplified/"
  val outputDirectoryAtWork = "/mnt/Nederlab/Corpusdata/5-TEI/generalemissiven/"

  val inputs = List(inputDirectoryatHome, inputDirectoryatWork)
  val outputs = List(outputDirectoryatHome, outputDirectoryAtWork)

  val inputDirectory = inputs.filter(new File(_).exists()).head
  val outputDirectory = outputs.filter(new File(_).exists()).head


  println(s"$inputDirectory $outputDirectory")

  lazy val allFiles = new File(inputDirectory).listFiles().filter(_.getName.endsWith(".xml")).toStream

  val readCorrectedTocs = true
  val theOneToProcess  = 1
  val doAll = true

  val part6PlaceIndex = "INT_096a7727-adcb-31f5-a536-e92ef8601c69.xml"

  lazy val part6PlaceIndexDocument = XML.loadFile(outputDirectory + "split/6/" + part6PlaceIndex)

  val part6PersonIndex = "INT_d8df3e95-0b61-3b01-a8ea-7e2166ae8e01.xml"

  lazy val part6PersonIndexDocument = XML.loadFile(outputDirectory + "split/6/" + part6PersonIndex)

  val part6ShipIndex = "INT_0fc75eb1-93fd-3fba-93a8-2ab674fa6ac4.xml"

  lazy val part6ShipIndexDocument = XML.loadFile(outputDirectory + "split/6/" + part6ShipIndex)

  lazy val part6Files = new File(Settings.outputDirectory + "split/6/").listFiles()
  lazy val part6Documents = part6Files.toStream.map(XML.loadFile)
}
