package clariah.standoffmarkup

import java.io.{File, FileInputStream, PrintWriter}
import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.zip.GZIPInputStream

import clariah.standoffmarkup.TEI2NAF.tei2naf

import scala.xml.{Elem, Node, Text, XML}

object TEI2NAF {

  private val splittingTags = Set("lb", "p", "div", "head", "fw")

  def toTextStructure(n: NodeWithOffsets): Node = {
    val nodeType = n.node.getClass.getName.replaceAll(".*\\.", "")
    n.node match {
      case t: Text =>
          <textnode offset={n.start.toString} length={n.length.toString}/>
      case e: Elem =>
        val down = n.children.map(toTextStructure)
        val attributes = n.node.attributes.map(a => <attribute name={a.prefixedKey} value={a.value.text}/>)
        <element id={n.id} offset={n.start.toString} length={n.length.toString} name={n.label}>
          {attributes}{down}
        </element>
      case _ =>
          <node type={nodeType} offset={n.start.toString} length={n.length.toString}/>
    }
  }

  def toTextUnits(n: NodeWithOffsets): scala.xml.NodeSeq = {
    val nodeType = n.node.getClass.getName.replaceAll(".*\\.","")
    if (n.node.isInstanceOf[Elem]) {
      val down = n.children.flatMap(toTextUnits)
      val attributes = n.node.attributes.map(a => <attribute name={a.prefixedKey} value={a.value.text}/>)
      <tunit id={n.id} offset={n.start.toString} length={n.length.toString} type={n.label}/> ++ down
    } else {
       Seq()
    }
  }

  def tei2naf(d0: Elem) = {
    val d = AddExtraIdsInTEI.completeIds(d0)
    val textNode = (d \\ "text").headOption
    textNode.map(n => {
      val n1 = StandoffMarkup.createStandoffMarkup(n,0)
      val txt = n1.text

      lazy val pcdata = txt
      lazy val cdata =  scala.xml.Unparsed("<![CDATA[" +  txt   + "]]>")
      val allTokens = StandoffTokenizing.tokenize(n1, x => splittingTags.contains(x.label))
        ._1.zipWithIndex.map({case (t,i) =>
          val id = s"wf.$i"
          <wf id={id} offset={t.start.toString} length={(t.end-t.start).toString}>{t.word}</wf>
      })

      val timestamp = LocalDateTime.now.format(DateTimeFormatter.ofPattern("YYYYMMdd_HHmmss"))
      val pid = (d \\ "idno").filter(x => (x \ "@type").text == "pid").headOption.map(_.text).getOrElse("__")
      val naf = <NAF version="v3.1a" xml:lang="nl">
      <nafHeader>
        <public publicId={pid + ".naf"} uri=""></public>
        <fileDesc
          title={(d \\ "title").text}
          filename={pid}/>
        <linguisticProcessors layer="rawText">
          <lp name="tei2nafsimple" version="0.0" timestamp={timestamp}/>
        </linguisticProcessors>
        <linguisticProcessors layer="teiTextUnits">
          <lp name="tei2nafsimple" version="0.0" timestamp={timestamp}/>
        </linguisticProcessors>
        <linguisticProcessors layer="basicTokenLayer">
          <lp name="tei2nafsimple" version="0.0" timestamp={timestamp}/>
        </linguisticProcessors>
      </nafHeader>
        <raw id="rawText">{pcdata}</raw>
        <text id="basicTokenLayer">{allTokens}</text>
        <tunits id="teiTextUnits">{toTextUnits(n1)}</tunits>
      </NAF>

      NAF(naf)
      // nee dit gaat niet goed als je hem niet eerst opslaat....
      //<textStructure type="TEI" namespace="http://www.tei-c.org/ns/1.0">{toTextStructure(n1)}</textStructure></NAF>
    })
  }

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(new File(args(0)), new File(args(1)), (in,out) => {
      val inDoc = XML.loadFile(in)
      tei2naf(inDoc).foreach(
        _.save(out)
      )
    })
  }
}

object MissivenToNAF {

  def getId(e: Node) = e.attributes.filter(_.key == "id").value.text

  def getTermsFromTEI(d: Elem): List[Term] = d.descendant.filter(x => x.label=="w" || x.label=="pc").map(
    { case e:Elem if e.label == "w" => Term(getId(e), e.text, (e \ "@lemma").text, (e \ "@type").text)
     case e:Elem if e.label == "pc" => Term(getId(e), e.text, "_", "PC")
    })

  def getTermsFromTEIWithSentenceIds(d: Elem): List[Term] = {
    val d1 = AddExtraIdsInTEI.completeIds(d)
    val sentences = d1 \\ "s"
    val terms = sentences.flatMap(s => {
      val sentenceId = Some(getId(s))
      s.descendant.filter(x => x.label == "w" || x.label == "pc").map(
        {
          case e: Elem if e.label == "w" => Term(getId(e), e.text, (e \ "@lemma").text, (e \ "@type").text, sentenceId)
          case e: Elem if e.label == "pc" => Term(getId(e), e.text, "_", "PC", sentenceId)
        })
    })
    terms.toList
  }

  def getTermsFromTEIFile(f: String): List[Term] = {
    val fIn = new File(f)
    val doc = if (fIn.exists()) XML.load(f) else {
      val zipped = s"$f.gz"
      val inputStream = new GZIPInputStream(new FileInputStream(zipped))
      XML.load(inputStream)
    }
    getTermsFromTEIWithSentenceIds(doc)
  }

  def findTaggedFile(in: String, tagged: String, f: String)  = {
    f.replaceAll(in,tagged)
  }

  def tei2NAFWithTermsFromTaggedFile(indir: String, outdir: String, tagged: String): Unit =
  {
    utils.ProcessFolder.processFolder(
      new File(indir),
      new File(outdir),
      (in, out) => {
        val inDoc = XML.loadFile(in)
        val naf0 = tei2naf(inDoc)
        naf0.foreach(n => {
          val taggedFile = findTaggedFile(indir, tagged, in)
          Console.err.println(taggedFile)
          val terms = getTermsFromTEIFile(taggedFile)
          val n1 = n.integrateTermLayer(terms.iterator, "termsFromTaggedTEI")
          n1.save(out)
        } // hierbij gaat de cdata verloren - repareer dat!
        )
      })
  }

  def main(args: Array[String]): Unit = {
    //TEI2NAF.main(Array(DataLocations.TEIFolder, DataLocations.tei2nafFolder))
    utils.ProcessFolder.processFolder(
      new File(DataLocations.TEIFolder),
      new File(DataLocations.tei2nafFolder),
      (in, out) => {
        val inDoc = XML.loadFile(in)
        val naf0 = tei2naf(inDoc)
        naf0.foreach(n => {
          val taggedFile = DataLocations.taggedTEIFolder + "/" + new File(in).getName
          val terms = getTermsFromTEIFile(taggedFile)
          val n1 = n.integrateTermLayer(terms.iterator, "termsFromTaggedTEI")
          n1.save(out)
        } // hierbij gaat de cdata verloren - repareer dat!
        )
      })
  }
}

object TEIToNafWithTagging {
  def main(args: Array[String]): Unit = {
    MissivenToNAF.tei2NAFWithTermsFromTaggedFile(args(0), args(1), args(2))
  }
}
