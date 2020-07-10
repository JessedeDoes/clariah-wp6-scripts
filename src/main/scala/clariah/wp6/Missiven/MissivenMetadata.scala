package clariah.wp6.Missiven

import java.io.{File, PrintWriter}

import Settings.XMLDirectory
import utils.PostProcessXML.updateElement

import scala.collection.immutable
import scala.xml.{Elem, Node, NodeSeq, XML}
import scala.util.matching.Regex
import scala.util.{Success, Try}

object MissivenMetadata {

  def createGrouping[T](l: Seq[T], starts: T => Boolean, ends: T => Boolean): Seq[(Seq[T], Boolean)] = {

      def plak(a: Seq[(Seq[T], Boolean)], b: T) = {

        if ((a.isEmpty)) Seq((Seq(b), starts(b))) else
        {
          if (starts(b) || ends(b)) a ++ Seq((Seq(b), starts(b)))
          else a.dropRight(1) ++ Seq( (a.last._1 ++ Seq(b), a.last._2))
        }
      }
     l.foldLeft(Seq[(Seq[T], Boolean)]())(plak)
  }

  def pushOptionInside[T](o: Option[(T,Int)]):(Option[T], Int) =
    o.map(x => (Some(x._1).asInstanceOf[Option[T]],x._2)).getOrElse( (None, 0) )

  def groupWithFirst[T](l: Seq[T], f: T => Boolean): Seq[Seq[T]] =
  {
    val numberedChild:List[(T, Int)] = l.toList.zipWithIndex
    def lastBefore(i:Int):(Option[T],Int) =
      pushOptionInside(numberedChild.filter({case (n,j) => j <= i && f(n)}).lastOption)
    val grouped = numberedChild.groupBy({case (n,i) => lastBefore(i)})
    grouped.keySet.toList.sortBy(_._2).map(grouped).map(l => l.map(_._1)) // ahem, unorded...
  }

  val pretty = new scala.xml.PrettyPrinter(300, 4)

  val maanden: List[(String, Int)] = List("jan", "feb", "maart", "apr", "mei", "jun", "jul", "aug", "sep", "o[kc]t", "nov", "dec").zipWithIndex

  def parseMonth(s: String): Option[Int] = maanden.find(x => s.toLowerCase.matches(x._1 + ".*")).map(_._2 + 1)

  def tryOrNone[T](x: () => T): Option[T]  = Try(x()) match {
    case Success(z) =>  Some(z)
    case _ => None
  }

  val tocFiles: Array[(Int, File)] = new File(XMLDirectory).listFiles.filter(_.getName.matches("[0-9]+"))
    .map(f => (f.getName.toInt ->  new File(f.getCanonicalPath + "/" + "toc.xml")))

  val pageLists: Array[(Int, File)] = new File(XMLDirectory).listFiles.filter(_.getName.matches("[0-9]+"))
    .map(f => (f.getName.toInt ->  new File(f.getCanonicalPath + "/" + "pagelist.xml")))

  lazy val pageMapping: Map[(Int, String), String] = pageLists.toList.flatMap({ case (v, f) => {
    val d = XML.loadFile(f)
    val base_url = ((d \\ "meta") \ "@base_url").text
    val mapping: Seq[((Int, String), String)] = (d \\ "page").map(p => {
      val pn = (p \ "@page_number").text;
      val img = (p \ "@original_image").text//.replaceAll(".tif$",".jpg")
      val url = s"$base_url/$img"
      (v, pn) -> url
    })
    mapping
  }}).toMap

  case class TocItem(volume: Int, pageNumber: String, title: String, level: Int, n: String,
                     parent: Option[TocItem]  = None, parsedTitle: Option[NodeSeq] = None)
  {
    def parseTitle = if (parsedTitle.nonEmpty) parsedTitle.get else {
      val t0 = title.replaceAll("(,\\s*geheim\\s*)$", "<secret>$1</secret>")
      val t1 = t0.replaceAll(",\\s*([^<,]*)(<|$)", "<date>$1</date>$2")
      val t2 = t1.replaceAll("(^[^<]*)", "<author>$1</author>")
        .replaceAll("<author>([IVXLC]{0,4}[ab]?)\\.","<N>$1</N><author>")
      val t3 = if (titleIsLocationDate) t2.replaceAll("author>", "place>") else t2

      Try(XML.loadString(s"<e>$t3</e>").child) match {
        case util.Success(x) => x
        case util.Failure(k) => println(k); scala.xml.Text(title)
      }
    }

    lazy val page: Int = pageNumber.toInt
    private lazy val titleZonderGeheim = title.replaceAll(",\\s*geheim\\s*$", "")
    lazy val date: String = titleZonderGeheim.replaceAll(".*, *", "").trim // dit werkt niet altijd...
    lazy val year: Option[Int] = "[0-9]{4}".r.findAllIn(date).toList.headOption.map(_.toInt)
    lazy val day: Option[Int] = tryOrNone(() => date.replaceAll("\\s.*","").toInt)
    lazy val month: Option[Int] = parseMonth(date.replaceAll("[0-9]+", "").trim)
    lazy val place: Option[String] = (parseTitle \\ "place").headOption.map(_.text)
    lazy val author: String = (parseTitle \\ "author").text // titleZonderGeheim.replaceAll(",[^,]*$","")
    lazy val better_n: String = if (n.nonEmpty) n else (parseTitle \\ "N").text
    lazy val key = s"$volume/$n/$title/$level/$pageNumber"
    lazy val authors: Array[String] = author.split("\\s*(,|\\sen\\s)\\s*")

    def interp[T](name: String, value: Option[T]): Seq[Node] =
      if (value.isEmpty)
        Seq()
      else
        <interpGrp inst={inst} type={name}><interp>{value.get.toString}</interp></interpGrp>

    def parentInfo = interp("parent_title", parent.map(_.title))

    def toXML: Elem = if (level == 1) <item level={level.toString}>
      <n>{better_n}</n>
      <title>{title}</title>
      <parsedTitle>{parseTitle}</parsedTitle>
      <level>{level}</level>
      <volume>{volume}</volume>
      <page>{page}</page>
      <author>{author}</author>
      <date>{date}</date>
      </item>
    else
      <item level={level.toString}><n>{better_n}</n><title>{title}</title>
        <level>{level}</level>
        <volume>{volume}</volume>
        <page>{page}</page>
      </item>

    def isIndex: Boolean = title.trim.toLowerCase().startsWith("index")

    def titleIsLocationDate: Boolean = (volume == 1 && page < 97 && level==1)
    def titleIsAuthorlocationDate: Boolean = (volume ==1 && (page >= 97 && page <= 121))

    def toTEI: Elem = if (isIndex || level == 0)
      <listBibl><bibl inst={inst}>
        {interp("page", Some(page))}
        {interp(value=Some(level), name = "tocLevel")}
        {interp(value=Some(volume), name = "volume")}
        {interp(value=Some(better_n), name = "n")}
        <interpGrp inst={inst} type="pid"><interp>{pid}</interp></interpGrp>
        <interpGrp inst={inst} type="titleLevel1"><interp>{title}</interp></interpGrp>
      </bibl></listBibl>
      else
      <listBibl>{parseTitle}<bibl type="missive" inst={inst}>
        {interp("page", Some(page) )}
        {parentInfo}
      <interpGrp inst={inst} type="titleLevel1"><interp>{title}</interp></interpGrp>
      <interpGrp inst={inst} type="dateLevel1"><interp>{date}</interp></interpGrp>
        {interp(value=Some(level), name = "tocLevel")}
      {interp(value=Some(volume), name = "volume")}
        {interp(value=Some(better_n), name = "n")}
      {interp("localization_placeLevel1", place )}
      {interp("witnessYearLevel1_from", year )}
      {interp("witnessYearLevel1_to", year )}
      {interp("witnessMonthLevel1_to", month )}
      {interp("witnessMonthLevel1_from", month )}
      {interp("witnessDayLevel1_to", day )}
      {interp("witnessDayLevel1_from", day )}
      <interpGrp inst={inst} type="authorLevel1">{authors.map(z => <interp>{z}</interp>)}</interpGrp>
      <interpGrp inst={inst} type="pid"><interp>{pid}</interp></interpGrp>
    </bibl></listBibl>

    def uuid():String =
    {
      val source = this.key.toString
      val bytes = source.getBytes("UTF-8")
      java.util.UUID.nameUUIDFromBytes(bytes).toString
    }

    lazy val pid = s"INT_${uuid()}"

    def inst  = s"#$pid"
  }

  def tocItemFromXML(b: Node, v:Option[Int] = None): TocItem =  { //
    val volume = v.getOrElse((b \ "volume").text.toInt)
    val pageNumber = (b \ "page").text
    val title =  (b \ "title").text
    val level = (b \ "@level").text.toInt
    val n = (b \ "n").text
    val parsedTitle = (b \\ "parsedTitle").headOption
    TocItem(volume, pageNumber, title, level, n, parsedTitle=parsedTitle)
  }


  def uuidForVolume(n: Int): String = {
    val source = "INT-generalemissiven-deeltje" + n
    val bytes = source.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }


  def getFieldFromBibl(b: Node, f: String) = (b \\ "interpGrp").filter(g => (g \ "@type").text == f).flatMap(g => (g \\ "interp").map(_.text)).mkString("//")

  private lazy val tocItems_unsorted: Map[Int, Seq[TocItem]] =
    tocFiles.flatMap({
          case (n, f) =>
            val z: immutable.Seq[TocItem] = (XML.loadFile(f) \\ "item").map(x => tocItemFromXML(x,Some(n)))
            z }
    ).groupBy(_.volume).mapValues(l => findParents(l))

  private lazy val corrected_tocItems: Map[Int, Seq[TocItem]] = (XML.load("data/Missiven/correctedTocs.xml") \\ "item")
    .map(x => tocItemFromXML(x,None))
    .groupBy(_.volume)
    .mapValues(l => findParents(l))

  private lazy val tocItems_read = if (Settings.readCorrectedTocs) corrected_tocItems else tocItems_unsorted

  private def findParents(l: Seq[TocItem]): Seq[TocItem] = {
    val grouped: Seq[Seq[TocItem]] = groupWithFirst[TocItem](l, t => t.level == 0 )
    grouped.flatMap(
      g => { if (g.head.level == 0) {
        g.map(t => if (t.level == 1) t.copy(parent=Some(g.head)) else t)
      } else g}
    )
  }

  lazy val tocItemsPerVolume: Map[Int, Seq[TocItem]] =
    tocItems_read.mapValues(l =>
      l.filter(_.pageNumber.matches("[0-9]+"))
        .sortBy(x => 10000 * x.page + x.level)) // kies liever een level 1 item (als laatste)

  def unusedTocItemsForVolume(n: Int, volume: Node) =
  {
    val usedIds: immutable.Seq[String] = (volume \\ "div").map(PageStructure.getId(_)).filter(_.isDefined).map(_.get)
    //println(usedIds)
    tocItemsPerVolume(n).filter(t => !usedIds.contains(t.pid))
  }

  def findTocItem(v: Int, p: Int): TocItem =
  {
    val bestMatch = tocItemsPerVolume(v).filter(_.page <= p).lastOption
    bestMatch.getOrElse(TocItem(0, "0", "no match", 0, "NOPE"))
  }

  def createHeaderForMissive(v: Int, bibl: Node): Elem = {
    val header = <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>{getFieldFromBibl(bibl, "titleLevel1")}</title>
        </titleStmt>
        <publicationStmt>
          <p>
            <date>{getFieldFromBibl(bibl, "dateLevel1")}</date>
            <idno type="sourceID">missiven:vol{v}</idno>
            <idno type="pid">{getFieldFromBibl(bibl, "pid")}</idno>
          </p>
        </publicationStmt>
        <notesStmt>
          <note/>
        </notesStmt>
        <sourceDesc>
          <listBibl xml:id="inlMetadata">
            {bibl}
          </listBibl>
        </sourceDesc>
      </fileDesc>
    </teiHeader>

      header
    }

  def addHeaderForVolume(v: Int, d: Elem): Elem = {
    val allTocItems = (d \\ "item").map(b => tocItemFromXML(b.asInstanceOf[Elem]))

    val header = <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>Tweede boek, deel I</title>
        </titleStmt>
        <publicationStmt>
          <p>
            <date></date>
            <idno type="sourceID">missiven:vol{v}</idno>
            <idno type="pid">INT_{uuidForVolume(v)}</idno>
          </p>
        </publicationStmt>
        <notesStmt>
          <note/>
        </notesStmt>
        <sourceDesc>
          <listBibl xml:id="inlMetadata">
            <bibl>
            </bibl>
          </listBibl>
          {allTocItems.map(t => t.toTEI)}
        </sourceDesc>
      </fileDesc>
    </teiHeader>

    def fixDivje(d: Node) = {
      val t = tocItemFromXML((d \ "item").head.asInstanceOf[Elem])
      <div xml:id={t.pid}>
        {d.child.filter(_.label != "item")}
      </div>
    }
    val d1 = updateElement(d, x => x.label == "div" && (x \ "@type").text == "missive", fixDivje)

    <TEI>
      {header}
      {d1.child}
    </TEI>
  }

  def parseTitles() = {
    val p = new PrintWriter("/tmp/parsed.xml")
    p.println("<bibls>")
    tocItemsPerVolume.toList.sortBy(_._1).foreach(
      {
        case (v, l) =>
          val volumeXML = <book n={v.toString}>{l.map(_.toXML)}</book>
          p.println(pretty.format(volumeXML))
      }
    )
    p.println("</bibls>")
    p.close()
  }

  def severalTocsOnAPage = tocItemsPerVolume.toList.sortBy(_._1).foreach({
    case (n,l) =>
      val z = l.groupBy(_.page).filter({case (p,k) => k.size > 1})
      z.foreach({case (p,l)  =>
          println(
            s"""###Vol $n Page $p
               |${l.map(_.key).mkString("\n")}
               |""".stripMargin)
      })
  })

  def main(args: Array[String]): Unit = {
    severalTocsOnAPage
    parseTitles()
    val p = new PrintWriter("/tmp/bibls.xml")
    p.println("<bibls>")
    tocItemsPerVolume.toList.sortBy(_._1).foreach(
      {
        case (v, l) =>
          val volumeXML = <volume n={v.toString}>{l.map(_.toTEI)}</volume>
          p.println(pretty.format(volumeXML))
      }
    )
    p.println("</bibls>")
    p.close()
  }
}
