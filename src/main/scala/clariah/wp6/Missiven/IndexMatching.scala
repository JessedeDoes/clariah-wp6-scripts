package clariah.wp6.Missiven

import java.io.File

import utils.EditDistance

import scala.xml._
import utils.PostProcessXML._

import scala.collection.{immutable, mutable}
import scala.util.matching.Regex._
import org.apache.commons.text._


object IndexMatching {
  lazy val placeIndex = Settings.part6PlaceIndexDocument
  lazy val personIndex = Settings.part6PersonIndexDocument
  lazy val shipIndex = Settings.part6ShipIndexDocument


  def extractPages(d: Elem) = {
    val leafSpul = d.descendant.filter(_.child.isEmpty)
    val pages = groupWithFirst(leafSpul, x => x.label == "pb").map(g =>
      if (g.head.label == "pb") Some((g.head \ "@n").text.toInt) -> g.map(_.text).mkString(" ") else None -> g.text
    )
    pages.filter(_._1.nonEmpty)
  }

  def makeLines(p: Node) = {
    val lineGroups = groupWithFirst(p.child, x => x.label == "lb").map(c => <l>
      {c}
    </l>)
    p.asInstanceOf[Elem].copy(child = lineGroups)
  }

  case class IndexTerm(term: String, pagez: String, tag: String) {
    val pages: List[Int] = "[0-9]+(-[0-9]+)?".r.findAllIn(pagez).toList.flatMap(s => {
      if (s.contains("-")) {
        val x = s.split("-").map(_.toInt)
        (x(0) to x(1)).toList
      } else List(s.toInt)
    })

    lazy val tokensInTerm: List[String] = term.split("[\\s,.]+").toList.map(_.toLowerCase.replaceAll("[\\s,].*", "").replaceAll("\\(.*?\\)", ""))

    lazy val length = tokensInTerm.size

    lazy val firstPart = term.toLowerCase.replaceAll("[\\s,].*", "").replaceAll("\\(.*?\\)", "")

    override def toString = s"[$term] $pages"
  }

  case class Index(index: Elem, tag: String) {

    def expand(seq: Seq[(String, String)]) = seq.foldLeft(Seq[(String, String)]()) { case (s, (t, p)) =>
      val item = (if (t.isEmpty && s.nonEmpty) s.last._1 else t) -> p
      s ++ Seq(item)
    }

    lazy val indexTerms0 = (index \\ "p").flatMap(
      p => {
        val lines = makeLines(p)
        val t_o_s = (lines \ "l").map(l => {
          val t = l.text
          val term = t.replaceAll("[.\\s,0-9-]+$", "")
          val pages = "[.\\s,0-9-]+$".r.findAllIn(t).mkString("")
          // Console.err.println(term  + " --> " + pages)
          term.trim -> pages.trim
        })
        val expanded_t_o_s = expand(t_o_s)
        expanded_t_o_s.filter(x => x._1.nonEmpty && x._2.nonEmpty)
      }
    )

    lazy val indexTerms1 = expand(indexTerms0)

    lazy val indexTerms: Seq[IndexTerm] = indexTerms1.map { case (t, i) => IndexTerm(t, i, tag) }

    lazy val p2t = indexTerms.flatMap(t => t.pages.map(p => p -> t)).groupBy(_._1) // .mapValues(l => l.map(tag -> _._2))

    def asXML = <index tag={tag}>
      {indexTerms.map(t => <term pages={t.pages.toString}>
        {t.term}
      </term>)}
    </index>

    lazy val termIndex = indexTerms.groupBy(t => t.term.replaceAll("[,\\s].*", "").toLowerCase())

    def findMatch(s: String) = termIndex.getOrElse(s.toLowerCase, List())
  }

  lazy val index = Index(placeIndex, "location")
  lazy val pIndex = Index(personIndex, "person")
  lazy val sIndex = Index(shipIndex, "ship")

  lazy val indices = List(index, pIndex, sIndex)

  def main(args: Array[String]): Unit = {
    Settings.part6Files.foreach(f => {
      Console.err.println(f.getCanonicalPath)
      val d = XML.loadFile(f)
      val fOut = "/tmp/match." + f.getName
      val processedPages: scala.Seq[_root_.scala.xml.Elem] = matchIndexTerms(d)
      XML.save(fOut, <matchTest>
        {processedPages}
      </matchTest>, "UTF-8")
    })
  }

  val useGlobalMatches = true

  private def matchIndexTerms(d: Elem): Seq[Elem] = {
    val allPages = extractPages(d)


    val processedPages: Seq[Elem] = allPages.map {

      case (Some(p), t) =>

        val termsForPage: Seq[(Int, IndexTerm)] = indices.flatMap(index => index.p2t.getOrElse(p, List()))
        val matchedTermSet: mutable.Set[IndexTerm] = new mutable.HashSet[IndexTerm]()

        def singleTokenMatch(t: String) = {
          // val termsForPage: Seq[(Int, IndexTerm)] = ???
          //val matchedTermSet: mutable.Set[IndexTerm] = ???

          val tok = utils.Tokenizer.tokenizeOne(t)
          val token = tok.token.replaceAll("'s$", "")
          val distances: Seq[((Int, IndexTerm), Int)] =
            termsForPage.map(term => term -> EditDistance.distance(term._2.firstPart, token.toLowerCase)).sortBy(_._2)
          val dMin = distances.head._2
          val bestMatch: (Int, IndexTerm) = distances.head._1
          val upperFirst = token.matches("^[A-Z].*")

          if (upperFirst && (dMin == 0 || dMin == 1 && token.length >= 4 || 2 >= dMin && token.length > 5)) {
            matchedTermSet.add(bestMatch._2)
            t -> Some(dMin -> bestMatch._2)
            //t -> Some(bestMatch)
          }
          else {
            val globalMatches: immutable.Seq[IndexTerm] = indices.flatMap(_.findMatch(token))
            if (useGlobalMatches && upperFirst && token.length > 4 && globalMatches.nonEmpty) {
              t -> Some(100 -> globalMatches.head)
            }
            else
              t -> None // [(Int,IndexTerm)] // Seq(Text(t + " "))
          }
        }
        // println(s"\n\n#### $p ### $termsForPage ###")

        if (termsForPage.nonEmpty) {
          val tokens = t.split("\\s+").toList


          val tokenMatches: Seq[(String, Option[(Int, IndexTerm)])] = tokens.map(t => singleTokenMatch(t))

          val multiTokenMatches = tokens.zipWithIndex.map(
            {
              case (w, i) =>
                val termMatches = termsForPage.map(_._2).filter(t => i + t.tokensInTerm.size < tokens.size).map(t => {
                  val k = t.tokensInTerm.size
                  val permutations = t.tokensInTerm.permutations.toList
                  val seqToMatch = tokens.slice(i, i + k).map(_.toLowerCase).map(utils.Tokenizer.tokenizeOne(_).token)

                  def seqDist(p: List[String]) = (0 until k).map(j => {
                    val w = seqToMatch(j)
                    val d = EditDistance.distance(w, p(j))
                    if (d == 0 || d == 1 && w.length >= 4 || d <= 2 && w.length > 5) d else 1000
                  }).sum

                  val bestPerm = permutations.minBy(seqDist)
                  val bestValue = seqDist(bestPerm) / seqToMatch.mkString("").length.asInstanceOf[Double]
                  bestValue -> t
                }).sortBy(_._1).filter({ case (d, t) => d < 0.3 }).headOption // filter: multiword match moet beter dan willekeurig zijn...
                w -> i -> termMatches
            })

          val decentMultis: List[((String, Int), Option[(Double, IndexTerm)])] = multiTokenMatches.filter({
            case ((w, i), Some((d, t))) => t.tokensInTerm.size > 1
            case _ => false
          })

          decentMultis.foreach {
            case ((w, i), Some((d, t))) =>
              val matchedPart = tokens.slice(i, i + t.tokensInTerm.size).mkString(" ")
              println(s"$matchedPart ---> $t")
          }

          val multiTermStartsMap = decentMultis.map {
            case ((w, i), Some((d, t))) =>
              i -> t
          }.toMap

          def extendGrouplet(b: Seq[(Seq[String], Option[IndexTerm])], i: Int) = {
            val t: Option[IndexTerm] = multiTermStartsMap.get(i)
            // Console.err.println(i)
            val z = if (i == 0 || b.last._2.isEmpty || b.last._2.get.length == b.last._1.size) {
              val tokz: Seq[String] = Seq(tokens(i))
              val z1 = Seq((tokz, t));
              b ++ z1
            } else {
              val newLast: (Seq[String], Option[IndexTerm]) = (b.last._1 ++ Seq(tokens(i)), b.last._2)
              b.dropRight(1) ++ Seq(newLast)
            }
            z
          }

          val grouplets: Seq[(Seq[String], Option[IndexTerm])] = tokens.indices.foldLeft(Seq[(Seq[String], Option[IndexTerm])]())({ case (b, i) => extendGrouplet(b, i) })

          val mp2 = grouplets.flatMap { case (s, t) =>
            // Console.err.println(s"$s $t")
            if (t.isEmpty) {
              s.map(singleTokenMatch).flatMap(x => x match {
                case (w, Some((d, t))) =>
                  val (matchType, dx) = if (d == 100) ("global", 0) else ("local", d)
                  Seq(<name matchType={matchType} type={t.tag} norm={t.term} distance={d.toString}>
                    {w}
                  </name>, Text(" "))
                case (w, _) => Seq(Text(w + " "))
              })
            } else {
              val t0 = t.get
              Seq(<name matchType="multiWord" type={t0.tag} norm={t0.term}>
                {s.mkString(" ")}
              </name>, Text(" "))
            }
          }

          val matchedPage = tokenMatches.flatMap(x => {
            x match {
              case (w, Some((d, t))) => Seq(<name type={t.tag} norm={t.term} distance={d.toString}>
                {w}
              </name>, Text(" "))
              case (w, _) => Seq(Text(w + " "))
            }
          })

          val termsOnPageXML = <terms>
            {termsForPage.map(t => <term matched={matchedTermSet.contains(t._2).toString} type={t._2.tag}>
              {t._2.term}
            </term>)}
          </terms>

            // println(WordUtils.wrap(matchedPage.mkString(" "), 60))

            val ee:Elem = <page n={p.toString}>
              <matchTerms>
                {termsOnPageXML}
              </matchTerms> <content>
              {mp2}
            </content>
            </page>
            ee
        } else <page n={p.toString}/>
      case _ => <noPage/>
    }
    processedPages
  }
}
