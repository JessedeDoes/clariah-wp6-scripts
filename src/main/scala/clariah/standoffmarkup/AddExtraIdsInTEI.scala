package clariah.standoffmarkup


import scala.xml._

object AddExtraIdsInTEI {
  // <interpGrp type="pid"><interp>INT_d62e680b-0979-365d-b59c-ddbaaa31d416</interp></interpGrp>
  def getPid(d: Elem) = ((d \\ "interpGrp").filter(x => (x \ "@type").text == "pid") \ "interp").text

  def completeIds(d: Elem)  = {
    val pid = getPid(d)
    assignIds(d, pid, 1)
  }

  def assignIds(e: Elem, prefix: String, k: Int, noIds:Set[String] = Set()): Elem =
  {
    val myId = s"$prefix.${e.label}.$k"
    //Console.err.println(myId)
    val children: Map[String, Seq[((Node, Int), Int)]] =
      e.child.toList.zipWithIndex.filter(_._1.isInstanceOf[Elem]).groupBy(_._1.label).mapValues(_.zipWithIndex)

    val indexMapping: Map[Int, Int] =
      children.values.flatMap(x => x).map( {case ((n,i),j) => i -> j} ).toMap


    val newChildren: Seq[Node] = if (noIds.contains(e.label)) e.child else e.child.zipWithIndex.map({
      case (e1: Elem, i) => val k1 = indexMapping(i); assignIds(e1, myId, k1+1)
      case (x, y) => x
    })

    val idAtts = e.attributes.filter(_.key == "id")
    val newAtts = if (e.label == "TEI" || idAtts.nonEmpty)
      e.attributes else  e.attributes.filter(_.key != "id")
      .append(new PrefixedAttribute("xml", "id", myId, Null))

    e.copy(child=newChildren, attributes = newAtts)
  }
}
