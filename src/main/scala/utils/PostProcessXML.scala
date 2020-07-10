package utils

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.xml
import scala.xml._


/*
ToDo: houd rekening met de sets.
- Pas alleen toe op tags uit een op te geven set
- Pas de setdeclaratie in de header aan
 */

object PostProcessXML {

  def updateElement(e: Elem, condition: Elem=>Boolean, f: Elem => Elem):Elem =
  {
    if (condition(e))
      f(e)
    else
      e.copy(child = e.child.map({
        {
          case e1: Elem => updateElement(e1,condition,f)
          case n:Node => n
        }
      }))
  }

  def updateElement2(e: Elem, condition: Elem=>Boolean, f: Elem => NodeSeq):NodeSeq =
  {
    if (condition(e))
      f(e)
    else
      e.copy(child = e.child.flatMap({
        {
          case e1: Elem => updateElement2(e1,condition,f)
          case n:Node => Seq(n)
        }
      }))
  }


  def updateElement3(e: Elem, condition: Elem=>Boolean, f: Elem => Elem):Elem =
  {
    val bla:Seq[Node] = e.child.map({
      case e1: Elem => updateElement3(e1,condition,f).asInstanceOf[Node]
      case n: Node => n.asInstanceOf[Node]
    })

    if (condition(e))
      f(e.copy(child=bla))
    else
      e.copy(child = bla)
  }

  def updateElement4(e: Elem, condition: Elem=>Boolean, f: Elem => NodeSeq):NodeSeq =
  {
    val newChildren =  e.child.flatMap({
      {
        case e1: Elem => updateElement2(e1,condition,f)
        case n:Node => Seq(n)
      }
    })

    if (condition(e)) {
      f(e.copy(child = newChildren))
    }
    else
      e.copy(child = newChildren)
  }

  def updateElement5(e: Elem, condition: Elem=>Boolean, f: Elem => NodeSeq):NodeSeq =
  {
    val newChildren =  e.child.flatMap({
      {
        case e1: Elem => updateElement5(e1,condition,f)
        case n:Node => Seq(n)
      }
    })

    if (condition(e)) {
      f(e.copy(child = newChildren))
    }
    else
      e.copy(child = newChildren)
  }

  def pushOptionInside(o: Option[(Node,Int)]):(Option[Node], Int) =
    o.map(x => (Some(x._1).asInstanceOf[Option[Node]],x._2)).getOrElse( (None, 0) )

  def groupWithFirst(l: NodeSeq, f: Node => Boolean): Seq[Seq[Node]] =
  {
    val numberedChild:List[(Node, Int)] = l.toList.zipWithIndex
    def lastBefore(i:Int):(Option[Node],Int) = pushOptionInside(numberedChild.filter({case (n,j) => j <= i && f(n)}).lastOption)
    val grouped = numberedChild.groupBy({case (n,i) => lastBefore(i)})
    grouped.keySet.toList.sortBy(_._2).map(grouped).map(l => l.map(_._1)) // ahem, unorded...
  }
}

// ToDo deze manier van cachen is niet threadsafe

