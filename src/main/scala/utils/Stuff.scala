package utils

object Stuff {
  def makeGroupx[T](s: Seq[T], currentGroup:List[T], f: T=>Boolean):Stream[List[T]] =
  {
    if (s.isEmpty) Stream(currentGroup)
    else if (f(s.head))
      Stream.cons(currentGroup, makeGroupx(s.tail, List(s.head), f))
    else
      makeGroupx(s.tail, currentGroup :+ s.head, f)
  }

  def makeGroup[T](s: Seq[T], f: T=>Boolean):Seq[List[T]] =
  {
    makeGroupx[T](s, List.empty, f).filter(_.nonEmpty)
  }

  def groupByOx[T,U >:Null](s: Seq[T], currentGroup:List[T], f: T=>U, lastVal: U):Stream[(U, List[T])] =
  {
    if (s.isEmpty) Stream(lastVal -> currentGroup)
    else if (currentGroup.isEmpty)
      groupByOx(s.tail, List(s.head), f, f(s.head))
    else if (f(s.head) != lastVal)
      Stream.cons(lastVal -> currentGroup, groupByOx(s.tail, List(s.head), f, f(s.head)))
    else
      groupByOx(s.tail, currentGroup :+ s.head, f, lastVal)
  }

  def groupBy[T ,U >:Null](s: Seq[T], f: T=>U ) =
  {
    val unul:U = null
    val z = groupByOx[T,U](s, List.empty, f, unul)
    z
  }

  def uniq[U >:Null](s: Seq[U]) = groupBy[U,U](s, identity[U]).map(_._2).map(_.head).toSeq

  def main(args: Array[String]): Unit = {
    val s = List("aap", "noot", "nu", "mies")
    println(groupBy[String,String](s, s => s.head.toString))
  }
}
