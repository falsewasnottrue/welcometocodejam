package welcome

object Welcome extends App {

  val pattern = "welcome to code jam".toList

//  val text = "elcomew elcome to code jam".toList
//  val text = "wweellccoommee to code qps jam".toList
  val text = "welcome to codejam".toList

  println(findPattern(text, pattern))

  def findPattern(text: List[Char], pattern: List[Char]): Int =
    pattern match {
      case p :: Nil => text.filter(_ == p).size
      case p :: ps => {
        val indices = text.zipWithIndex.filter(_._1 == p).map(_._2)
        indices.map(i => text.drop(i + 1)).filter(s => s.size >= ps.size).map(t => findPattern(t, ps)).sum
      }
      case _ => throw new IllegalArgumentException("pattern must not be empty")
    }
}
