package advent

object Day02 {

  def containsRequiredCharacters(input: String, numberRequired: Int) : Boolean = {
    val letterFrequency = input.groupBy(identity).mapValues(_.size).values.toSeq
    letterFrequency.contains(numberRequired)
  }

  def checksum(input: Seq[String]): Int = {
    val codesWith2 = for { 
      code <- input 
      if (containsRequiredCharacters(code, 2)) 
    } yield code 

    val codesWith3 = for { 
      code <- input 
      if (containsRequiredCharacters(code, 3)) 
    } yield code 

    codesWith2.length * codesWith3.length
  }

  def commonCharacters(input: Seq[String]): String = {
    val matchers = for {
      a <- input
      b <- input
      sameLetters = a.zip(b).filter(x => x._1 == x._2)
      if sameLetters.length == a.length - 1
    } yield a.intersect(b)

    matchers.head
  }
}
