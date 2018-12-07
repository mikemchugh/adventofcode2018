package advent

import org.scalatest._
import scala.io.Source

class ChecksumSpec extends FlatSpec with Matchers {

  def readInput(input: String): Seq[String] = {
    Source.fromFile(input).getLines toSeq
  }

  "frequency" should "be the sum of inputs" in {
    Day02.checksum(
      List("abcdef", 
        "bababc", 
        "abbcde", 
        "abcccd", 
        "aabcdd", 
        "abcdee", 
        "ababab")) shouldEqual 12
  }

  "frequency" should "get the first star" in {
    val input = readInput("input/day02")
    Day02.checksum(input) shouldEqual 5880
  }

  "commonCharacters" should "get the characters in common between two box IDs where the letters only differ by one character" in {
    Day02.commonCharacters(
    List("abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz")) shouldEqual "fgij"
  }

  "commonCharacters" should "get the second star" in {
    val input = readInput("input/day02")
    Day02.commonCharacters(input) shouldEqual "tiwcdpbseqhxryfmgkvjujvza"
  }
}
