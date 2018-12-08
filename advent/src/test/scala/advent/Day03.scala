package advent

import org.scalatest._
import scala.io.Source

class OverlapSpec extends FlatSpec with Matchers {

  def readInput(input: String): Seq[String] = {
    Source.fromFile(input).getLines toSeq
  }

  "overlap" should "be the square inches that overlap" in {
    Day03.overlap(
      List("#1 @ 1,3: 4x4",
        "#2 @ 3,1: 4x4",
        "#3 @ 5,5: 2x2").map(Day03.Rectangle.parse)) shouldEqual 4
  }

  "overlap" should "get the first star" in {
    val input = readInput("input/day03").map(Day03.Rectangle.parse)
    Day03.overlap(input) shouldEqual 113576
  }

  "noOverlap" should "return the only id that does not overlap" in {
    Day03.noOverlap(
      List("#1 @ 1,3: 4x4",
        "#2 @ 3,1: 4x4",
        "#3 @ 5,5: 2x2").map(Day03.Rectangle.parse)) shouldEqual 3
  }

  "noOverlap" should "get the second star" in {
    val input = readInput("input/day03").map(Day03.Rectangle.parse)
    Day03.noOverlap(input) shouldEqual 825
  }

}
