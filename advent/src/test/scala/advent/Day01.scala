package advent

import org.scalatest._

class FrequencySpec extends FlatSpec with Matchers {

  import scala.io.Source

  def readInput(input: String): Seq[String] = {
    Source.fromFile(input).getLines toSeq
  }

  "frequency" should "be the sum of inputs" in {
    Day01.frequency(List(1, -2, 3, 1)) shouldEqual 3
  }

  "frequency" should "get the first star" in {
    val input = readInput("input/day01").map(_.toInt)
    Day01.frequency(input) shouldEqual 435
  }
}
