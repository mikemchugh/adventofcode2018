package advent

object Day01 {

  def frequency(input: Seq[Int]): Int = {
    input.sum
  }

  def firstRepeatingFrequency(frequencies: Seq[Int]): Int = {
    def firstRepeatingFrequency(
      frequencies: Seq[Int], 
      frequenciesUsed: Map[Int, Int], 
      currentTotal: Int): Int = {
      val current = frequencies.head
      if (frequenciesUsed.contains(currentTotal + current))
        currentTotal + current
      else {
        firstRepeatingFrequency(
          frequencies.tail, 
          frequenciesUsed + (currentTotal + current -> 1), 
          currentTotal + current)
      }
    }

    def xs: Stream[Int] = frequencies.toStream #::: xs

    val frequenciesUsed = Map(0 -> 1)
    firstRepeatingFrequency(xs, frequenciesUsed, 0)
  }
}
