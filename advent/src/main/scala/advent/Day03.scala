package advent

object Day03 {

  case class Rectangle(id: Int, x: Int, y: Int, w: Int, h: Int)
  object Rectangle {
    def parse(str: String) = {
      val regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
      str match {
        case regex(id, x, y, w, h) => Rectangle(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
      }
    }
  }

  case class Point(id: Int, x: Int, y: Int)

  def overlap(rectangles: Seq[Rectangle]) = {
    val points = for {
      rectangle <- rectangles
      x <- 0 until rectangle.w
      y <- 0 until rectangle.h
    } yield (rectangle.x + x, rectangle.y + y)

    points.groupBy(identity).count(_._2.size > 1)
  }

  def noOverlap(rectangles: Seq[Rectangle]) = {
    val points = for {
      rectangle <- rectangles
      x <- 0 until rectangle.w
      y <- 0 until rectangle.h
    } yield Point(rectangle.id, rectangle.x + x, rectangle.y + y)

    val overlappingIds = points.groupBy(x => (x.x, x.y))
      .filter(_._2.size > 1)
      .values
      .toSeq
      .flatMap(identity)
      .map(_.id)
      .toSet

    val allIds = rectangles.map(_.id).toSet

    allIds.diff(overlappingIds).head
  }
}
