package vecxt_io

import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.no
import scala.math.Numeric
import scala.reflect.ClassTag

object MatrixIO:
  extension [A: ClassTag](m: Matrix[A])
    def write(path: os.Path, seperator: Char = ','): Unit =
      os.write.over(path, "") // Clean and create file
      val (r, c) = m.shape
      for (i <- 0 until r) do
          val value = m(Array(i), ::)
          os.write.append(path, value.deepCopy.raw.mkString(seperator.toString) + "\n")

    // TODO parse from lines to make cross platform. Maybe
  def loadMatrix[A: Numeric: ClassTag](path: os.Path, seperator: Char = ','): Matrix[A] =
    val lines = os.read.lines(path)

    val headerline = lines.head
    val cols = headerline.split(" ")(1).toInt

    val rows = lines.length - 1
    val data = new Array[A](rows * cols)

    for (line, i) <- lines.tail.zipWithIndex do
      val arr = line.split(seperator).map(_.trim).filter(_.nonEmpty)

      if arr.length != cols then
        throw new RuntimeException(s"...")
      arr.zipWithIndex.foreach { (s, j) =>
        val value = Numeric[A].parseString(s).getOrElse(
          throw new RuntimeException(s"...")
        )

        data(i + j * rows) = value
      }
    Matrix(data, (rows, cols))


  end loadMatrix