package vecxt_io

import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.no
import scala.math.Numeric
import scala.reflect.ClassTag

/** Matrix CSV serialization: each line stores one matrix row. */
object MatrixIO:
  private inline def dropLeadingRows(lines: Seq[String], dropRows: Int): Seq[String] =
    if dropRows < 0 then throw new IllegalArgumentException(s"dropRows must be non-negative, got $dropRows")
    else lines.drop(dropRows)

  private inline def splitLine(line: String, seperator: Char): Array[String] =
    line.split(java.util.regex.Pattern.quote(seperator.toString), -1).map(_.trim)

  private inline def parseValue[A: Numeric](value: String): A =
    summon[Numeric[A]].parseString(value).getOrElse(
      throw new IllegalArgumentException(s"Could not parse matrix value '$value'")
    )

  extension [A: ClassTag](m: Matrix[A])
    def write(path: os.Path, seperator: Char = ','): Unit =
      if m.rows == 0 || m.cols == 0 then
        os.write.over(path, "")
      else
        val lines = Array.ofDim[String](m.rows)
        var row = 0
        while row < m.rows do
          lines(row) = m.row(row).mkString(seperator.toString)
          row += 1
        end while
        os.write.over(path, lines.mkString("\n"))

  def loadMatrix[A: Numeric: ClassTag](
      path: os.Path | os.ResourcePath,
      seperator: Char = ',',
      dropRows: Int = 0
  ): Matrix[A] =
    val allLines = path match
      case p: os.Path         => os.read.lines(p)
      case p: os.ResourcePath => os.read.lines(p)
    val lines = dropLeadingRows(allLines, dropRows)

    if lines.isEmpty then
      Matrix(Array.empty[A], (0, 0))(using no)
    else
      val firstRow = splitLine(lines.head, seperator)
      if firstRow.isEmpty || firstRow.exists(_.isEmpty) then
        throw new IllegalArgumentException("Matrix file contains an empty value in the first row")

      val rows = lines.length
      val cols = firstRow.length
      val data = new Array[A](rows * cols)

      var row = 0
      while row < rows do
        val values = splitLine(lines(row), seperator)
        if values.length != cols then
          throw new IllegalArgumentException(
            s"Expected $cols values in row ${row + 1}, but found ${values.length}"
          )

        var col = 0
        while col < cols do
          val value = values(col)
          if value.isEmpty then
            throw new IllegalArgumentException(
              s"Matrix file contains an empty value at row ${row + 1}, column ${col + 1}"
            )

          data(row + col * rows) = parseValue[A](value)
          col += 1
        end while

        row += 1
      end while

      Matrix(data, (rows, cols))(using no)


  end loadMatrix

  def fromResource[A: Numeric: ClassTag](
      resourceName: String,
      seperator: Char = ',',
      dropRows: Int = 0
  ): Matrix[A] =
    loadMatrix(os.resource / resourceName, seperator, dropRows)

end MatrixIO