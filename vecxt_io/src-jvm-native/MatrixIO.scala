package vecxt_io

import vecxt.all.*
import scala.annotation.targetName
import scala.math.Numeric
import scala.reflect.ClassTag

/** Matrix CSV serialization: each line stores one matrix row. */
object MatrixIO:
  private inline def dropLeadingRows(lines: Seq[String], dropRows: Int): Seq[String] =
    if dropRows < 0 then throw new IllegalArgumentException(s"dropRows must be non-negative, got $dropRows")
    else lines.drop(dropRows)

  private inline def splitLine(line: String, seperator: Char): Array[String] =
    line.split(java.util.regex.Pattern.quote(seperator.toString), -1).map(_.trim)

  // See ArrayIO.parseValue: generic in `A`, but it touches no array at all.
  private def parseValue[A: Numeric](value: String): A =
    summon[Numeric[A]]
      .parseString(value)
      .getOrElse(
        throw new IllegalArgumentException(s"Could not parse matrix value '$value'")
      )

  extension [A: ClassTag](m: Matrix[A])
    def write(path: os.Path, seperator: Char = ','): Unit =
      if m.rows == 0 || m.cols == 0 then os.write.over(path, "")
      else
        val lines = Array.ofDim[String](m.rows)
        var row = 0
        while row < m.rows do
          lines(row) = m.row(row).mkString(seperator.toString)
          row += 1
        end while
        os.write.over(path, lines.mkString("\n"))
  end extension

  // Concrete overloads (vecxt/issues/105, check C6a). Matrix.row is itself `inline`, so a concrete
  // receiver here specializes through it - the generic version above boxes per element via
  // ScalaRunTime$.array_apply/array_update inside row's inlined body, then again via genericWrapArray on
  // the resulting Array[A].mkString; these avoid all three at once by construction, not by relabelling.
  extension (m: Matrix[Double])
    @targetName("writeMatrixDouble")
    def write(path: os.Path, seperator: Char): Unit =
      if m.rows == 0 || m.cols == 0 then os.write.over(path, "")
      else
        val lines = Array.ofDim[String](m.rows)
        var row = 0
        while row < m.rows do
          lines(row) = m.row(row).mkString(seperator.toString)
          row += 1
        end while
        os.write.over(path, lines.mkString("\n"))

    // Not `seperator: Char = ','`: Scala disallows more than one overloaded alternative of the same
    // method declaring a default. A plain forwarder gets the same call-site convenience without it.
    @targetName("writeMatrixDoubleDefaultSep")
    def write(path: os.Path): Unit = write(path, ',')
  end extension

  extension (m: Matrix[Float])
    @targetName("writeMatrixFloat")
    def write(path: os.Path, seperator: Char): Unit =
      if m.rows == 0 || m.cols == 0 then os.write.over(path, "")
      else
        val lines = Array.ofDim[String](m.rows)
        var row = 0
        while row < m.rows do
          lines(row) = m.row(row).mkString(seperator.toString)
          row += 1
        end while
        os.write.over(path, lines.mkString("\n"))

    @targetName("writeMatrixFloatDefaultSep")
    def write(path: os.Path): Unit = write(path, ',')
  end extension

  extension (m: Matrix[Int])
    @targetName("writeMatrixInt")
    def write(path: os.Path, seperator: Char): Unit =
      if m.rows == 0 || m.cols == 0 then os.write.over(path, "")
      else
        val lines = Array.ofDim[String](m.rows)
        var row = 0
        while row < m.rows do
          lines(row) = m.row(row).mkString(seperator.toString)
          row += 1
        end while
        os.write.over(path, lines.mkString("\n"))

    @targetName("writeMatrixIntDefaultSep")
    def write(path: os.Path): Unit = write(path, ',')
  end extension

  extension (m: Matrix[Long])
    @targetName("writeMatrixLong")
    def write(path: os.Path, seperator: Char): Unit =
      if m.rows == 0 || m.cols == 0 then os.write.over(path, "")
      else
        val lines = Array.ofDim[String](m.rows)
        var row = 0
        while row < m.rows do
          lines(row) = m.row(row).mkString(seperator.toString)
          row += 1
        end while
        os.write.over(path, lines.mkString("\n"))

    @targetName("writeMatrixLongDefaultSep")
    def write(path: os.Path): Unit = write(path, ',')
  end extension

  extension (m: Matrix[Boolean])
    @targetName("writeMatrixBoolean")
    def write(path: os.Path, seperator: Char): Unit =
      if m.rows == 0 || m.cols == 0 then os.write.over(path, "")
      else
        val lines = Array.ofDim[String](m.rows)
        var row = 0
        while row < m.rows do
          lines(row) = m.row(row).mkString(seperator.toString)
          row += 1
        end while
        os.write.over(path, lines.mkString("\n"))

    @targetName("writeMatrixBooleanDefaultSep")
    def write(path: os.Path): Unit = write(path, ',')
  end extension

  def loadMatrix[A: Numeric: ClassTag](
      path: os.Path | os.ResourcePath,
      seperator: Char = ',',
      dropRows: Int = 0
  ): Matrix[A] =
    val allLines = path match
      case p: os.Path         => os.read.lines(p)
      case p: os.ResourcePath => os.read.lines(p)
    val lines = dropLeadingRows(allLines, dropRows)

    if lines.isEmpty then Matrix(Array.empty[A], (0, 0))
    else
      val firstRow = splitLine(lines.head, seperator)
      if firstRow.isEmpty || firstRow.exists(_.isEmpty) then
        throw new IllegalArgumentException("Matrix file contains an empty value in the first row")
      end if

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
        end if

        var col = 0
        while col < cols do
          val value = values(col)
          if value.isEmpty then
            throw new IllegalArgumentException(
              s"Matrix file contains an empty value at row ${row + 1}, column ${col + 1}"
            )
          end if

          data(row + col * rows) = parseValue[A](value)
          col += 1
        end while

        row += 1
      end while

      Matrix(data, (rows, cols))
    end if

  end loadMatrix

  def fromResource[A: Numeric: ClassTag](
      resourceName: String,
      seperator: Char = ',',
      dropRows: Int = 0
  ): Matrix[A] =
    loadMatrix(os.resource / resourceName, seperator, dropRows)

end MatrixIO
