package vecxt_io

import scala.math.Numeric
import scala.reflect.ClassTag

object ArrayIO:
  private inline def dropLeadingRows(lines: Seq[String], dropRows: Int): Seq[String] =
    if dropRows < 0 then throw new IllegalArgumentException(s"dropRows must be non-negative, got $dropRows")
    else lines.drop(dropRows)

  private inline def splitLine(line: String, seperator: Char): Array[String] =
    line.split(java.util.regex.Pattern.quote(seperator.toString), -1).map(_.trim)

  private inline def parseValue[A: Numeric](value: String): A =
    summon[Numeric[A]].parseString(value).getOrElse(
      throw new IllegalArgumentException(s"Could not parse array value '$value'")
    )

  extension [A](arr: Array[A])
    def write(path: os.Path, seperator: Char = ','): Unit =
      os.write.over(path, arr.mkString(seperator.toString))

  def loadArray[A: Numeric: ClassTag](
      path: os.Path | os.ResourcePath,
      seperator: Char = ',',
      dropRows: Int = 0
  ): Array[A] =
    val allLines = path match
      case p: os.Path         => os.read.lines(p)
      case p: os.ResourcePath => os.read.lines(p)
    val lines = dropLeadingRows(allLines, dropRows)

    if lines.isEmpty then Array.empty[A]
    else
      val builder = Array.newBuilder[A]
      var lineIndex = 0
      while lineIndex < lines.length do
        val values = splitLine(lines(lineIndex), seperator)
        var valueIndex = 0
        while valueIndex < values.length do
          val value = values(valueIndex)
          if value.isEmpty then
            throw new IllegalArgumentException(
              s"Array file contains an empty value at line ${lineIndex + 1}, column ${valueIndex + 1}"
            )

          builder += parseValue[A](value)
          valueIndex += 1
        end while
        lineIndex += 1
      end while
      builder.result()

  def fromResource[A: Numeric: ClassTag](
      resourceName: String,
      seperator: Char = ',',
      dropRows: Int = 0
  ): Array[A] =
    loadArray(os.resource / resourceName, seperator, dropRows)

end ArrayIO