package vecxt.rangeExtender

export MatrixRange.*

object MatrixRange:

  type RangeExtender = Range | Array[Int] | ::.type

  inline def range(r: RangeExtender, max: Int): Array[Int] = r match
    case _: ::.type    => Array.from((0 until max).toArray)
    case r: Range      => Array.from(r.toArray)
    case l: Array[Int] => l

end MatrixRange
