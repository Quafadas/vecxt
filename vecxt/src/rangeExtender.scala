package vecxt.rangeExtender

import narr.*

export MatrixRange.*

object MatrixRange:

  type RangeExtender = Range | NArray[Int] | ::.type | IntArray

  inline def range(r: RangeExtender, max: Int): NArray[Int] = r match
    case _: ::.type     => NArray.from((0 until max).toArray)
    case r: Range       => NArray.from(r.toArray)
    case l: NArray[Int] => l

end MatrixRange
