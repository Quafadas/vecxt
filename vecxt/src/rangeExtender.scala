package vecxt.rangeExtender

import narr.*

export MatrixRange.*

object MatrixRange:

  type RangeExtender = Range | Int | NArray[Int] | ::.type

  inline def range(r: RangeExtender, max: Int): NArray[Int] = inline r match
    case _: ::.type     => NArray.from((0 until max).toArray)
    case r: Range       => NArray.from(r.toArray)
    case l: NArray[Int] => l
    case i: Int         => NArray(i)
end MatrixRange
