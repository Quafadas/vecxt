package vecxt

import all.*
import dimensionExtender.DimensionExtender.Dimension.*

class IntMatrixReduceSuite extends munit.FunSuite:

  test("sum reduction, genuinely non-contiguous (padded strides)"):
    // rows=2, cols=2, rowStride=1, colStride=3, offset=0 over a length-6 backing array: col 0 is raw(0),raw(1) =
    // [1,2], col 1 is raw(3),raw(4) = [3,4], and raw(2)/raw(5) are unused padding. dataLength (6) != numel (4),
    // so hasSimpleContiguousMemoryLayout is false - reduceAlongDimension used to bail out with `???` for exactly
    // this shape, even though its loop already reads every element through m.layout.linearIndex, which handles
    // arbitrary offsets/strides correctly regardless of contiguity.
    val mat = Matrix[Int](Array(1, 2, 99, 3, 4, 99), 2, 2, 1, 3, 0)
    assert(!mat.hasSimpleContiguousMemoryLayout)

    assertMatrixEquals(mat.sum(Rows), Matrix[Int](Array(4, 6), (2, 1)))
    assertMatrixEquals(mat.sum(Cols), Matrix[Int](Array(3, 7), (1, 2)))

  test("max/min/product reduction, genuinely non-contiguous (padded strides)"):
    val mat = Matrix[Int](Array(1, 2, 99, 3, 4, 99), 2, 2, 1, 3, 0)

    assertMatrixEquals(mat.max(Rows), Matrix[Int](Array(3, 4), (2, 1)))
    assertMatrixEquals(mat.min(Cols), Matrix[Int](Array(1, 3), (1, 2)))
    assertMatrixEquals(mat.product(Rows), Matrix[Int](Array(3, 8), (2, 1)))

end IntMatrixReduceSuite
