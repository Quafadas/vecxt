package vecxt

import all.*
import dimensionExtender.DimensionExtender.Dimension.*

class FloatMatrixReduceSuite extends munit.FunSuite:

  private def assertFloatMatrixEquals(actual: Matrix[Float], expected: Matrix[Float])(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(actual.rows, expected.rows, "rows mismatch")
    assertEquals(actual.cols, expected.cols, "cols mismatch")
    var i = 0
    while i < actual.rows do
      var j = 0
      while j < actual.cols do
        assertEqualsDouble(actual(i, j).toDouble, expected(i, j).toDouble, 1e-4, clue = s"mismatch at ($i, $j)")
        j += 1
      end while
      i += 1
    end while
  end assertFloatMatrixEquals

  test("sum reduction, genuinely non-contiguous (padded strides)"):
    // rows=2, cols=2, rowStride=1, colStride=3, offset=0 over a length-6 backing array: col 0 is raw(0),raw(1) =
    // [1,2], col 1 is raw(3),raw(4) = [3,4], and raw(2)/raw(5) are unused padding. dataLength (6) != numel (4),
    // so hasSimpleContiguousMemoryLayout is false - reduceAlongDimension used to bail out with `???` for exactly
    // this shape, even though its loop already reads every element through m.layout.linearIndex, which handles
    // arbitrary offsets/strides correctly regardless of contiguity.
    val mat = Matrix[Float](Array[Float](1.0f, 2.0f, 99.0f, 3.0f, 4.0f, 99.0f), 2, 2, 1, 3, 0)
    assert(!mat.hasSimpleContiguousMemoryLayout)

    assertFloatMatrixEquals(mat.sum(Rows), Matrix[Float](Array[Float](4.0f, 6.0f), (2, 1)))
    assertFloatMatrixEquals(mat.sum(Cols), Matrix[Float](Array[Float](3.0f, 7.0f), (1, 2)))

  test("max/min reduction, genuinely non-contiguous (padded strides)"):
    val mat = Matrix[Float](Array[Float](1.0f, 2.0f, 99.0f, 3.0f, 4.0f, 99.0f), 2, 2, 1, 3, 0)

    assertFloatMatrixEquals(mat.max(Rows), Matrix[Float](Array[Float](3.0f, 4.0f), (2, 1)))
    assertFloatMatrixEquals(mat.min(Cols), Matrix[Float](Array[Float](1.0f, 3.0f), (1, 2)))

end FloatMatrixReduceSuite
