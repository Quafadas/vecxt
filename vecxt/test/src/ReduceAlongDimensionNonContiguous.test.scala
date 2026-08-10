package vecxt

import all.*
import dimensionExtender.DimensionExtender.Dimension.*

/** `reduceAlongDimension` (backing `max`/`min`/`sum`/`product(dim)` for `Matrix[Double]`, `Matrix[Float]` and
  * `Matrix[Int]`, on every platform) used to bail out with `???` for any layout that wasn't
  * `hasSimpleContiguousMemoryLayout`. Its loop already reads every element through `m.layout.linearIndex` -
  * `offset + row * rowStride + col * colStride` - which is valid for any layout, contiguous or not, so the guard was
  * only ever blocking a path that already worked.
  *
  * All three element types share one non-contiguous fixture: rows=2, cols=2, rowStride=1, colStride=3, offset=0 over a
  * length-6 backing array. Column 0 is raw(0),raw(1) = [1,2], column 1 is raw(3),raw(4) = [3,4], and raw(2)/raw(5) are
  * unused padding - dataLength (6) != numel (4), so hasSimpleContiguousMemoryLayout is false and this genuinely
  * exercises the removed guard (a merely dense row-major layout would not: it already satisfies
  * hasSimpleContiguousMemoryLayout, guard or no guard).
  */
class ReduceAlongDimensionNonContiguousSuite extends munit.FunSuite:

  test("Double: sum/max/min/product reduction, genuinely non-contiguous (padded strides)"):
    val mat = Matrix[Double](Array(1.0, 2.0, 99.0, 3.0, 4.0, 99.0), 2, 2, 1, 3, 0)
    assert(!mat.hasSimpleContiguousMemoryLayout)

    assertMatrixEquals(mat.sum(Rows), Matrix[Double](Array(4.0, 6.0), (2, 1)))
    assertMatrixEquals(mat.sum(Cols), Matrix[Double](Array(3.0, 7.0), (1, 2)))
    assertMatrixEquals(mat.max(Rows), Matrix[Double](Array(3.0, 4.0), (2, 1)))
    assertMatrixEquals(mat.min(Cols), Matrix[Double](Array(1.0, 3.0), (1, 2)))
    assertMatrixEquals(mat.product(Rows), Matrix[Double](Array(3.0, 8.0), (2, 1)))

  test("Float: sum/max/min/product reduction, genuinely non-contiguous (padded strides)"):
    val mat = Matrix[Float](Array[Float](1.0f, 2.0f, 99.0f, 3.0f, 4.0f, 99.0f), 2, 2, 1, 3, 0)
    assert(!mat.hasSimpleContiguousMemoryLayout)

    assertMatrixEquals(mat.sum(Rows), Matrix[Float](Array[Float](4.0f, 6.0f), (2, 1)))
    assertMatrixEquals(mat.sum(Cols), Matrix[Float](Array[Float](3.0f, 7.0f), (1, 2)))
    assertMatrixEquals(mat.max(Rows), Matrix[Float](Array[Float](3.0f, 4.0f), (2, 1)))
    assertMatrixEquals(mat.min(Cols), Matrix[Float](Array[Float](1.0f, 3.0f), (1, 2)))
    assertMatrixEquals(mat.product(Rows), Matrix[Float](Array[Float](3.0f, 8.0f), (2, 1)))

  test("Int: sum/max/min/product reduction, genuinely non-contiguous (padded strides)"):
    val mat = Matrix[Int](Array(1, 2, 99, 3, 4, 99), 2, 2, 1, 3, 0)
    assert(!mat.hasSimpleContiguousMemoryLayout)

    assertMatrixEquals(mat.sum(Rows), Matrix[Int](Array(4, 6), (2, 1)))
    assertMatrixEquals(mat.sum(Cols), Matrix[Int](Array(3, 7), (1, 2)))
    assertMatrixEquals(mat.max(Rows), Matrix[Int](Array(3, 4), (2, 1)))
    assertMatrixEquals(mat.min(Cols), Matrix[Int](Array(1, 3), (1, 2)))
    assertMatrixEquals(mat.product(Rows), Matrix[Int](Array(3, 8), (2, 1)))

end ReduceAlongDimensionNonContiguousSuite
