package vecxt

import all.*

/** `d - m` and `d / m` (scalar on the left) used to be `???` - unlike `d * m`/`d + m`, they can't just delegate to
  * `m * d`/`m + d` since subtraction and division aren't commutative. Covers the same three layout shapes as the
  * other scalar ops in doublematrix.scala: dense column-major (fast path), dense row-major (fast path, but exercises
  * a non-col-major `m.layout`), and genuinely non-contiguous / padded-stride (falls through to the elementwise loop).
  */
class ScalarMinusDivMatrixSuite extends munit.FunSuite:

  test("d - m and d / m, dense column-major"):
    val m = Matrix.fromRows[Double](Array(1.0, 2.0), Array(3.0, 4.0)) // [[1,2],[3,4]]

    assertMatrixEquals(10.0 - m, Matrix.fromRows[Double](Array(9.0, 8.0), Array(7.0, 6.0)))
    assertMatrixEquals(10.0 / m, Matrix.fromRows[Double](Array(10.0, 5.0), Array(10.0 / 3.0, 2.5)))

  test("d - m and d / m, dense row-major"):
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0), 2, 2, 2, 1, 0) // [[1,2],[3,4]], row-major
    assert(m.isDenseRowMajor)

    assertMatrixEquals(10.0 - m, Matrix.fromRows[Double](Array(9.0, 8.0), Array(7.0, 6.0)))
    assertMatrixEquals(10.0 / m, Matrix.fromRows[Double](Array(10.0, 5.0), Array(10.0 / 3.0, 2.5)))

  test("d - m and d / m, genuinely non-contiguous (padded strides)"):
    // Same fixture as ReduceAlongDimensionNonContiguous.test.scala: rows=2, cols=2, rowStride=1, colStride=3,
    // offset=0 over a length-6 array. Column 0 is raw(0),raw(1) = [1,2], column 1 is raw(3),raw(4) = [3,4], and
    // raw(2)/raw(5) are unused padding.
    val m = Matrix[Double](Array(1.0, 2.0, 99.0, 3.0, 4.0, 99.0), 2, 2, 1, 3, 0)
    assert(!m.hasSimpleContiguousMemoryLayout)

    assertMatrixEquals(10.0 - m, Matrix.fromRows[Double](Array(9.0, 7.0), Array(8.0, 6.0)))
    assertMatrixEquals(10.0 / m, Matrix.fromRows[Double](Array(10.0, 10.0 / 3.0), Array(5.0, 2.5)))

end ScalarMinusDivMatrixSuite
