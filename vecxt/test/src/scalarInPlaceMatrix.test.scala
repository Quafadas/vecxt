package vecxt

import all.*

/** `*=` already had a real implementation to mirror
  * for `-=`/`/=`, and each platform already had its own more specialised `+=(n: Double)`, so only `d += m` needed
  * wiring up to it - not a new `+=` implementation. Covers dense contiguous (including a size that isn't a multiple of
  * any common SIMD width, to catch tail-loop bugs like the one fixed in `Array[Double]#-=(d: Double)` alongside this)
  * and genuinely non-contiguous (padded-stride) layouts, plus the `Double`-first spellings.
  */
class ScalarInPlaceMatrixSuite extends munit.FunSuite:

  test("m += d, m -= d, m /= d, dense contiguous, size not a multiple of common SIMD widths"):
    val mPlus = Matrix[Double](Array.tabulate(9)(_.toDouble + 1), 3, 3) // 1..9
    mPlus += 2.0
    assertMatrixEquals(mPlus, Matrix[Double](Array.tabulate(9)(i => (i + 1).toDouble + 2.0), 3, 3))

    val mMinus = Matrix[Double](Array.tabulate(9)(_.toDouble + 1), 3, 3)
    mMinus -= 2.0
    assertMatrixEquals(mMinus, Matrix[Double](Array.tabulate(9)(i => (i + 1).toDouble - 2.0), 3, 3))

    val mDiv = Matrix[Double](Array.tabulate(9)(_.toDouble + 1), 3, 3)
    mDiv /= 2.0
    assertMatrixEquals(mDiv, Matrix[Double](Array.tabulate(9)(i => (i + 1).toDouble / 2.0), 3, 3))

  test("m += d, m -= d, m /= d, genuinely non-contiguous (padded strides)"):
    // Same fixture as ReduceAlongDimensionNonContiguous.test.scala: rows=2, cols=2, rowStride=1, colStride=3,
    // offset=0 over a length-6 array. Column 0 is raw(0),raw(1) = [1,2], column 1 is raw(3),raw(4) = [3,4], and
    // raw(2)/raw(5) are unused padding.
    def paddedMat = Matrix[Double](Array(1.0, 2.0, 99.0, 3.0, 4.0, 99.0), 2, 2, 1, 3, 0)
    assert(!paddedMat.hasSimpleContiguousMemoryLayout)

    val mPlus = paddedMat
    mPlus += 2.0
    assertMatrixEquals(mPlus, Matrix.fromRows[Double](Array(3.0, 5.0), Array(4.0, 6.0)))

    val mMinus = paddedMat
    mMinus -= 2.0
    assertMatrixEquals(mMinus, Matrix.fromRows[Double](Array(-1.0, 1.0), Array(0.0, 2.0)))

    val mDiv = paddedMat
    mDiv /= 2.0
    assertMatrixEquals(mDiv, Matrix.fromRows[Double](Array(0.5, 1.5), Array(1.0, 2.0)))

  test("d += m, d -= m, d /= m mutate m in place with scalar-left semantics"):
    val mPlus = Matrix.fromRows[Double](Array(1.0, 2.0), Array(3.0, 4.0))
    10.0 += mPlus
    assertMatrixEquals(mPlus, Matrix.fromRows[Double](Array(11.0, 12.0), Array(13.0, 14.0)))

    val mMinus = Matrix.fromRows[Double](Array(1.0, 2.0), Array(3.0, 4.0))
    10.0 -= mMinus
    assertMatrixEquals(mMinus, Matrix.fromRows[Double](Array(9.0, 8.0), Array(7.0, 6.0)))

    val mDiv = Matrix.fromRows[Double](Array(1.0, 2.0), Array(4.0, 5.0))
    10.0 /= mDiv
    assertMatrixEquals(mDiv, Matrix.fromRows[Double](Array(10.0, 5.0), Array(2.5, 2.0)))

end ScalarInPlaceMatrixSuite
