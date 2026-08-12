package vecxt

import matrix.*
import all.*

/** Covers `JvmFloatMatrix`'s `extension (d: Float)` block — the `Float` counterpart of the `Double` scalar-left
  * operators exercised by `ScalarInPlaceMatrixSuite` in `test/src`. This one lives in `test/src-jvm` rather than
  * `test/src` because the block itself is JVM-only: `src-js-native/floatmatrix.scala` declares a stripped
  * `JvmFloatMatrix` carrying only the dimension reductions, so `d - m` and friends don't exist on JS or Native.
  *
  * Three layout families per operator, because that's where the branches are: dense column-major (the
  * `hasSimpleContiguousMemoryLayout` fast path, sized 9 so the SIMD loops in `vecxt.floatarrays` have a tail to get
  * wrong), dense row-major (also contiguous, but the result must keep row-major orientation rather than being
  * normalised), and genuinely non-contiguous padded strides (the `foreach2D` path, where padding must be left alone).
  */
class ScalarLeftFloatMatrixSuite extends munit.FunSuite:

  /** rows=2, cols=2, rowStride=1, colStride=3, offset=0 over a length-6 array — the same fixture shape as
    * `ScalarInPlaceMatrixSuite`. Column 0 is raw(0),raw(1) = [1,2]; column 1 is raw(3),raw(4) = [3,4]; raw(2) and
    * raw(5) are padding that no operator may touch. A `def`, not a `val`, so each in-place test gets a fresh one.
    */
  private def paddedMat = Matrix[Float](Array(1.0f, 2.0f, 99.0f, 3.0f, 4.0f, 99.0f), 2, 2, 1, 3, 0)

  /** Dense row-major 2x3: row 0 = [1,2,3], row 1 = [4,5,6]. Contiguous, so it takes the fast path — the point of it
    * is the layout of the result, not the arithmetic.
    */
  private def rowMajorMat = Matrix[Float](Array.tabulate(6)(i => (i + 1).toFloat), 2, 3, 3, 1, 0)

  private def denseMat = Matrix[Float](Array.tabulate(9)(i => (i + 1).toFloat), 3, 3)

  test("d - m and d / m, dense contiguous, size not a multiple of common SIMD widths"):
    assert(denseMat.hasSimpleContiguousMemoryLayout)

    assertMatrixEquals(10.0f - denseMat, Matrix[Float](Array.tabulate(9)(i => 10.0f - (i + 1).toFloat), 3, 3))
    assertMatrixEquals(10.0f / denseMat, Matrix[Float](Array.tabulate(9)(i => 10.0f / (i + 1).toFloat), 3, 3))

    // Delegating (commutative) forms, for completeness.
    assertMatrixEquals(10.0f + denseMat, Matrix[Float](Array.tabulate(9)(i => 10.0f + (i + 1).toFloat), 3, 3))
    assertMatrixEquals(10.0f * denseMat, Matrix[Float](Array.tabulate(9)(i => 10.0f * (i + 1).toFloat), 3, 3))

  test("d - m and d / m leave the operand untouched"):
    val m = denseMat
    val _ = 10.0f - m
    val _ = 10.0f / m
    assertMatrixEquals(m, denseMat)

  test("d - m and d / m keep a row-major operand row-major"):
    val m = rowMajorMat
    assert(m.hasSimpleContiguousMemoryLayout)

    val sub = 10.0f - m
    assert(sub.isDenseRowMajor, s"expected row-major result, got ${sub.layoutString}")
    assertMatrixEquals(sub, Matrix.fromRows[Float](Array(9.0f, 8.0f, 7.0f), Array(6.0f, 5.0f, 4.0f)))

    val div = 10.0f / m
    assert(div.isDenseRowMajor, s"expected row-major result, got ${div.layoutString}")
    assertMatrixEquals(div, Matrix.fromRows[Float](Array(10.0f, 5.0f, 10.0f / 3.0f), Array(2.5f, 2.0f, 10.0f / 6.0f)))

  test("d - m and d / m, genuinely non-contiguous (padded strides)"):
    val m = paddedMat
    assert(!m.hasSimpleContiguousMemoryLayout)

    // rowStride == 1, so the unit-stride axis is rows and the result materialises dense column-major.
    val sub = 10.0f - m
    assert(sub.isDenseColMajor, s"expected column-major result, got ${sub.layoutString}")
    assert(sub.hasSimpleContiguousMemoryLayout, "result should be dense, not inherit the padding")
    assertMatrixEquals(sub, Matrix.fromRows[Float](Array(9.0f, 7.0f), Array(8.0f, 6.0f)))

    val div = 10.0f / m
    assert(div.isDenseColMajor, s"expected column-major result, got ${div.layoutString}")
    assertMatrixEquals(div, Matrix.fromRows[Float](Array(10.0f, 10.0f / 3.0f), Array(5.0f, 2.5f)))

  test("d += m, d -= m, d *= m, d /= m, dense contiguous"):
    val mPlus = denseMat
    10.0f += mPlus
    assertMatrixEquals(mPlus, Matrix[Float](Array.tabulate(9)(i => 10.0f + (i + 1).toFloat), 3, 3))

    val mMinus = denseMat
    10.0f -= mMinus
    assertMatrixEquals(mMinus, Matrix[Float](Array.tabulate(9)(i => 10.0f - (i + 1).toFloat), 3, 3))

    val mTimes = denseMat
    10.0f *= mTimes
    assertMatrixEquals(mTimes, Matrix[Float](Array.tabulate(9)(i => 10.0f * (i + 1).toFloat), 3, 3))

    val mDiv = denseMat
    10.0f /= mDiv
    assertMatrixEquals(mDiv, Matrix[Float](Array.tabulate(9)(i => 10.0f / (i + 1).toFloat), 3, 3))

  test("d -= m and d /= m, dense row-major"):
    val mMinus = rowMajorMat
    10.0f -= mMinus
    assertMatrixEquals(mMinus, Matrix.fromRows[Float](Array(9.0f, 8.0f, 7.0f), Array(6.0f, 5.0f, 4.0f)))

    val mDiv = rowMajorMat
    10.0f /= mDiv
    assertMatrixEquals(mDiv, Matrix.fromRows[Float](Array(10.0f, 5.0f, 10.0f / 3.0f), Array(2.5f, 2.0f, 10.0f / 6.0f)))

  test("d += m, d -= m, d *= m, d /= m, genuinely non-contiguous (padded strides)"):
    val mPlus = paddedMat
    10.0f += mPlus
    assertMatrixEquals(mPlus, Matrix.fromRows[Float](Array(11.0f, 13.0f), Array(12.0f, 14.0f)))

    val mMinus = paddedMat
    10.0f -= mMinus
    assertMatrixEquals(mMinus, Matrix.fromRows[Float](Array(9.0f, 7.0f), Array(8.0f, 6.0f)))

    val mTimes = paddedMat
    10.0f *= mTimes
    assertMatrixEquals(mTimes, Matrix.fromRows[Float](Array(10.0f, 30.0f), Array(20.0f, 40.0f)))

    val mDiv = paddedMat
    10.0f /= mDiv
    assertMatrixEquals(mDiv, Matrix.fromRows[Float](Array(10.0f, 10.0f / 3.0f), Array(5.0f, 2.5f)))

  test("the in-place scalar-left operators never write to padding"):
    // Nothing above would catch this: every read goes through `linearIndex`, so a `while i < raw.length` loop that
    // ignored the layout would still pass the value assertions while silently clobbering raw(2) and raw(5).
    val mMinus = paddedMat
    10.0f -= mMinus
    assertEqualsFloat(mMinus.raw(2), 99.0f, 0.0f)
    assertEqualsFloat(mMinus.raw(5), 99.0f, 0.0f)

    val mDiv = paddedMat
    10.0f /= mDiv
    assertEqualsFloat(mDiv.raw(2), 99.0f, 0.0f)
    assertEqualsFloat(mDiv.raw(5), 99.0f, 0.0f)

    val mTimes = paddedMat
    10.0f *= mTimes
    assertEqualsFloat(mTimes.raw(2), 99.0f, 0.0f)
    assertEqualsFloat(mTimes.raw(5), 99.0f, 0.0f)

    val mPlus = paddedMat
    10.0f += mPlus
    assertEqualsFloat(mPlus.raw(2), 99.0f, 0.0f)
    assertEqualsFloat(mPlus.raw(5), 99.0f, 0.0f)

  test("d - m and d / m on an offset view read through the offset"):
    // rows=2, cols=2, rowStride=1, colStride=2, offset=2 over a length-6 array: the trailing 2x2 block of a
    // column-major 2x3 parent. Exercises `linearIndex`'s offset term, which the padded fixture leaves at 0.
    val parent = Array.tabulate(6)(i => (i + 1).toFloat)
    val view = Matrix[Float](parent, 2, 2, 1, 2, 2)
    assert(!view.hasSimpleContiguousMemoryLayout)
    assertMatrixEquals(view, Matrix.fromRows[Float](Array(3.0f, 5.0f), Array(4.0f, 6.0f)))

    assertMatrixEquals(10.0f - view, Matrix.fromRows[Float](Array(7.0f, 5.0f), Array(6.0f, 4.0f)))

    10.0f -= view
    assertMatrixEquals(view, Matrix.fromRows[Float](Array(7.0f, 5.0f), Array(6.0f, 4.0f)))
    // The two elements outside the view are untouched.
    assertEqualsFloat(parent(0), 1.0f, 0.0f)
    assertEqualsFloat(parent(1), 2.0f, 0.0f)

end ScalarLeftFloatMatrixSuite
