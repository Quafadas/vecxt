package vecxt

import munit.FunSuite

import all.*

/** Cross-platform coverage for the `Matrix[Float]` scalar/comparison/broadcast operators that used to live only in
  * `JvmFloatMatrix` (`vecxt/src-jvm/floatmatrix.scala`) and are now also implemented on JS/Native via the shared
  * `vecxt/src-js-native/floatmatrix.scala`. These mirror (a simplified subset of) the JVM-only assertions in
  * `vecxt/test/src-jvm/floatmatrix.test.scala` and `vecxt/test/src-jvm/scalarLeftFloatMatrix.test.scala`, but live
  * here so they run on all three platforms.
  */
class FloatMatrixParitySuite extends FunSuite:

  private def denseMat = Matrix[Float](Array.tabulate(6)(i => (i + 1).toFloat), 2, 3)

  test("comparison operators on a dense Float matrix"):
    val m = denseMat
    assertVecEquals[Boolean](m.>=(3.0f).raw, Array(false, false, true, true, true, true))
    assertVecEquals[Boolean](m.>(3.0f).raw, Array(false, false, false, true, true, true))
    assertVecEquals[Boolean](m.<=(3.0f).raw, Array(true, true, true, false, false, false))
    assertVecEquals[Boolean](m.<(3.0f).raw, Array(true, true, false, false, false, false))

  test("*:* and *:*= mask out elements where the Boolean matrix is false"):
    val m = denseMat
    val mask = Matrix[Boolean](Array(true, false, true, false, true, false), 2, 3)

    val masked = m *:* mask
    assertVecEquals[Float](masked.raw, Array(1.0f, 0.0f, 3.0f, 0.0f, 5.0f, 0.0f))
    // *:* must not mutate its operand.
    assertVecEquals[Float](m.raw, Array.tabulate(6)(i => (i + 1).toFloat))

    val inPlace = m.deepCopy
    inPlace *:*= mask
    assertVecEquals[Float](inPlace.raw, Array(1.0f, 0.0f, 3.0f, 0.0f, 5.0f, 0.0f))

  test("+= array broadcasts down each column"):
    val m = denseMat
    m += Array(10.0f, 20.0f, 30.0f)
    assertVecEquals[Float](m.raw, Array(11.0f, 12.0f, 23.0f, 24.0f, 35.0f, 36.0f))

  test("-= array broadcasts down each column"):
    val m = denseMat
    m -= Array(1.0f, 2.0f, 3.0f)
    assertVecEquals[Float](m.raw, Array(0.0f, 1.0f, 1.0f, 2.0f, 2.0f, 3.0f))

  test("- elementwise matrix subtraction"):
    val m1 = denseMat
    val m2 = Matrix[Float](Array.fill(6)(1.0f), 2, 3)
    val result = m1 - m2
    assertVecEquals[Float](result.raw, Array(0.0f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f))
    // Must not mutate either operand.
    assertVecEquals[Float](m1.raw, Array.tabulate(6)(i => (i + 1).toFloat))

  test("+= and -= scalar mutate in place"):
    val mPlus = denseMat
    mPlus += 10.0f
    assertVecEquals[Float](mPlus.raw, Array.tabulate(6)(i => (i + 1).toFloat + 10.0f))

    val mMinus = denseMat
    mMinus -= 1.0f
    assertVecEquals[Float](mMinus.raw, Array.tabulate(6)(i => (i + 1).toFloat - 1.0f))

  test("*= scalar mutates in place, * and + scalar return fresh matrices"):
    val m = denseMat
    val timesResult = m * 2.0f
    assertVecEquals[Float](timesResult.raw, Array.tabulate(6)(i => (i + 1).toFloat * 2.0f))
    assertVecEquals[Float](m.raw, Array.tabulate(6)(i => (i + 1).toFloat))

    val plusResult = m + 1.0f
    assertVecEquals[Float](plusResult.raw, Array.tabulate(6)(i => (i + 1).toFloat + 1.0f))
    assertVecEquals[Float](m.raw, Array.tabulate(6)(i => (i + 1).toFloat))

    val mMut = denseMat
    mMut *= 3.0f
    assertVecEquals[Float](mMut.raw, Array.tabulate(6)(i => (i + 1).toFloat * 3.0f))

  test("scalar-left operators: d * m, d + m, d - m, d / m"):
    val m = denseMat
    assertVecEquals[Float]((2.0f * m).raw, Array.tabulate(6)(i => 2.0f * (i + 1).toFloat))
    assertVecEquals[Float]((2.0f + m).raw, Array.tabulate(6)(i => 2.0f + (i + 1).toFloat))
    assertVecEquals[Float]((10.0f - m).raw, Array.tabulate(6)(i => 10.0f - (i + 1).toFloat))
    assertVecEquals[Float]((10.0f / m).raw, Array.tabulate(6)(i => 10.0f / (i + 1).toFloat))

  test("scalar-left in-place operators: d *= m, d += m, d -= m, d /= m"):
    val mTimes = denseMat
    2.0f *= mTimes
    assertVecEquals[Float](mTimes.raw, Array.tabulate(6)(i => 2.0f * (i + 1).toFloat))

    val mPlus = denseMat
    2.0f += mPlus
    assertVecEquals[Float](mPlus.raw, Array.tabulate(6)(i => 2.0f + (i + 1).toFloat))

    val mMinus = denseMat
    10.0f -= mMinus
    assertVecEquals[Float](mMinus.raw, Array.tabulate(6)(i => 10.0f - (i + 1).toFloat))

    val mDiv = denseMat
    10.0f /= mDiv
    assertVecEquals[Float](mDiv.raw, Array.tabulate(6)(i => 10.0f / (i + 1).toFloat))

end FloatMatrixParitySuite
