package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArrayFloatOpsSuite extends FunSuite:

  // ── Binary ops ────────────────────────────────────────────────────────────

  test("NDArray[Float] + NDArray[Float] (col-major)") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    val b = NDArray(Array(4.0f, 5.0f, 6.0f), Array(3))
    val result = a + b
    assertEquals(result.toArray.toSeq, Seq(5.0f, 7.0f, 9.0f))
    assert(result.isColMajor)
  }

  test("NDArray[Float] - NDArray[Float]") {
    val a = NDArray(Array(10.0f, 20.0f, 30.0f), Array(3))
    val b = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    assertEquals((a - b).toArray.toSeq, Seq(9.0f, 18.0f, 27.0f))
  }

  test("NDArray[Float] * NDArray[Float]") {
    val a = NDArray(Array(2.0f, 3.0f, 4.0f), Array(3))
    val b = NDArray(Array(5.0f, 6.0f, 7.0f), Array(3))
    assertEquals((a * b).toArray.toSeq, Seq(10.0f, 18.0f, 28.0f))
  }

  test("NDArray[Float] / NDArray[Float]") {
    val a = NDArray(Array(6.0f, 9.0f, 12.0f), Array(3))
    val b = NDArray(Array(2.0f, 3.0f, 4.0f), Array(3))
    assertEquals((a / b).toArray.toSeq, Seq(3.0f, 3.0f, 3.0f))
  }

  test("NDArray[Float] + NDArray[Float] via general kernel (transposed view)") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), Array(2, 3))
    val at = a.T
    val bt = a.T
    val result = at + bt
    val expected = at.toArray.map(_ * 2)
    assertEquals(result.toArray.toSeq, expected.toSeq)
    assert(result.isRowMajor)
  }

  // ── Scalar ops ────────────────────────────────────────────────────────────

  test("NDArray[Float] + scalar") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    assertEquals((a + 10.0f).toArray.toSeq, Seq(11.0f, 12.0f, 13.0f))
  }

  test("NDArray[Float] * scalar") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    assertEquals((a * 3.0f).toArray.toSeq, Seq(3.0f, 6.0f, 9.0f))
  }

  test("NDArray[Float] / scalar") {
    val a = NDArray(Array(6.0f, 9.0f, 12.0f), Array(3))
    assertEquals((a / 3.0f).toArray.toSeq, Seq(2.0f, 3.0f, 4.0f))
  }

  // ── Left-scalar ops ───────────────────────────────────────────────────────

  test("scalar + NDArray[Float]") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    assertEquals((10.0f + a).toArray.toSeq, Seq(11.0f, 12.0f, 13.0f))
  }

  test("scalar - NDArray[Float]") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    assertEquals((10.0f - a).toArray.toSeq, Seq(9.0f, 8.0f, 7.0f))
  }

  test("scalar * NDArray[Float]") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    assertEquals((3.0f * a).toArray.toSeq, Seq(3.0f, 6.0f, 9.0f))
  }

  // ── Unary ops ─────────────────────────────────────────────────────────────

  test("NDArray[Float] neg") {
    val a = NDArray(Array(1.0f, -2.0f, 3.0f), Array(3))
    assertEquals(a.neg.toArray.toSeq, Seq(-1.0f, 2.0f, -3.0f))
  }

  test("NDArray[Float] abs") {
    val a = NDArray(Array(-3.0f, 0.0f, 4.0f), Array(3))
    assertEquals(a.abs.toArray.toSeq, Seq(3.0f, 0.0f, 4.0f))
  }

  test("NDArray[Float] exp") {
    val a = NDArray(Array(0.0f, 1.0f), Array(2))
    val result = a.exp
    assertEqualsDouble(result.toArray(0).toDouble, 1.0, 1e-6)
    assertEqualsDouble(result.toArray(1).toDouble, Math.E, 1e-6)
  }

  test("NDArray[Float] log") {
    val a = NDArray(Array(1.0f, Math.E.toFloat), Array(2))
    val result = a.log
    assertEqualsDouble(result.toArray(0).toDouble, 0.0, 1e-6)
    assertEqualsDouble(result.toArray(1).toDouble, 1.0, 1e-6)
  }

  test("NDArray[Float] sqrt") {
    val a = NDArray(Array(4.0f, 9.0f, 16.0f), Array(3))
    val result = a.sqrt
    assertEquals(result.toArray.toSeq, Seq(2.0f, 3.0f, 4.0f))
  }

  test("NDArray[Float] sigmoid at 0 = 0.5") {
    val a = NDArray(Array(0.0f), Array(1))
    assertEqualsDouble(a.sigmoid.toArray(0).toDouble, 0.5, 1e-6)
  }

  // ── Comparison ops ────────────────────────────────────────────────────────

  test("NDArray[Float] > NDArray[Float]") {
    val a = NDArray(Array(1.0f, 5.0f, 3.0f), Array(3))
    val b = NDArray(Array(2.0f, 4.0f, 3.0f), Array(3))
    assertEquals((a > b).toArray.toSeq, Seq(false, true, false))
  }

  test("NDArray[Float] =:= scalar") {
    val a = NDArray(Array(1.0f, 2.0f, 1.0f, 3.0f), Array(4))
    assertEquals((a =:= 1.0f).toArray.toSeq, Seq(true, false, true, false))
  }

  test("NDArray[Float] >= NDArray[Float]") {
    val a = NDArray(Array(1.0f, 5.0f, 3.0f), Array(3))
    val b = NDArray(Array(2.0f, 4.0f, 3.0f), Array(3))
    assertEquals((a >= b).toArray.toSeq, Seq(false, true, true))
  }

  // ── In-place ops ──────────────────────────────────────────────────────────

  test("NDArray[Float] += NDArray[Float]") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    val b = NDArray(Array(10.0f, 20.0f, 30.0f), Array(3))
    a += b
    assertEquals(a.toArray.toSeq, Seq(11.0f, 22.0f, 33.0f))
  }

  test("NDArray[Float] += scalar") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    a += 5.0f
    assertEquals(a.toArray.toSeq, Seq(6.0f, 7.0f, 8.0f))
  }

  // ── Shape mismatch ────────────────────────────────────────────────────────

  test("NDArray[Float] + shape mismatch throws") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    val b = NDArray(Array(1.0f, 2.0f), Array(2))
    intercept[ShapeMismatchException](a + b)
  }

end NDArrayFloatOpsSuite
