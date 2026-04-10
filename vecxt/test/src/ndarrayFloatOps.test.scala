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

  test("NDArray[Float] + NDArray[Float] general kernel (non-zero offset)") {
    // slice from index 2: offset=2, shape=[4], strides=[1] → not isColMajor
    val raw = Array(0.0f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val a = NDArray(raw, Array(6))
    val view = a.slice(0, 2, 6) // elements [2,3,4,5], offset=2
    assert(!view.isColMajor, "slice view must not be col-major")
    assert(view.offset == 2)
    val b = NDArray(Array(10.0f, 20.0f, 30.0f, 40.0f), Array(4))
    assertEquals((view + b).toArray.toSeq, Seq(12.0f, 23.0f, 34.0f, 45.0f))
  }

  test("NDArray[Float] unary op on row-major input preserves layout") {
    // raw col-major [2,2]: (0,0)=1,(1,0)=4,(0,1)=9,(1,1)=16 → after .T logical matrix [[1,9],[4,16]]
    val a = NDArray(Array(1.0f, 4.0f, 9.0f, 16.0f), Array(2, 2)).T // row-major
    val result = a.sqrt
    assert(result.isRowMajor, "result should be row-major when input is row-major")
    // toArray returns logical row-major order: (0,0)=1,(0,1)=3,(1,0)=2,(1,1)=4
    assertEquals(result.toArray.toSeq, a.toArray.map(x => math.sqrt(x.toDouble).toFloat).toSeq)
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

  test("NDArray[Float] tanh") {
    val a = NDArray(Array(0.0f, 1.0f, -1.0f), Array(3))
    val result = a.tanh
    assertEqualsDouble(result.toArray(0).toDouble, 0.0, 1e-6)
    assertEqualsDouble(result.toArray(1).toDouble, Math.tanh(1.0), 1e-6)
    assertEqualsDouble(result.toArray(2).toDouble, Math.tanh(-1.0), 1e-6)
  }

  test("NDArray[Float] sin") {
    val a = NDArray(Array(0.0f, (Math.PI / 2).toFloat, Math.PI.toFloat), Array(3))
    val result = a.sin
    assertEqualsDouble(result.toArray(0).toDouble, 0.0, 1e-6)
    assertEqualsDouble(result.toArray(1).toDouble, 1.0, 1e-6)
    assertEqualsDouble(result.toArray(2).toDouble, 0.0, 1e-5)
  }

  test("NDArray[Float] cos") {
    val a = NDArray(Array(0.0f, (Math.PI / 2).toFloat, Math.PI.toFloat), Array(3))
    val result = a.cos
    assertEqualsDouble(result.toArray(0).toDouble, 1.0, 1e-6)
    assertEqualsDouble(result.toArray(1).toDouble, 0.0, 1e-5)
    assertEqualsDouble(result.toArray(2).toDouble, -1.0, 1e-6)
  }

  test("NDArray[Float] atan") {
    val a = NDArray(Array(0.0f, 1.0f, -1.0f), Array(3))
    val result = a.atan
    assertEqualsDouble(result.toArray(0).toDouble, 0.0, 1e-6)
    assertEqualsDouble(result.toArray(1).toDouble, Math.PI / 4, 1e-6)
    assertEqualsDouble(result.toArray(2).toDouble, -Math.PI / 4, 1e-6)
  }

  test("NDArray[Float] sin on transposed (general kernel)") {
    val a = NDArray(Array(0.0f, (Math.PI / 2).toFloat, Math.PI.toFloat, 0.0f), Array(2, 2)).T
    val result = a.sin
    val expected = a.toArray.map(x => Math.sin(x.toDouble).toFloat)
    result.toArray.zip(expected).foreach { (got, exp) =>
      assertEqualsDouble(got.toDouble, exp.toDouble, 1e-5)
    }
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
