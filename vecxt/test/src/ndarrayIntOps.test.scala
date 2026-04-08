package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArrayIntOpsSuite extends FunSuite:

  // ── Construction ──────────────────────────────────────────────────────────

  test("NDArray[Int] zeros and ones") {
    val z = NDArray.zeros[Int](Array(2, 3))
    assertEquals(z.toArray.toSeq, Seq(0, 0, 0, 0, 0, 0))
    val o = NDArray.ones[Int](Array(2, 3))
    assertEquals(o.toArray.toSeq, Seq(1, 1, 1, 1, 1, 1))
  }

  // ── Binary ops (col-major fast path) ──────────────────────────────────────

  test("NDArray[Int] + NDArray[Int] (col-major 1D)") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    val b = NDArray(Array(4, 5, 6), Array(3))
    val result = a + b
    assertEquals(result.toArray.toSeq, Seq(5, 7, 9))
    assert(result.isColMajor)
  }

  test("NDArray[Int] - NDArray[Int] (col-major 2D)") {
    val a = NDArray(Array(10, 20, 30, 40), Array(2, 2))
    val b = NDArray(Array(1, 2, 3, 4), Array(2, 2))
    assertEquals((a - b).toArray.toSeq, Seq(9, 18, 27, 36))
  }

  test("NDArray[Int] * NDArray[Int]") {
    val a = NDArray(Array(2, 3, 4), Array(3))
    val b = NDArray(Array(5, 6, 7), Array(3))
    assertEquals((a * b).toArray.toSeq, Seq(10, 18, 28))
  }

  test("NDArray[Int] / NDArray[Int] (integer division)") {
    val a = NDArray(Array(7, 10, 15), Array(3))
    val b = NDArray(Array(2, 3, 4), Array(3))
    assertEquals((a / b).toArray.toSeq, Seq(3, 3, 3))
  }

  test("NDArray[Int] % NDArray[Int]") {
    val a = NDArray(Array(7, 10, 15), Array(3))
    val b = NDArray(Array(2, 3, 4), Array(3))
    assertEquals((a % b).toArray.toSeq, Seq(1, 1, 3))
  }

  // ── Binary ops (general kernel — transposed views) ────────────────────────

  test("NDArray[Int] + NDArray[Int] via general kernel (transposed view)") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val at = a.T // shape [3,2], strides [2,1] — non-col-major
    val bt = a.T
    val result = at + bt
    val expected = at.toArray.map(_ * 2)
    assertEquals(result.toArray.toSeq, expected.toSeq)
    assert(result.isColMajor, "result of general kernel should be col-major")
  }

  // ── Scalar ops ────────────────────────────────────────────────────────────

  test("NDArray[Int] + scalar") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    assertEquals((a + 10).toArray.toSeq, Seq(11, 12, 13))
  }

  test("NDArray[Int] - scalar") {
    val a = NDArray(Array(10, 20, 30), Array(3))
    assertEquals((a - 5).toArray.toSeq, Seq(5, 15, 25))
  }

  test("NDArray[Int] * scalar") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    assertEquals((a * 3).toArray.toSeq, Seq(3, 6, 9))
  }

  test("NDArray[Int] / scalar") {
    val a = NDArray(Array(6, 9, 12), Array(3))
    assertEquals((a / 3).toArray.toSeq, Seq(2, 3, 4))
  }

  test("NDArray[Int] % scalar") {
    val a = NDArray(Array(7, 10, 15), Array(3))
    assertEquals((a % 4).toArray.toSeq, Seq(3, 2, 3))
  }

  // ── Left-scalar ops ───────────────────────────────────────────────────────

  test("scalar + NDArray[Int]") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    assertEquals((10 + a).toArray.toSeq, Seq(11, 12, 13))
  }

  test("scalar - NDArray[Int]") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    assertEquals((10 - a).toArray.toSeq, Seq(9, 8, 7))
  }

  test("scalar * NDArray[Int]") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    assertEquals((3 * a).toArray.toSeq, Seq(3, 6, 9))
  }

  test("scalar / NDArray[Int]") {
    val a = NDArray(Array(1, 2, 4), Array(3))
    assertEquals((12 / a).toArray.toSeq, Seq(12, 6, 3))
  }

  // ── Unary ops ─────────────────────────────────────────────────────────────

  test("NDArray[Int] neg") {
    val a = NDArray(Array(1, -2, 3), Array(3))
    assertEquals(a.neg.toArray.toSeq, Seq(-1, 2, -3))
  }

  test("NDArray[Int] abs") {
    val a = NDArray(Array(-3, 0, 4), Array(3))
    assertEquals(a.abs.toArray.toSeq, Seq(3, 0, 4))
  }

  // ── Comparison ops ────────────────────────────────────────────────────────

  test("NDArray[Int] > NDArray[Int]") {
    val a = NDArray(Array(1, 5, 3), Array(3))
    val b = NDArray(Array(2, 4, 3), Array(3))
    assertEquals((a > b).toArray.toSeq, Seq(false, true, false))
  }

  test("NDArray[Int] < NDArray[Int]") {
    val a = NDArray(Array(1, 5, 3), Array(3))
    val b = NDArray(Array(2, 4, 3), Array(3))
    assertEquals((a < b).toArray.toSeq, Seq(true, false, false))
  }

  test("NDArray[Int] =:= scalar") {
    val a = NDArray(Array(1, 2, 1, 3), Array(4))
    assertEquals((a =:= 1).toArray.toSeq, Seq(true, false, true, false))
  }

  test("NDArray[Int] !:= scalar") {
    val a = NDArray(Array(1, 2, 1, 3), Array(4))
    assertEquals((a !:= 1).toArray.toSeq, Seq(false, true, false, true))
  }

  test("NDArray[Int] >= NDArray[Int]") {
    val a = NDArray(Array(1, 5, 3), Array(3))
    val b = NDArray(Array(2, 4, 3), Array(3))
    assertEquals((a >= b).toArray.toSeq, Seq(false, true, true))
  }

  test("NDArray[Int] <= NDArray[Int]") {
    val a = NDArray(Array(1, 5, 3), Array(3))
    val b = NDArray(Array(2, 4, 3), Array(3))
    assertEquals((a <= b).toArray.toSeq, Seq(true, false, true))
  }

  test("NDArray[Int] =:= NDArray[Int]") {
    val a = NDArray(Array(1, 5, 3), Array(3))
    val b = NDArray(Array(1, 4, 3), Array(3))
    assertEquals((a =:= b).toArray.toSeq, Seq(true, false, true))
  }

  test("NDArray[Int] > scalar") {
    val a = NDArray(Array(1, 5, 3), Array(3))
    assertEquals((a > 2).toArray.toSeq, Seq(false, true, true))
  }

  test("NDArray[Int] < scalar") {
    val a = NDArray(Array(1, 5, 3), Array(3))
    assertEquals((a < 4).toArray.toSeq, Seq(true, false, true))
  }

  // ── In-place ops ──────────────────────────────────────────────────────────

  test("NDArray[Int] += NDArray[Int]") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    val b = NDArray(Array(10, 20, 30), Array(3))
    a += b
    assertEquals(a.toArray.toSeq, Seq(11, 22, 33))
  }

  test("NDArray[Int] -= NDArray[Int]") {
    val a = NDArray(Array(10, 20, 30), Array(3))
    val b = NDArray(Array(1, 2, 3), Array(3))
    a -= b
    assertEquals(a.toArray.toSeq, Seq(9, 18, 27))
  }

  test("NDArray[Int] *= NDArray[Int]") {
    val a = NDArray(Array(2, 3, 4), Array(3))
    val b = NDArray(Array(3, 4, 5), Array(3))
    a *= b
    assertEquals(a.toArray.toSeq, Seq(6, 12, 20))
  }

  test("NDArray[Int] += scalar") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    a += 5
    assertEquals(a.toArray.toSeq, Seq(6, 7, 8))
  }

  test("NDArray[Int] -= scalar") {
    val a = NDArray(Array(10, 20, 30), Array(3))
    a -= 5
    assertEquals(a.toArray.toSeq, Seq(5, 15, 25))
  }

  test("NDArray[Int] *= scalar") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    a *= 3
    assertEquals(a.toArray.toSeq, Seq(3, 6, 9))
  }

  test("NDArray[Int] in-place on non-contiguous throws") {
    val a = NDArray(Array(1, 2, 3, 4), Array(2, 2))
    val at = a.T // non-contiguous view
    val b = NDArray(Array(1, 1, 1, 1), Array(2, 2))
    intercept[InvalidNDArray] { at += b.T }
  }

  // ── Shape mismatch ────────────────────────────────────────────────────────

  test("NDArray[Int] + shape mismatch throws") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    val b = NDArray(Array(1, 2), Array(2))
    intercept[ShapeMismatchException] { a + b }
  }

  test("NDArray[Int] > shape mismatch throws") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    val b = NDArray(Array(1, 2), Array(2))
    intercept[ShapeMismatchException] { a > b }
  }

end NDArrayIntOpsSuite
