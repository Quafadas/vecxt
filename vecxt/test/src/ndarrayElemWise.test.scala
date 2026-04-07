package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArrayElemWiseSuite extends FunSuite:

  private val eps = 1e-10

  private def assertClose(actual: Double, expected: Double, clue: String = ""): Unit =
    assert(Math.abs(actual - expected) < eps, s"$clue: expected $expected got $actual")

  private def assertNDArrayClose(actual: NDArray[Double], expected: Array[Double]): Unit =
    val arr = actual.toArray
    assertEquals(arr.length, expected.length, "length mismatch")
    for i <- expected.indices do assertClose(arr(i), expected(i), s"element $i")

  // ── Binary ops (col-major fast path) ─────────────────────────────────────

  test("NDArray + NDArray (col-major 1D)") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array(4.0, 5.0, 6.0), Array(3))
    assertNDArrayClose(a + b, Array(5.0, 7.0, 9.0))
  }

  test("NDArray - NDArray (col-major 2D)") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val b = NDArray(Array(0.5, 1.0, 1.5, 2.0), Array(2, 2))
    assertNDArrayClose(a - b, Array(0.5, 1.0, 1.5, 2.0))
  }

  test("NDArray * NDArray (col-major 2D)") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val b = NDArray(Array(2.0, 3.0, 4.0, 5.0), Array(2, 2))
    assertNDArrayClose(a * b, Array(2.0, 6.0, 12.0, 20.0))
  }

  test("NDArray / NDArray (col-major 1D)") {
    val a = NDArray(Array(6.0, 9.0, 12.0), Array(3))
    val b = NDArray(Array(2.0, 3.0, 4.0), Array(3))
    assertNDArrayClose(a / b, Array(3.0, 3.0, 3.0))
  }

  // ── Binary ops (general kernel for non-col-major) ─────────────────────────

  test("NDArray + NDArray via general kernel (transposed view)") {
    // col-major 2×3, then transpose → row-major-like 3×2 view
    val raw = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val a = NDArray(raw, Array(2, 3)) // col-major shape [2,3] strides [1,2]
    val b = NDArray(raw.clone(), Array(2, 3))
    val at = a.T // shape [3,2] strides [2,1] (non-col-major)
    val bt = b.T
    val result = at + bt
    // Each element should be 2x the transposed value
    // at(0,0)=a(0,0)=1, at(1,0)=a(0,1)=3, etc.
    val expected = at.toArray.map(_ * 2.0)
    assertNDArrayClose(result, expected)
  }

  test("result of binary op on non-col-major inputs is col-major") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val at = a.T // non-col-major view
    val bt = a.T
    val result = at + bt
    assert(result.isColMajor, "result should be col-major")
  }

  // ── Scalar ops ─────────────────────────────────────────────────────────────

  test("NDArray + scalar") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertNDArrayClose(a + 10.0, Array(11.0, 12.0, 13.0))
  }

  test("NDArray - scalar") {
    val a = NDArray(Array(5.0, 6.0, 7.0), Array(3))
    assertNDArrayClose(a - 2.0, Array(3.0, 4.0, 5.0))
  }

  test("NDArray * scalar") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertNDArrayClose(a * 3.0, Array(3.0, 6.0, 9.0))
  }

  test("NDArray / scalar") {
    val a = NDArray(Array(6.0, 9.0, 12.0), Array(3))
    assertNDArrayClose(a / 3.0, Array(2.0, 3.0, 4.0))
  }

  // ── Left-scalar ops ────────────────────────────────────────────────────────

  test("scalar + NDArray") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertNDArrayClose(10.0 + a, Array(11.0, 12.0, 13.0))
  }

  test("scalar - NDArray") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertNDArrayClose(10.0 - a, Array(9.0, 8.0, 7.0))
  }

  test("scalar * NDArray") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertNDArrayClose(2.0 * a, Array(2.0, 4.0, 6.0))
  }

  test("scalar / NDArray") {
    val a = NDArray(Array(1.0, 2.0, 4.0), Array(3))
    assertNDArrayClose(8.0 / a, Array(8.0, 4.0, 2.0))
  }

  // ── Unary ops ──────────────────────────────────────────────────────────────

  test("neg") {
    val a = NDArray(Array(1.0, -2.0, 3.0), Array(3))
    assertNDArrayClose(a.neg, Array(-1.0, 2.0, -3.0))
  }

  test("abs") {
    val a = NDArray(Array(-3.0, 0.0, 4.0), Array(3))
    assertNDArrayClose(a.abs, Array(3.0, 0.0, 4.0))
  }

  test("exp") {
    val a = NDArray(Array(0.0, 1.0), Array(2))
    assertNDArrayClose(a.exp, Array(1.0, Math.E))
  }

  test("log") {
    val a = NDArray(Array(1.0, Math.E), Array(2))
    assertNDArrayClose(a.log, Array(0.0, 1.0))
  }

  test("sqrt") {
    val a = NDArray(Array(4.0, 9.0, 16.0), Array(3))
    assertNDArrayClose(a.sqrt, Array(2.0, 3.0, 4.0))
  }

  test("tanh") {
    val a = NDArray(Array(0.0), Array(1))
    assertNDArrayClose(a.tanh, Array(0.0))
  }

  test("sigmoid at 0 = 0.5") {
    val a = NDArray(Array(0.0), Array(1))
    assertNDArrayClose(a.sigmoid, Array(0.5))
  }

  test("unary ops on non-col-major (transposed)") {
    val raw = Array(1.0, 4.0, 9.0, 16.0)
    val a = NDArray(raw, Array(2, 2)).T // transposed view: shape[2,2] strides[2,1]
    val result = a.sqrt
    assert(result.isColMajor, "result must be col-major")
    // Transposed elements: (0,0)=1.0, (1,0)=9.0, (0,1)=4.0, (1,1)=16.0
    // sqrt in col-major output order: (0,0)=1, (1,0)=3, (0,1)=2, (1,1)=4
    assertNDArrayClose(result, Array(1.0, 3.0, 2.0, 4.0))
  }

  // ── In-place binary ops ────────────────────────────────────────────────────

  test("in-place += array (col-major fast path)") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array(4.0, 5.0, 6.0), Array(3))
    a += b
    assertNDArrayClose(a, Array(5.0, 7.0, 9.0))
  }

  test("in-place -= array") {
    val a = NDArray(Array(5.0, 7.0, 9.0), Array(3))
    val b = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    a -= b
    assertNDArrayClose(a, Array(4.0, 5.0, 6.0))
  }

  test("in-place *= array") {
    val a = NDArray(Array(2.0, 3.0, 4.0), Array(3))
    val b = NDArray(Array(10.0, 10.0, 10.0), Array(3))
    a *= b
    assertNDArrayClose(a, Array(20.0, 30.0, 40.0))
  }

  test("in-place /= array") {
    val a = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    val b = NDArray(Array(2.0, 4.0, 5.0), Array(3))
    a /= b
    assertNDArrayClose(a, Array(5.0, 5.0, 6.0))
  }

  test("in-place += array (general kernel: contiguous a, non-col-major b)") {
    // a is col-major, b is a broadcast view
    val a = NDArray.fill(Array(2, 3), 1.0)
    val bias = NDArray(Array(10.0, 20.0, 30.0), Array(1, 3)) // shape [1,3]
    a += bias.broadcastTo(Array(2, 3))
    // col-major order: (0,0)=1+10=11, (1,0)=1+10=11, (0,1)=1+20=21, (1,1)=1+20=21, (0,2)=1+30=31, (1,2)=1+30=31
    assertNDArrayClose(a, Array(11.0, 11.0, 21.0, 21.0, 31.0, 31.0))
  }

  // ── In-place scalar ops ────────────────────────────────────────────────────

  test("in-place += scalar") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    a += 10.0
    assertNDArrayClose(a, Array(11.0, 12.0, 13.0))
  }

  test("in-place -= scalar") {
    val a = NDArray(Array(5.0, 6.0, 7.0), Array(3))
    a -= 2.0
    assertNDArrayClose(a, Array(3.0, 4.0, 5.0))
  }

  test("in-place *= scalar") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    a *= 3.0
    assertNDArrayClose(a, Array(3.0, 6.0, 9.0))
  }

  test("in-place /= scalar") {
    val a = NDArray(Array(6.0, 9.0, 12.0), Array(3))
    a /= 3.0
    assertNDArrayClose(a, Array(2.0, 3.0, 4.0))
  }

  test("in-place op throws on non-contiguous array") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val view = a.slice(0, 0, 1) // strided view, not contiguous
    intercept[UnsupportedOperationException] {
      view += NDArray(Array(1.0, 2.0), Array(1, 2))
    }
  }

  // ── Comparison ops ─────────────────────────────────────────────────────────

  test("comparison > scalar") {
    val a = NDArray(Array(1.0, 5.0, 3.0, 7.0), Array(4))
    val result = a > 4.0
    assertEquals(result.toArray.toSeq, Seq(false, true, false, true))
  }

  test("comparison < scalar") {
    val a = NDArray(Array(1.0, 5.0, 3.0, 7.0), Array(4))
    val result = a < 4.0
    assertEquals(result.toArray.toSeq, Seq(true, false, true, false))
  }

  test("comparison >= scalar") {
    val a = NDArray(Array(1.0, 4.0, 3.0, 7.0), Array(4))
    assertEquals((a >= 4.0).toArray.toSeq, Seq(false, true, false, true))
  }

  test("comparison <= scalar") {
    val a = NDArray(Array(1.0, 4.0, 5.0, 7.0), Array(4))
    assertEquals((a <= 4.0).toArray.toSeq, Seq(true, true, false, false))
  }

  test("comparison =:= scalar") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertEquals((a =:= 2.0).toArray.toSeq, Seq(false, true, false))
  }

  test("comparison !:= scalar") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertEquals((a !:= 2.0).toArray.toSeq, Seq(true, false, true))
  }

  test("comparison > array") {
    val a = NDArray(Array(5.0, 3.0, 7.0), Array(3))
    val b = NDArray(Array(4.0, 4.0, 4.0), Array(3))
    assertEquals((a > b).toArray.toSeq, Seq(true, false, true))
  }

  test("comparison =:= array") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array(1.0, 0.0, 3.0), Array(3))
    assertEquals((a =:= b).toArray.toSeq, Seq(true, false, true))
  }

  // ── Shape mismatch errors ─────────────────────────────────────────────────

  test("binary op throws ShapeMismatchException on shape mismatch") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array(1.0, 2.0), Array(2))
    intercept[ShapeMismatchException] { a + b }
    intercept[ShapeMismatchException] { a - b }
    intercept[ShapeMismatchException] { a * b }
    intercept[ShapeMismatchException] { a / b }
  }

  // ── Broadcasting ──────────────────────────────────────────────────────────

  test("broadcastTo 1D → 2D") {
    val row = NDArray(Array(1.0, 2.0, 3.0), Array(1, 3))
    val bcast = row.broadcastTo(Array(4, 3))
    assertEquals(bcast.shape.toSeq, Seq(4, 3))
    assertEquals(bcast.strides.toSeq, Seq(0, 1)) // stride-0 for broadcast dim
    // verify by materialising via arithmetic to avoid inline-given scope issues
    val expected = Array(1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0)
    val zeros = NDArray.zeros[Double](Array(4, 3))
    assertNDArrayClose(bcast + zeros, expected)
  }

  test("broadcastPair computes common shape") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array(10.0), Array(1))
    val (a2, b2) = broadcastPair(a, b)
    assertEquals(a2.shape.toSeq, Seq(3))
    assertEquals(b2.shape.toSeq, Seq(3))
    // verify b2 broadcasts 10.0 across all 3 positions via arithmetic
    val result = a2 + b2
    assertNDArrayClose(result, Array(11.0, 12.0, 13.0))
  }

  test("binary op on broadcast views produces correct results") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(1, 3))
    val ones = NDArray.fill(Array(4, 3), 1.0)
    val (a2, ones2) = broadcastPair(a, ones)
    val result = a2 + ones2 // both shape [4,3] col-major result
    // col-major order for [4,3]: (0,0),(1,0),(2,0),(3,0),(0,1),(1,1),(2,1),(3,1),(0,2),(1,2),(2,2),(3,2)
    // a2 rows are all [1,2,3], plus ones → [2,3,4] per row
    val arr = result.toArray
    assertEquals(arr.length, 12)
    // col 0 (rows 0-3): 1+1=2
    for i <- 0 until 4 do assertClose(arr(i), 2.0, s"col0 row $i")
    // col 1 (rows 0-3): 2+1=3
    for i <- 0 until 4 do assertClose(arr(4 + i), 3.0, s"col1 row $i")
    // col 2 (rows 0-3): 3+1=4
    for i <- 0 until 4 do assertClose(arr(8 + i), 4.0, s"col2 row $i")
  }

  test("broadcastTo throws on incompatible shapes") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    intercept[BroadcastException] { a.broadcastTo(Array(4)) }
  }

  test("broadcastShape for compatible shapes") {
    // [3] and [1,3] → [1,3]
    val s = broadcastShape(Array(3), Array(1, 3))
    assertEquals(s.toSeq, Seq(1, 3))
    // [1,3] and [4,1] → [4,3]
    val s2 = broadcastShape(Array(1, 3), Array(4, 1))
    assertEquals(s2.toSeq, Seq(4, 3))
  }

  test("broadcastShape throws on incompatible shapes") {
    intercept[BroadcastException] { broadcastShape(Array(3), Array(4)) }
  }

  test("sameShape") {
    assert(sameShape(Array(2, 3), Array(2, 3)))
    assert(!sameShape(Array(2, 3), Array(3, 2)))
    assert(!sameShape(Array(2), Array(2, 1)))
  }

  // ── N-D ops (3D array) ────────────────────────────────────────────────────

  test("element-wise ops on 3D array") {
    val data = Array.tabulate(24)(_.toDouble)
    val a = NDArray(data, Array(2, 3, 4))
    val b = NDArray(Array.fill(24)(1.0), Array(2, 3, 4))
    val result = a + b
    assertNDArrayClose(result, data.map(_ + 1.0))
  }

end NDArrayElemWiseSuite
