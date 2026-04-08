package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArrayIntReductionsSuite extends FunSuite:

  test("NDArray[Int] sum") {
    val a = NDArray(Array(1, 2, 3, 4), Array(4))
    assertEquals(a.sum, 10)
  }

  test("NDArray[Int] sum (col-major 2D)") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    assertEquals(a.sum, 21)
  }

  test("NDArray[Int] sum strided (general path)") {
    val a = NDArray(Array(1, 2, 3, 4), Array(2, 2))
    val t = a.T
    assert(!t.isColMajor)
    assertEquals(t.sum, 10)
  }

  test("NDArray[Int] mean") {
    val a = NDArray(Array(2, 4, 6, 8), Array(4))
    assertEqualsDouble(a.mean, 5.0, 1e-10)
  }

  test("NDArray[Int] min / max") {
    val a = NDArray(Array(3, 1, 4, 1, 5, 9), Array(6))
    assertEquals(a.min, 1)
    assertEquals(a.max, 9)
  }

  test("NDArray[Int] min / max strided") {
    val a = NDArray(Array(3, 1, 4, 1, 5, 9), Array(2, 3))
    val t = a.T
    assert(!t.isColMajor)
    assertEquals(t.min, 1)
    assertEquals(t.max, 9)
  }

  test("NDArray[Int] product") {
    val a = NDArray(Array(1, 2, 3, 4), Array(4))
    assertEquals(a.product, 24)
  }

  test("NDArray[Int] argmax / argmin") {
    val a = NDArray(Array(3, 1, 4, 1, 5, 9), Array(6))
    assertEquals(a.argmax, 5)
    assertEquals(a.argmin, 1)
  }

  test("NDArray[Int] argmax / argmin strided") {
    val a = NDArray(Array(1, 5, 3, 2), Array(2, 2))
    val t = a.T
    assert(!t.isColMajor)
    // Col-major traversal of t (shape [2,2]): (0,0)=1, (1,0)=3, (0,1)=5, (1,1)=2 → argmax=2, argmin=0
    assertEquals(t.argmax, 2)
    assertEquals(t.argmin, 0)
  }

  // ── Axis reductions (2D) ─────────────────────────────────────────────────

  test("NDArray[Int] sum(axis=0) on 2×3") {
    // col-major data [1,2,3,4,5,6] → shape [2,3]
    // col 0: [1,2], col 1: [3,4], col 2: [5,6]
    // sum along axis 0 → [3, 7, 11]
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val result = a.sum(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(3, 7, 11))
  }

  test("NDArray[Int] sum(axis=1) on 2×3") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val result = a.sum(1)
    assertEquals(result.shape.toSeq, Seq(2))
    assertEquals(result.toArray.toSeq, Seq(9, 12))
  }

  test("NDArray[Int] max(axis=0) on 2×3") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val result = a.max(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(2, 4, 6))
  }

  test("NDArray[Int] min(axis=1) on 2×3") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val result = a.min(1)
    assertEquals(result.shape.toSeq, Seq(2))
    assertEquals(result.toArray.toSeq, Seq(1, 2))
  }

  test("NDArray[Int] product(axis=0) on 2×3") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val result = a.product(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(2, 12, 30))
  }

  test("NDArray[Int] mean(axis=0) on 2×3 returns Double") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val result = a.mean(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEqualsDouble(result.toArray(0), 1.5, 1e-10)
    assertEqualsDouble(result.toArray(1), 3.5, 1e-10)
    assertEqualsDouble(result.toArray(2), 5.5, 1e-10)
  }

  test("NDArray[Int] argmax(axis=0) on 2×3") {
    // data [1,4,3,2,5,6] col-major [2,3]
    // max along axis 0: col0→row1(idx=1), col1→row0(idx=0), col2→row1(idx=1)
    val a = NDArray(Array(1, 4, 3, 2, 5, 6), Array(2, 3))
    val result = a.argmax(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(1, 0, 1))
  }

  test("NDArray[Int] argmin(axis=1) on 2×3") {
    val a = NDArray(Array(5, 6, 1, 2, 3, 4), Array(2, 3))
    val result = a.argmin(1)
    assertEquals(result.shape.toSeq, Seq(2))
    assertEquals(result.toArray.toSeq, Seq(1, 1))
  }

  // ── Axis validation ────────────────────────────────────────────────────────

  test("NDArray[Int] sum(-1) throws InvalidNDArray") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    intercept[InvalidNDArray] { a.sum(-1) }
  }

  test("NDArray[Int] sum(ndim) throws InvalidNDArray") {
    val a = NDArray(Array(1, 2, 3, 4), Array(2, 2))
    intercept[InvalidNDArray] { a.sum(2) }
  }

end NDArrayIntReductionsSuite
