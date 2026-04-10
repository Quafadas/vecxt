package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArray0DSuite extends FunSuite:

  // ── Construction tests ───────────────────────────────────────────────────

  test("NDArray.scalar creates 0-d array") {
    val arr = NDArray.scalar(3.0)
    assertEquals(arr.ndim, 0)
    assertEquals(arr.numel, 1)
    assertEquals(arr.shape.toSeq, Seq.empty[Int])
    assertEquals(arr.strides.toSeq, Seq.empty[Int])
    assertClose(arr.data(0), 3.0)
  }

  test("NDArray(Array(v), Array(), Array(), 0) primary constructor works") {
    val arr = NDArray(Array(5.0), Array(), Array(), 0)
    assertEquals(arr.ndim, 0)
    assertEquals(arr.numel, 1)
    assertClose(arr.data(0), 5.0)
  }

  test("NDArray.zeros(Array()) creates 0-d zero") {
    val arr = NDArray.zeros[Double](Array())
    assertEquals(arr.ndim, 0)
    assertEquals(arr.numel, 1)
    assertClose(arr.data(0), 0.0)
  }

  test("NDArray.ones(Array()) creates 0-d one") {
    val arr = NDArray.ones[Double](Array())
    assertEquals(arr.ndim, 0)
    assertEquals(arr.numel, 1)
    assertClose(arr.data(0), 1.0)
  }

  test("NDArray.fill(Array(), 7.0) creates 0-d with value 7.0") {
    val arr = NDArray.fill(Array(), 7.0)
    assertEquals(arr.ndim, 0)
    assertEquals(arr.numel, 1)
    assertClose(arr.data(0), 7.0)
  }

  // ── Property tests ───────────────────────────────────────────────────────

  test("0-d isContiguous, isColMajor, isRowMajor all true") {
    val arr = NDArray.scalar(3.0)
    assert(arr.isContiguous)
    assert(arr.isColMajor)
    assert(arr.isRowMajor)
  }

  test("0-d isScalar is true") {
    val arr = NDArray.scalar(3.0)
    assert(arr.isScalar)
  }

  test("1-d isScalar is false") {
    val arr = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assert(!arr.isScalar)
  }

  test("0-d numel is 1") {
    val arr = NDArray.scalar(42.0)
    assertEquals(arr.numel, 1)
  }

  test("0-d layout string is sensible") {
    val arr = NDArray.scalar(3.0)
    val l = arr.layout
    assert(l.contains("ndim"))
    assert(l.contains("shape"))
    assert(l.contains("strides"))
    assert(l.contains("offset"))
  }

  // ── Element access tests ─────────────────────────────────────────────────

  test("0-d scalar accessor returns value") {
    val arr = NDArray.scalar(7.5)
    assertClose(arr.scalar, 7.5)
  }

  test("0-d apply(Array()) returns value") {
    val arr = NDArray.scalar(2.0)
    assertClose(arr(Array.emptyIntArray), 2.0)
  }

  test("0-d apply(0) throws rank mismatch") {
    val arr = NDArray.scalar(1.0)
    intercept[InvalidNDArray] {
      arr(0)
    }
  }

  test("0-d setScalar writes value") {
    val arr = NDArray.scalar(1.0)
    arr.setScalar(99.0)
    assertClose(arr.data(0), 99.0)
  }

  test("0-d update(Array(), v) writes value") {
    val arr = NDArray.scalar(1.0)
    arr(Array.emptyIntArray) = 42.0
    assertClose(arr.data(0), 42.0)
  }

  test("0-d scalar accessor throws when applied to non-0-d") {
    val arr = NDArray(Array(1.0, 2.0), Array(2))
    intercept[InvalidNDArray] {
      arr.scalar
    }
  }

  test("0-d setScalar throws when applied to non-0-d") {
    val arr = NDArray(Array(1.0, 2.0), Array(2))
    intercept[InvalidNDArray] {
      arr.setScalar(5.0)
    }
  }

  // ── View operation tests ─────────────────────────────────────────────────

  test("0-d squeeze returns 0-d") {
    val arr = NDArray.scalar(3.0)
    val s = arr.squeeze
    assertEquals(s.ndim, 0)
  }

  test("0-d unsqueeze(0) returns 1-d shape [1]") {
    val arr = NDArray.scalar(3.0)
    val u = arr.unsqueeze(0)
    assertEquals(u.shape.toSeq, Seq(1))
    assertClose(u(0), 3.0)
  }

  test("0-d flatten returns 1-d shape [1]") {
    val arr = NDArray.scalar(3.0)
    val f = arr.flatten
    assertEquals(f.shape.toSeq, Seq(1))
    assertClose(f(0), 3.0)
  }

  test("0-d reshape(Array(1)) returns 1-d shape [1]") {
    val arr = NDArray.scalar(3.0)
    val r = arr.reshape(Array(1))
    assertEquals(r.shape.toSeq, Seq(1))
    assertClose(r(0), 3.0)
  }

  test("0-d reshape(Array(1,1)) returns 2-d shape [1,1]") {
    val arr = NDArray.scalar(3.0)
    val r = arr.reshape(Array(1, 1))
    assertEquals(r.shape.toSeq, Seq(1, 1))
    assertClose(r(0, 0), 3.0)
  }

  test("1-element 1-d reshape(Array()) returns 0-d") {
    val arr = NDArray(Array(5.0), Array(1))
    val r = arr.reshape(Array())
    assertEquals(r.ndim, 0)
    assertEquals(r.numel, 1)
    assertClose(r.data(0), 5.0)
  }

  test("0-d toArray returns Array(value)") {
    val arr = NDArray.scalar(7.0)
    val a = arr.toArray
    assertEquals(a.toSeq, Seq(7.0))
  }

  test("0-d transpose(Array()) returns 0-d") {
    val arr = NDArray.scalar(3.0)
    val t = arr.transpose(Array())
    assertEquals(t.ndim, 0)
    assertClose(t.data(0), 3.0)
  }

  // ── Broadcast tests ──────────────────────────────────────────────────────

  test("broadcastShape(Array(), Array(3,4)) == Array(3,4)") {
    assertEquals(broadcastShape(Array(), Array(3, 4)).toSeq, Seq(3, 4))
  }

  test("broadcastShape(Array(), Array()) == Array()") {
    assertEquals(broadcastShape(Array(), Array()).toSeq, Seq.empty[Int])
  }

  test("broadcastShape(Array(3), Array()) == Array(3)") {
    assertEquals(broadcastShape(Array(3), Array()).toSeq, Seq(3))
  }

  test("0-d broadcastTo(Array(2,3)) produces correct shape and stride-0") {
    val arr = NDArray.scalar(5.0)
    val b = arr.broadcastTo(Array(2, 3))
    assertEquals(b.shape.toSeq, Seq(2, 3))
    assertEquals(b.strides.toSeq, Seq(0, 0))
    // all elements should be 5.0
    for i <- 0 until 2 do
      for j <- 0 until 3 do assertClose(b(i, j), 5.0)
  }

  test("0-d broadcastTo(Array()) is identity") {
    val arr = NDArray.scalar(5.0)
    val b = arr.broadcastTo(Array())
    assertEquals(b.ndim, 0)
    assertClose(b.data(b.offset), 5.0)
  }

  // ── Binary op tests (Double) ─────────────────────────────────────────────

  test("0-d + 1-d: scalar 3.0 + [1,2,3] == [4,5,6]") {
    val a = NDArray.scalar(3.0)
    val b = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertNDArrayClose(a + b, Array(4.0, 5.0, 6.0))
  }

  test("1-d + 0-d: [1,2,3] + scalar 3.0 == [4,5,6]") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray.scalar(3.0)
    assertNDArrayClose(a + b, Array(4.0, 5.0, 6.0))
  }

  test("0-d + 0-d: scalar 2.0 + scalar 3.0 == scalar 5.0") {
    val a = NDArray.scalar(2.0)
    val b = NDArray.scalar(3.0)
    val r = a + b
    assertEquals(r.ndim, 0)
    assertClose(r.data(0), 5.0)
  }

  test("0-d * 2-d: scalar 2.0 * [[1,2],[3,4]] == [[2,4],[6,8]]") {
    val a = NDArray.scalar(2.0)
    val b = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    assertNDArrayClose(a * b, Array(2.0, 4.0, 6.0, 8.0))
  }

  test("0-d - 1-d: scalar 10.0 - [1,2,3] == [9,8,7]") {
    val a = NDArray.scalar(10.0)
    val b = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertNDArrayClose(a - b, Array(9.0, 8.0, 7.0))
  }

  test("1-d - 0-d: [10,20,30] - scalar 1.0 == [9,19,29]") {
    val a = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    val b = NDArray.scalar(1.0)
    assertNDArrayClose(a - b, Array(9.0, 19.0, 29.0))
  }

  test("0-d / 1-d: scalar 12.0 / [2,3,4] == [6,4,3]") {
    val a = NDArray.scalar(12.0)
    val b = NDArray(Array(2.0, 3.0, 4.0), Array(3))
    assertNDArrayClose(a / b, Array(6.0, 4.0, 3.0))
  }

  test("1-d / 0-d: [6,4,3] / scalar 2.0 == [3,2,1.5]") {
    val a = NDArray(Array(6.0, 4.0, 3.0), Array(3))
    val b = NDArray.scalar(2.0)
    assertNDArrayClose(a / b, Array(3.0, 2.0, 1.5))
  }

  // ── Binary op tests (Float) ──────────────────────────────────────────────

  test("Float: 0-d + 1-d: scalar 3f + [1f,2f,3f] == [4f,5f,6f]") {
    val a = NDArray.scalar(3.0f)
    val b = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    val r = a + b
    assertEquals(r.toArray.toSeq, Seq(4.0f, 5.0f, 6.0f))
  }

  test("Float: 1-d + 0-d: [1f,2f,3f] + scalar 3f == [4f,5f,6f]") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    val b = NDArray.scalar(3.0f)
    val r = a + b
    assertEquals(r.toArray.toSeq, Seq(4.0f, 5.0f, 6.0f))
  }

  test("Float: 0-d + 0-d: scalar 2f + scalar 3f == scalar 5f") {
    val a = NDArray.scalar(2.0f)
    val b = NDArray.scalar(3.0f)
    val r = a + b
    assertEquals(r.ndim, 0)
    assertEquals(r.data(0), 5.0f)
  }

  test("Float: 0-d - 1-d: scalar 10f - [1f,2f,3f] == [9f,8f,7f]") {
    val a = NDArray.scalar(10.0f)
    val b = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    val r = a - b
    assertEquals(r.toArray.toSeq, Seq(9.0f, 8.0f, 7.0f))
  }

  test("Float: 1-d - 0-d: [10f,20f,30f] - scalar 1f == [9f,19f,29f]") {
    val a = NDArray(Array(10.0f, 20.0f, 30.0f), Array(3))
    val b = NDArray.scalar(1.0f)
    val r = a - b
    assertEquals(r.toArray.toSeq, Seq(9.0f, 19.0f, 29.0f))
  }

  test("Float: 0-d / 1-d: scalar 12f / [2f,3f,4f] == [6f,4f,3f]") {
    val a = NDArray.scalar(12.0f)
    val b = NDArray(Array(2.0f, 3.0f, 4.0f), Array(3))
    val r = a / b
    assertEquals(r.toArray.toSeq, Seq(6.0f, 4.0f, 3.0f))
  }

  test("Float: 1-d / 0-d: [6f,4f,3f] / scalar 2f == [3f,2f,1.5f]") {
    val a = NDArray(Array(6.0f, 4.0f, 3.0f), Array(3))
    val b = NDArray.scalar(2.0f)
    val r = a / b
    assertEquals(r.toArray.toSeq, Seq(3.0f, 2.0f, 1.5f))
  }

  // ── In-place op tests ────────────────────────────────────────────────────

  test("1-d += 0-d: [1,2,3] += scalar 10 -> [11,12,13]") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray.scalar(10.0)
    a += b
    assertNDArrayClose(a, Array(11.0, 12.0, 13.0))
  }

  test("0-d += 0-d: scalar 3 += scalar 2 -> scalar 5") {
    val a = NDArray.scalar(3.0)
    val b = NDArray.scalar(2.0)
    a += b
    assertClose(a.data(0), 5.0)
  }

  test("1-d *= 0-d: [1,2,3] *= scalar 2 -> [2,4,6]") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray.scalar(2.0)
    a *= b
    assertNDArrayClose(a, Array(2.0, 4.0, 6.0))
  }

  test("1-d -= 0-d: [10,20,30] -= scalar 5 -> [5,15,25]") {
    val a = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    val b = NDArray.scalar(5.0)
    a -= b
    assertNDArrayClose(a, Array(5.0, 15.0, 25.0))
  }

  test("1-d /= 0-d: [10,20,30] /= scalar 2 -> [5,10,15]") {
    val a = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    val b = NDArray.scalar(2.0)
    a /= b
    assertNDArrayClose(a, Array(5.0, 10.0, 15.0))
  }

  // ── Comparison op tests ──────────────────────────────────────────────────

  test("0-d > 1-d: scalar 5.0 > [3,5,7] == [true, false, false]") {
    val a = NDArray.scalar(5.0)
    val b = NDArray(Array(3.0, 5.0, 7.0), Array(3))
    val r = a > b
    assertEquals(r.toArray.toSeq, Seq(true, false, false))
  }

  test("1-d =:= 0-d: [1,2,3] =:= scalar 2.0 == [false, true, false]") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray.scalar(2.0)
    val r = a =:= b
    assertEquals(r.toArray.toSeq, Seq(false, true, false))
  }

  test("0-d < 1-d: scalar 2.0 < [1,2,3] == [false, false, true]") {
    val a = NDArray.scalar(2.0)
    val b = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val r = a < b
    assertEquals(r.toArray.toSeq, Seq(false, false, true))
  }

  // ── Reduction tests ──────────────────────────────────────────────────────

  test("0-d sum returns the scalar value") {
    val arr = NDArray.scalar(7.0)
    assertClose(arr.sum, 7.0)
  }

  test("0-d mean returns the scalar value") {
    val arr = NDArray.scalar(3.5)
    assertClose(arr.mean, 3.5)
  }

  test("0-d min returns the scalar value") {
    val arr = NDArray.scalar(4.0)
    assertClose(arr.min, 4.0)
  }

  test("0-d max returns the scalar value") {
    val arr = NDArray.scalar(4.0)
    assertClose(arr.max, 4.0)
  }

  // ── Unary op tests ───────────────────────────────────────────────────────

  test("0-d exp: scalar(1.0).exp == scalar(e)") {
    val arr = NDArray.scalar(1.0)
    val r = arr.exp
    assertEquals(r.ndim, 0)
    assertClose(r.data(0), Math.E)
  }

  test("0-d neg: scalar(3.0).neg == scalar(-3.0)") {
    val arr = NDArray.scalar(3.0)
    val r = arr.neg
    assertEquals(r.ndim, 0)
    assertClose(r.data(0), -3.0)
  }

  test("0-d sqrt: scalar(4.0).sqrt == scalar(2.0)") {
    val arr = NDArray.scalar(4.0)
    val r = arr.sqrt
    assertEquals(r.ndim, 0)
    assertClose(r.data(0), 2.0)
  }

  test("0-d log: scalar(1.0).log == scalar(0.0)") {
    val arr = NDArray.scalar(1.0)
    val r = arr.log
    assertEquals(r.ndim, 0)
    assertClose(r.data(0), 0.0)
  }

  test("0-d tanh: scalar(0.0).tanh == scalar(0.0)") {
    val arr = NDArray.scalar(0.0)
    val r = arr.tanh
    assertEquals(r.ndim, 0)
    assertClose(r.data(0), 0.0)
  }

  test("0-d sigmoid: scalar(0.0).sigmoid == scalar(0.5)") {
    val arr = NDArray.scalar(0.0)
    val r = arr.sigmoid
    assertEquals(r.ndim, 0)
    assertClose(r.data(0), 0.5)
  }

end NDArray0DSuite
