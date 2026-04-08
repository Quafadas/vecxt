package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArrayBooleanOpsSuite extends FunSuite:

  // ── Logical binary ops ─────────────────────────────────────────────────────

  test("NDArray[Boolean] && (col-major)") {
    val a = NDArray(Array(true, false, true, false), Array(4))
    val b = NDArray(Array(true, true, false, false), Array(4))
    assertEquals((a && b).toArray.toSeq, Seq(true, false, false, false))
  }

  test("NDArray[Boolean] || (col-major)") {
    val a = NDArray(Array(true, false, true, false), Array(4))
    val b = NDArray(Array(true, true, false, false), Array(4))
    assertEquals((a || b).toArray.toSeq, Seq(true, true, true, false))
  }

  test("NDArray[Boolean] && via general kernel (transposed)") {
    val a = NDArray(Array(true, false, true, false), Array(2, 2))
    val at = a.T
    val bt = a.T
    assert(!at.isColMajor)
    val result = at && bt
    assertEquals(result.isColMajor, true)
    assertEquals(result.toArray.toSeq, at.toArray.toSeq)
  }

  test("NDArray[Boolean] && shape mismatch throws") {
    val a = NDArray(Array(true, false, true), Array(3))
    val b = NDArray(Array(true, false), Array(2))
    intercept[ShapeMismatchException](a && b)
  }

  // ── Logical unary ops ─────────────────────────────────────────────────────

  test("NDArray[Boolean] not (col-major)") {
    val a = NDArray(Array(true, false, true, false), Array(4))
    assertEquals(a.not.toArray.toSeq, Seq(false, true, false, true))
  }

  test("NDArray[Boolean] not via general kernel (transposed)") {
    val a = NDArray(Array(true, false, false, true), Array(2, 2))
    val at = a.T
    assert(!at.isColMajor)
    val result = at.not
    assertEquals(result.isColMajor, true)
    assertEquals(result.toArray.toSeq, at.toArray.map(!_).toSeq)
  }

  test("NDArray[Boolean] not! in-place") {
    val a = NDArray(Array(true, false, true), Array(3))
    a.`not!`
    assertEquals(a.toArray.toSeq, Seq(false, true, false))
  }

  test("NDArray[Boolean] not! non-contiguous throws") {
    val a = NDArray(Array(true, false, false, true), Array(2, 2))
    val at = a.T
    intercept[InvalidNDArray](at.`not!`)
  }

  // ── Full reductions ────────────────────────────────────────────────────────

  test("NDArray[Boolean] any — some true") {
    val a = NDArray(Array(false, false, true, false), Array(4))
    assertEquals(a.any, true)
  }

  test("NDArray[Boolean] any — all false") {
    val a = NDArray(Array(false, false, false), Array(3))
    assertEquals(a.any, false)
  }

  test("NDArray[Boolean] all — all true") {
    val a = NDArray(Array(true, true, true), Array(3))
    assertEquals(a.all, true)
  }

  test("NDArray[Boolean] all — some false") {
    val a = NDArray(Array(true, false, true), Array(3))
    assertEquals(a.all, false)
  }

  test("NDArray[Boolean] countTrue") {
    val a = NDArray(Array(true, false, true, true, false), Array(5))
    assertEquals(a.countTrue, 3)
  }

  test("NDArray[Boolean] any strided (general path)") {
    val a = NDArray(Array(false, true, false, false), Array(2, 2))
    val t = a.T
    assert(!t.isColMajor)
    assertEquals(t.any, true)
  }

  test("NDArray[Boolean] all strided (general path)") {
    val a = NDArray(Array(true, true, true, true), Array(2, 2))
    val t = a.T
    assert(!t.isColMajor)
    assertEquals(t.all, true)
  }

  test("NDArray[Boolean] countTrue strided (general path)") {
    val a = NDArray(Array(true, false, true, true), Array(2, 2))
    val t = a.T
    assert(!t.isColMajor)
    assertEquals(t.countTrue, 3)
  }

  // ── Axis reductions ────────────────────────────────────────────────────────

  test("NDArray[Boolean] any(axis=0) on 2×3") {
    // col-major [T,F,T,F,F,T] shape [2,3]
    // axis 0: (T||F)=T, (T||F)=T, (F||T)=T
    val a = NDArray(Array(true, false, true, false, false, true), Array(2, 3))
    val result = a.any(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(true, true, true))
  }

  test("NDArray[Boolean] all(axis=1) on 2×3") {
    // col-major [T,T,T,T,T,T] shape [2,3]: all true
    val a = NDArray(Array(true, true, true, true, true, true), Array(2, 3))
    val result = a.all(1)
    assertEquals(result.shape.toSeq, Seq(2))
    assertEquals(result.toArray.toSeq, Seq(true, true))
  }

  test("NDArray[Boolean] all(axis=1) with some false") {
    val a = NDArray(Array(true, false, true, true, true, true), Array(2, 3))
    val result = a.all(1)
    assertEquals(result.shape.toSeq, Seq(2))
    // row 0: col0=T, col1=T, col2=T → T; row 1: col0=F, col1=T, col2=T → F
    assertEquals(result.toArray.toSeq, Seq(true, false))
  }

  test("NDArray[Boolean] countTrue(axis=0) on 2×3") {
    val a = NDArray(Array(true, false, true, true, false, true), Array(2, 3))
    val result = a.countTrue(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(1, 2, 1))
  }

  test("NDArray[Boolean] axis out of range throws") {
    val a = NDArray(Array(true, false, true), Array(3))
    intercept[InvalidNDArray](a.any(-1))
    intercept[InvalidNDArray](a.all(1))
    intercept[InvalidNDArray](a.countTrue(1))
  }

end NDArrayBooleanOpsSuite
