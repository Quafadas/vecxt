package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArrayWhereSuite extends FunSuite:

  // ── where(condition, x: NDArray, y: NDArray) ─────────────────────────────

  test("where NDArray NDArray - basic 1D"):
    val cond = NDArray(Array(true, false, true, false), Array(4))
    val x = NDArray(Array(10.0, 20.0, 30.0, 40.0), Array(4))
    val y = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    val result = where(cond, x, y)
    assertEquals(result.toArray.toSeq, Seq(10.0, 2.0, 30.0, 4.0))

  test("where NDArray NDArray - all true"):
    val cond = NDArray(Array(true, true, true), Array(3))
    val x = NDArray(Array(5.0, 6.0, 7.0), Array(3))
    val y = NDArray(Array(0.0, 0.0, 0.0), Array(3))
    assertEquals(where(cond, x, y).toArray.toSeq, Seq(5.0, 6.0, 7.0))

  test("where NDArray NDArray - all false"):
    val cond = NDArray(Array(false, false, false), Array(3))
    val x = NDArray(Array(5.0, 6.0, 7.0), Array(3))
    val y = NDArray(Array(0.0, 1.0, 2.0), Array(3))
    assertEquals(where(cond, x, y).toArray.toSeq, Seq(0.0, 1.0, 2.0))

  test("where NDArray NDArray - shape mismatch throws"):
    val cond = NDArray(Array(true, false, true), Array(3))
    val x = NDArray(Array(1.0, 2.0), Array(2))
    val y = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    intercept[ShapeMismatchException](where(cond, x, y))

  test("where NDArray NDArray - result is col-major"):
    val cond = NDArray(Array(true, false), Array(2))
    val x = NDArray(Array(1.0, 2.0), Array(2))
    val y = NDArray(Array(9.0, 8.0), Array(2))
    assert(where(cond, x, y).isColMajor)

  test("where NDArray NDArray - non-contiguous (transposed view)"):
    val a = NDArray(Array(true, false, false, true), Array(2, 2))
    val at = a.T
    val x = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val y = NDArray(Array(9.0, 8.0, 7.0, 6.0), Array(2, 2))
    val xt = x.T
    val yt = y.T
    val result = where(at, xt, yt)
    // at has logical layout [T,F; F,T] (col-major read of transposed)
    assertEquals(result.isColMajor, true)

  // ── where(condition, x: scalar, y: NDArray) ───────────────────────────────

  test("where scalar-x NDArray-y - basic"):
    val cond = NDArray(Array(true, false, true, false), Array(4))
    val y = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    val result = where(cond, 99.0, y)
    assertEquals(result.toArray.toSeq, Seq(99.0, 2.0, 99.0, 4.0))

  test("where scalar-x NDArray-y - shape mismatch throws"):
    val cond = NDArray(Array(true, false), Array(2))
    val y = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    intercept[ShapeMismatchException](where(cond, 1.0, y))

  // ── where(condition, x: NDArray, y: scalar) ───────────────────────────────

  test("where NDArray-x scalar-y - basic"):
    val cond = NDArray(Array(true, false, true, false), Array(4))
    val x = NDArray(Array(10.0, 20.0, 30.0, 40.0), Array(4))
    val result = where(cond, x, 0.0)
    assertEquals(result.toArray.toSeq, Seq(10.0, 0.0, 30.0, 0.0))

  test("where NDArray-x scalar-y - shape mismatch throws"):
    val cond = NDArray(Array(true, false), Array(2))
    val x = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    intercept[ShapeMismatchException](where(cond, x, 0.0))

  // ── where(condition, x: scalar, y: scalar) ────────────────────────────────

  test("where scalar-x scalar-y - basic"):
    val cond = NDArray(Array(true, false, false, true), Array(4))
    val result = where(cond, 1.0, 0.0)
    assertEquals(result.toArray.toSeq, Seq(1.0, 0.0, 0.0, 1.0))

  test("where scalar-x scalar-y - works on Int type"):
    val cond = NDArray(Array(true, false, true), Array(3))
    val result = where(cond, 7, -1)
    assertEquals(result.toArray.toSeq, Seq(7, -1, 7))

  test("where scalar-x scalar-y - non-contiguous condition"):
    val a = NDArray(Array(true, false, false, true), Array(2, 2))
    val at = a.T
    assert(!at.isColMajor)
    val result = where(at, 1.0, 0.0)
    assertEquals(result.isColMajor, true)

end NDArrayWhereSuite
