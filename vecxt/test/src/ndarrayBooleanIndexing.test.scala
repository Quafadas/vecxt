package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArrayBooleanIndexingSuite extends FunSuite:

  // ── apply(mask) ───────────────────────────────────────────────────────────

  test("boolean indexing - select true elements 1D"):
    val a = NDArray(Array(10.0, 20.0, 30.0, 40.0, 50.0), Array(5))
    val mask = NDArray(Array(true, false, true, false, true), Array(5))
    val result = a(mask)
    assertEquals(result.toArray.toSeq, Seq(10.0, 30.0, 50.0))
    assertEquals(result.ndim, 1)
    assertEquals(result.numel, 3)

  test("boolean indexing - all false returns empty"):
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val mask = NDArray(Array(false, false, false), Array(3))
    val result = a(mask)
    assertEquals(result.numel, 0)

  test("boolean indexing - all true returns all"):
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val mask = NDArray(Array(true, true, true), Array(3))
    val result = a(mask)
    assertEquals(result.toArray.toSeq, Seq(1.0, 2.0, 3.0))

  test("boolean indexing - result is col-major 1D"):
    val a = NDArray(Array(5.0, 6.0, 7.0), Array(3))
    val mask = NDArray(Array(false, true, true), Array(3))
    val result = a(mask)
    assert(result.isColMajor)

  test("boolean indexing - works on Int NDArray"):
    val a = NDArray(Array(1, 2, 3, 4, 5), Array(5))
    val mask = NDArray(Array(false, true, false, true, false), Array(5))
    val result = a(mask)
    assertEquals(result.toArray.toSeq, Seq(2, 4))

  test("boolean indexing - shape mismatch throws"):
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val mask = NDArray(Array(true, false), Array(2))
    intercept[ShapeMismatchException](a(mask))

  test("boolean indexing - 2D col-major"):
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val mask = NDArray(Array(true, false, false, true), Array(2, 2))
    val result = a(mask)
    assertEquals(result.toArray.toSeq, Seq(1.0, 4.0))

  // ── update(mask, value) ───────────────────────────────────────────────────

  test("boolean mask assignment - sets true positions"):
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    val mask = NDArray(Array(true, false, true, false), Array(4))
    a(mask) = 99.0
    assertEquals(a.toArray.toSeq, Seq(99.0, 2.0, 99.0, 4.0))

  test("boolean mask assignment - all false leaves unchanged"):
    val a = NDArray(Array(5.0, 6.0, 7.0), Array(3))
    val mask = NDArray(Array(false, false, false), Array(3))
    a(mask) = 0.0
    assertEquals(a.toArray.toSeq, Seq(5.0, 6.0, 7.0))

  test("boolean mask assignment - all true sets all"):
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val mask = NDArray(Array(true, true, true), Array(3))
    a(mask) = -1.0
    assertEquals(a.toArray.toSeq, Seq(-1.0, -1.0, -1.0))

  test("boolean mask assignment - shape mismatch throws"):
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val mask = NDArray(Array(true, false), Array(2))
    intercept[ShapeMismatchException](a(mask) = 0.0)

  test("boolean mask assignment - Int NDArray"):
    val a = NDArray(Array(1, 2, 3, 4, 5), Array(5))
    val mask = NDArray(Array(false, false, true, false, true), Array(5))
    a(mask) = 0
    assertEquals(a.toArray.toSeq, Seq(1, 2, 0, 4, 0))

end NDArrayBooleanIndexingSuite
