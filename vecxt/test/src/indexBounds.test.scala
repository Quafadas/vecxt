package vecxt

import munit.FunSuite

import all.*

/** Regression coverage for the `indexCheckMat` off-by-one: it used to accept `row == rows` and `col == cols` (one past
  * the last valid index in that axis) because the bounds predicate compared with `<=` instead of `<`. That let
  * `apply`/`update` compute a linear index into the backing array for an out-of-range `(row, col)` instead of throwing
  * `IndexOutOfBoundsException` — sometimes landing on a different, valid-looking element (silent wrong answer) rather
  * than failing loudly.
  *
  * Every matrix here is non-square, so a swapped row/col bound would also be caught. Each element type below reaches a
  * different arm of `indexCheckMat`: `Double`/`Float`/`Int`/`Long` each have their own `@targetName` overload, and
  * `Boolean` has none, so it falls through to the generic `apply(a: Matrix[?], dim: RowCol)` arm.
  */
class IndexBoundsSuite extends FunSuite:

  // ─── Double, dense column-major (default layout) ──────────────────────────
  // 2x3, col-major: col0=[1,2], col1=[3,4], col2=[5,6]. This is the exact shape from the bug report: pre-fix,
  // `mat(2, 0)` passed the bounds check and read raw(2) == 3.0, silently returning element (0, 1) instead of
  // throwing.

  private def denseColMajorDouble: Matrix[Double] = Matrix[Double](Array.tabulate(6)(_.toDouble + 1), 2, 3)

  test("Double col-major: row == rows / col == cols / both throw on read") {
    val mat = denseColMajorDouble
    intercept[IndexOutOfBoundsException](mat(2, 0)) // row == rows; used to silently return element (0, 1)
    intercept[IndexOutOfBoundsException](mat(0, 3)) // col == cols
    intercept[IndexOutOfBoundsException](mat(2, 3)) // both
  }

  test("Double col-major: row == rows / col == cols / both throw on write") {
    val mat = denseColMajorDouble
    intercept[IndexOutOfBoundsException](mat(2, 0) = 99.0)
    intercept[IndexOutOfBoundsException](mat(0, 3) = 99.0)
    intercept[IndexOutOfBoundsException](mat(2, 3) = 99.0)
  }

  test("Double col-major: last valid corner (rows - 1, cols - 1) still reads and writes correctly") {
    val mat = denseColMajorDouble
    assertEqualsDouble(mat(1, 2), 6.0, 0.0000001) // guard against over-correction
    mat(1, 2) = 42.0
    assertEqualsDouble(mat(1, 2), 42.0, 0.0000001)
  }

  // ─── Double, dense row-major ───────────────────────────────────────────────
  // Same logical values, but stored row-major (rowStride = cols, colStride = 1) via the raw `Layout` constructor,
  // mirroring `denseRowMajor2x3` in test/src-jvm/intmatrix.test.scala. Row 0 = [1,2,3], row 1 = [4,5,6]. Here it's
  // `col == cols` at row 0 that lands *inside* the backing array (index 3 of 6) and silently aliases element (1, 0)
  // pre-fix, rather than throwing.

  private def denseRowMajorDouble: Matrix[Double] =
    Matrix[Double](Array.tabulate(6)(_.toDouble + 1), Layout(2, 3, 3, 1, 0, 6))

  test("Double row-major: row == rows / col == cols / both throw on read") {
    val mat = denseRowMajorDouble
    intercept[IndexOutOfBoundsException](mat(2, 0))
    intercept[IndexOutOfBoundsException](mat(0, 3)) // lands on raw(3), aliasing element (1, 0) pre-fix
    intercept[IndexOutOfBoundsException](mat(2, 3))
  }

  test("Double row-major: row == rows / col == cols / both throw on write") {
    val mat = denseRowMajorDouble
    intercept[IndexOutOfBoundsException](mat(2, 0) = 99.0)
    intercept[IndexOutOfBoundsException](mat(0, 3) = 99.0)
    intercept[IndexOutOfBoundsException](mat(2, 3) = 99.0)
  }

  test("Double row-major: last valid corner (rows - 1, cols - 1) still reads and writes correctly") {
    val mat = denseRowMajorDouble
    assertEqualsDouble(mat(1, 2), 6.0, 0.0000001)
    mat(1, 2) = 42.0
    assertEqualsDouble(mat(1, 2), 42.0, 0.0000001)
  }

  // ─── Double, strided view via `submatrix` ──────────────────────────────────
  // A 5x5 parent (values 1..25, col-major) sliced to a 2x3 zero-copy view at (1,1). The view's `raw` is the whole
  // 25-element parent array (raw.length == 25 >> numel == 6), so unlike the dense cases above, *every* one-past-the
  // end probe below lands inside the backing array and would silently succeed pre-fix — not just coincidentally
  // caught by the array's own bounds check. This is the sharpest demonstration of the bug: indexing one past a
  // submatrix view's own shape reads (or corrupts) a cell that legitimately belongs to the parent matrix.

  private def stridedView: Matrix[Double] =
    val parent = Matrix[Double](Array.tabulate(25)(_.toDouble + 1), 5, 5)
    parent.submatrix(1 to 2, 1 to 3) // rows=2, cols=3, offset=6, rowStride=1, colStride=5, raw.length=25
  end stridedView

  test("Double strided view: submatrix has the expected shape and corner values") {
    val sub = stridedView
    assertEquals(sub.rows, 2)
    assertEquals(sub.cols, 3)
    assertEqualsDouble(sub(0, 0), 7.0, 0.0000001) // parent(1, 1)
    assertEqualsDouble(sub(1, 2), 18.0, 0.0000001) // parent(2, 3), the last valid corner
  }

  test("Double strided view: row == rows / col == cols / both throw on read, even though raw.length > numel") {
    val sub = stridedView
    intercept[IndexOutOfBoundsException](sub(2, 0)) // linear index 8 of 25 — inside the backing array
    intercept[IndexOutOfBoundsException](sub(0, 3)) // linear index 21 of 25 — inside the backing array
    intercept[IndexOutOfBoundsException](sub(2, 3)) // linear index 23 of 25 — inside the backing array
  }

  test("Double strided view: row == rows / col == cols / both throw on write") {
    val sub = stridedView
    intercept[IndexOutOfBoundsException](sub(2, 0) = -1.0)
    intercept[IndexOutOfBoundsException](sub(0, 3) = -1.0)
    intercept[IndexOutOfBoundsException](sub(2, 3) = -1.0)
  }

  test("Double strided view: last valid corner (rows - 1, cols - 1) still reads and writes correctly") {
    val sub = stridedView
    sub(1, 2) = 42.0
    assertEqualsDouble(sub(1, 2), 42.0, 0.0000001)
  }

  // ─── Float ──────────────────────────────────────────────────────────────── (indexCheckMatInFloat overload)

  private def denseColMajorFloat: Matrix[Float] = Matrix[Float](Array.tabulate(6)(_.toFloat + 1), 2, 3)

  test("Float: row == rows / col == cols / both throw on read") {
    val mat = denseColMajorFloat
    intercept[IndexOutOfBoundsException](mat(2, 0))
    intercept[IndexOutOfBoundsException](mat(0, 3))
    intercept[IndexOutOfBoundsException](mat(2, 3))
  }

  test("Float: row == rows / col == cols / both throw on write") {
    val mat = denseColMajorFloat
    intercept[IndexOutOfBoundsException](mat(2, 0) = 99.0f)
    intercept[IndexOutOfBoundsException](mat(0, 3) = 99.0f)
    intercept[IndexOutOfBoundsException](mat(2, 3) = 99.0f)
  }

  test("Float: last valid corner (rows - 1, cols - 1) still reads and writes correctly") {
    val mat = denseColMajorFloat
    assertEqualsDouble(mat(1, 2).toDouble, 6.0, 0.0000001)
    mat(1, 2) = 42.5f
    assertEqualsDouble(mat(1, 2).toDouble, 42.5, 0.0000001)
  }

  // ─── Int ──────────────────────────────────────────────────────────────────── (indexCheckMatInInt overload)

  private def denseColMajorInt: Matrix[Int] = Matrix[Int](Array.tabulate(6)(_ + 1), 2, 3)

  test("Int: row == rows / col == cols / both throw on read") {
    val mat = denseColMajorInt
    intercept[IndexOutOfBoundsException](mat(2, 0))
    intercept[IndexOutOfBoundsException](mat(0, 3))
    intercept[IndexOutOfBoundsException](mat(2, 3))
  }

  test("Int: row == rows / col == cols / both throw on write") {
    val mat = denseColMajorInt
    intercept[IndexOutOfBoundsException](mat(2, 0) = 99)
    intercept[IndexOutOfBoundsException](mat(0, 3) = 99)
    intercept[IndexOutOfBoundsException](mat(2, 3) = 99)
  }

  test("Int: last valid corner (rows - 1, cols - 1) still reads and writes correctly") {
    val mat = denseColMajorInt
    assertEquals(mat(1, 2), 6)
    mat(1, 2) = 42
    assertEquals(mat(1, 2), 42)
  }

  // ─── Long ─────────────────────────────────────────────────────────────────── (indexCheckMatInLong overload)

  private def denseColMajorLong: Matrix[Long] = Matrix[Long](Array.tabulate(6)(i => (i + 1).toLong), 2, 3)

  test("Long: row == rows / col == cols / both throw on read") {
    val mat = denseColMajorLong
    intercept[IndexOutOfBoundsException](mat(2, 0))
    intercept[IndexOutOfBoundsException](mat(0, 3))
    intercept[IndexOutOfBoundsException](mat(2, 3))
  }

  test("Long: row == rows / col == cols / both throw on write") {
    val mat = denseColMajorLong
    intercept[IndexOutOfBoundsException](mat(2, 0) = 99L)
    intercept[IndexOutOfBoundsException](mat(0, 3) = 99L)
    intercept[IndexOutOfBoundsException](mat(2, 3) = 99L)
  }

  test("Long: last valid corner (rows - 1, cols - 1) still reads and writes correctly") {
    val mat = denseColMajorLong
    assertEquals(mat(1, 2), 6L)
    mat(1, 2) = 42L
    assertEquals(mat(1, 2), 42L)
  }

  // ─── Boolean ────────────────────────────────────────────────────────────── (generic `apply(Matrix[?], RowCol)` arm)
  // Boolean has no `@targetName` overload in `indexCheckMat`, so `Matrix[Boolean]` is what actually exercises the
  // generic arm — the one other than Double/Float/Int/Long that also had the `<=` bug.

  private def denseColMajorBoolean: Matrix[Boolean] =
    Matrix[Boolean](Array[Boolean](true, false, false, true, true, false), 2, 3)

  test("Boolean: row == rows / col == cols / both throw on read") {
    val mat = denseColMajorBoolean
    intercept[IndexOutOfBoundsException](mat(2, 0))
    intercept[IndexOutOfBoundsException](mat(0, 3))
    intercept[IndexOutOfBoundsException](mat(2, 3))
  }

  test("Boolean: row == rows / col == cols / both throw on write") {
    val mat = denseColMajorBoolean
    intercept[IndexOutOfBoundsException](mat(2, 0) = true)
    intercept[IndexOutOfBoundsException](mat(0, 3) = true)
    intercept[IndexOutOfBoundsException](mat(2, 3) = true)
  }

  test("Boolean: last valid corner (rows - 1, cols - 1) still reads and writes correctly") {
    val mat = denseColMajorBoolean
    assertEquals(mat(1, 2), false)
    mat(1, 2) = true
    assertEquals(mat(1, 2), true)
  }

end IndexBoundsSuite
