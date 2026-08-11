package vecxt

import munit.FunSuite

import all.*

class MatrixUtilSuite extends FunSuite:

  // ── transpose / .T ────────────────────────────────────────────────────────

  test("transpose swaps rows and cols"):
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), (2, 3))
    val mt = m.T
    assertEquals(mt.rows, 3)
    assertEquals(mt.cols, 2)

  test("transpose shares data (zero-copy)"):
    val raw = Array(1.0, 2.0, 3.0, 4.0)
    val m = Matrix[Double](raw, (2, 2))
    val mt = m.T
    assert(mt.raw eq m.raw)

  test("transpose is involution"):
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), (2, 3))
    val mt = m.T
    assertEquals(mt.T.rows, m.rows)
    assertEquals(mt.T.cols, m.cols)
    assertVecEquals[Double](mt.T.raw, m.raw)

  // ── diag (extract main diagonal) ──────────────────────────────────────────

  test("diag of square matrix"):
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0), (2, 2))
    val d = m.diag
    assertVecEquals[Double](d, Array(1.0, 4.0))

  test("diag of 3x3 identity"):
    val m = Matrix.eye[Double](3)
    val d = m.diag
    assertVecEquals[Double](d, Array(1.0, 1.0, 1.0))

  test("diag of non-square (more rows) takes min dim"):
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), (3, 2))
    val d = m.diag
    assertEquals(d.length, 2)

  // ── mapRowsInPlace ────────────────────────────────────────────────────────

  test("mapRowsInPlace doubles every row"):
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0), (2, 2))
    m.mapRowsInPlace(row => row.map(_ * 2))
    assertVecEquals[Double](m.raw, Array(2.0, 4.0, 6.0, 8.0))

  // ── mapRows ───────────────────────────────────────────────────────────────

  test("mapRows produces new matrix without mutating original"):
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0), (2, 2))
    val m2 = m.mapRows(row => row.map(_ + 10.0))
    assertVecEquals[Double](m2.raw, Array(11.0, 12.0, 13.0, 14.0))
    // original unchanged
    assertVecEquals[Double](m.raw, Array(1.0, 2.0, 3.0, 4.0))

  // ── mapRowsToScalar ───────────────────────────────────────────────────────

  test("mapRowsToScalar computes row sum"):
    // col-major: Array(1,2,3,4) in 2x2 → row(0)=[1,3], row(1)=[2,4]
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0), (2, 2))
    val sums = m.mapRowsToScalar(row => row.sum)
    assertEquals(sums.rows, 2)
    assertEquals(sums.cols, 1)
    assertVecEquals[Double](sums.raw, Array(4.0, 6.0))

  // ── mapColsInPlace ────────────────────────────────────────────────────────

  test("mapColsInPlace negates every column"):
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0), (2, 2))
    m.mapColsInPlace(col => col.map(x => -x))
    assertVecEquals[Double](m.raw, Array(-1.0, -2.0, -3.0, -4.0))

  // ── mapCols ───────────────────────────────────────────────────────────────

  test("mapCols produces new matrix"):
    val m = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0), (2, 2))
    val m2 = m.mapCols(col => col.map(_ * 3.0))
    assertVecEquals[Double](m2.raw, Array(3.0, 6.0, 9.0, 12.0))
    assertVecEquals[Double](m.raw, Array(1.0, 2.0, 3.0, 4.0))

  // ── mapColsToScalar ───────────────────────────────────────────────────────

  test("mapColsToScalar computes column max"):
    val m = Matrix[Double](Array(1.0, 3.0, 2.0, 4.0), (2, 2))
    val maxes = m.mapColsToScalar(col => col.max)
    assertEquals(maxes.rows, 1)
    assertEquals(maxes.cols, 2)
    assertVecEquals[Double](maxes.raw, Array(3.0, 4.0))

  // ── row / col across layouts ──────────────────────────────────────────────
  // `row` takes an arraycopy path when colStride == 1 and a strided walk otherwise; `col` mirrors that on
  // rowStride == 1. Each fixture below is the same logical matrix [[1,2,3],[4,5,6]] in a different layout, so both
  // branches of both methods are exercised, including on a padded/offset view where the backing array is larger than
  // numel and the old `m((i, j))`-per-element version had to re-derive the index every step.

  private def rowMajor2x3 = Matrix[Double](Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2, 3, 3, 1, 0)
  private def colMajor2x3 = Matrix[Double](Array(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), 2, 3, 1, 2, 0)
  // rowStride 1, colStride 3 over a length-9 array: col j lives at raw(3j), raw(3j+1); raw(3j+2) is padding.
  private def paddedColMajor2x3 =
    Matrix[Double](Array(1.0, 4.0, 99.0, 2.0, 5.0, 99.0, 3.0, 6.0, 99.0), 2, 3, 1, 3, 0)

  test("row returns the same values for row-major, col-major and padded layouts"):
    for m <- List(rowMajor2x3, colMajor2x3, paddedColMajor2x3) do
      assertVecEquals[Double](m.row(0), Array(1.0, 2.0, 3.0))
      assertVecEquals[Double](m.row(1), Array(4.0, 5.0, 6.0))
    end for

  test("col returns the same values for row-major, col-major and padded layouts"):
    for m <- List(rowMajor2x3, colMajor2x3, paddedColMajor2x3) do
      assertVecEquals[Double](m.col(0), Array(1.0, 4.0))
      assertVecEquals[Double](m.col(1), Array(2.0, 5.0))
      assertVecEquals[Double](m.col(2), Array(3.0, 6.0))
    end for

  test("row/col copy rather than view — mutating the result leaves the matrix alone"):
    val m = colMajor2x3
    val r = m.row(0)
    r(0) = -100.0
    assertEqualsDouble(m(0, 0), 1.0, 1e-9)
    val c = m.col(0)
    c(0) = -100.0
    assertEqualsDouble(m(0, 0), 1.0, 1e-9)

  test("row/col reject out-of-range indices"):
    val m = colMajor2x3
    intercept[IndexOutOfBoundsException](m.row(2))
    intercept[IndexOutOfBoundsException](m.col(3))

  // ── mapRows / mapCols on non-column-major sources ─────────────────────────

  test("mapRowsInPlace writes back correctly on a row-major matrix"):
    val m = rowMajor2x3
    m.mapRowsInPlace(row => row.map(_ * 2))
    assertVecEquals[Double](m.row(0), Array(2.0, 4.0, 6.0))
    assertVecEquals[Double](m.row(1), Array(8.0, 10.0, 12.0))

  test("mapColsInPlace writes back correctly on a padded (non-contiguous) matrix"):
    val m = paddedColMajor2x3
    m.mapColsInPlace(col => col.map(_ + 1.0))
    assertVecEquals[Double](m.col(0), Array(2.0, 5.0))
    assertVecEquals[Double](m.col(2), Array(4.0, 7.0))
    // padding untouched
    assertEqualsDouble(m.raw(2), 99.0, 1e-9)

  test("mapRows reads a row-major source correctly"):
    val mapped = rowMajor2x3.mapRows[Double](row => row.map(_ + 10.0))
    assertVecEquals[Double](mapped.row(0), Array(11.0, 12.0, 13.0))
    assertVecEquals[Double](mapped.row(1), Array(14.0, 15.0, 16.0))

  test("mapRows rejects a function that changes the row length"):
    intercept[MatrixDimensionMismatch](colMajor2x3.mapRows[Double](row => row.take(2)))

  test("mapCols rejects a function that changes the column length"):
    intercept[MatrixDimensionMismatch](colMajor2x3.mapCols[Double](col => col ++ col))

  // ── horzcat ───────────────────────────────────────────────────────────────
  // horzcat has two branches per operand: an arraycopy fast path when the operand is dense column-major *and*
  // hasSimpleContiguousMemoryLayout, and a general foreach2D walk otherwise. Since the branch is chosen per operand,
  // the combinations below cover fast/fast, fast/slow, slow/fast and slow/slow.

  test("horzcat of two dense column-major matrices (fast/fast)"):
    val a = Matrix.fromRows[Double](Array(1.0, 2.0), Array(3.0, 4.0))
    val b = Matrix.fromRows[Double](Array(5.0, 6.0, 7.0), Array(8.0, 9.0, 10.0))
    assert(a.hasSimpleContiguousMemoryLayout && a.isDenseColMajor)

    val r = a.horzcat(b)
    assertEquals(r.rows, 2)
    assertEquals(r.cols, 5)
    assertMatrixEquals(
      r,
      Matrix.fromRows[Double](Array(1.0, 2.0, 5.0, 6.0, 7.0), Array(3.0, 4.0, 8.0, 9.0, 10.0))
    )

  test("horzcat with a row-major left operand (slow/fast)"):
    val r = rowMajor2x3.horzcat(Matrix.fromRows[Double](Array(7.0), Array(8.0)))
    assertMatrixEquals(
      r,
      Matrix.fromRows[Double](Array(1.0, 2.0, 3.0, 7.0), Array(4.0, 5.0, 6.0, 8.0))
    )

  test("horzcat with a padded, non-contiguous right operand (fast/slow)"):
    val r = Matrix.fromRows[Double](Array(0.0), Array(-1.0)).horzcat(paddedColMajor2x3)
    assertMatrixEquals(
      r,
      Matrix.fromRows[Double](Array(0.0, 1.0, 2.0, 3.0), Array(-1.0, 4.0, 5.0, 6.0))
    )

  test("horzcat of two non-column-major operands (slow/slow)"):
    val r = rowMajor2x3.horzcat(paddedColMajor2x3)
    assertMatrixEquals(
      r,
      Matrix.fromRows[Double](Array(1.0, 2.0, 3.0, 1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0, 4.0, 5.0, 6.0))
    )

  test("horzcat result is dense column-major and independent of its operands"):
    val a = Matrix.fromRows[Double](Array(1.0, 2.0), Array(3.0, 4.0))
    val b = Matrix.fromRows[Double](Array(5.0), Array(6.0))
    val r = a.horzcat(b)
    assert(r.isDenseColMajor)
    assert(r.hasSimpleContiguousMemoryLayout)
    r(0, 0) = -99.0
    assertEqualsDouble(a(0, 0), 1.0, 1e-9)

  test("horzcat rejects mismatched row counts"):
    val a = Matrix.fromRows[Double](Array(1.0, 2.0), Array(3.0, 4.0))
    val b = Matrix.fromRows[Double](Array(5.0, 6.0))
    intercept[MatrixDimensionMismatch](a.horzcat(b))

  // ── vertcat ───────────────────────────────────────────────────────────────

  test("vertcat of two dense column-major matrices (fast/fast)"):
    val a = Matrix.fromRows[Double](Array(1.0, 2.0), Array(3.0, 4.0))
    val b = Matrix.fromRows[Double](Array(5.0, 6.0), Array(7.0, 8.0), Array(9.0, 10.0))

    val r = a.vertcat(b)
    assertEquals(r.rows, 5)
    assertEquals(r.cols, 2)
    assertMatrixEquals(
      r,
      Matrix.fromRows[Double](
        Array(1.0, 2.0),
        Array(3.0, 4.0),
        Array(5.0, 6.0),
        Array(7.0, 8.0),
        Array(9.0, 10.0)
      )
    )

  test("vertcat with a row-major top operand (slow/fast)"):
    val r = rowMajor2x3.vertcat(Matrix.fromRows[Double](Array(7.0, 8.0, 9.0)))
    assertMatrixEquals(
      r,
      Matrix.fromRows[Double](Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    )

  test("vertcat with a padded, non-contiguous bottom operand (fast/slow)"):
    val r = Matrix.fromRows[Double](Array(0.0, -1.0, -2.0)).vertcat(paddedColMajor2x3)
    assertMatrixEquals(
      r,
      Matrix.fromRows[Double](Array(0.0, -1.0, -2.0), Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0))
    )

  test("vertcat of two non-column-major operands (slow/slow)"):
    val r = rowMajor2x3.vertcat(paddedColMajor2x3)
    assertMatrixEquals(
      r,
      Matrix.fromRows[Double](
        Array(1.0, 2.0, 3.0),
        Array(4.0, 5.0, 6.0),
        Array(1.0, 2.0, 3.0),
        Array(4.0, 5.0, 6.0)
      )
    )

  test("vertcat result is dense column-major and independent of its operands"):
    val a = Matrix.fromRows[Double](Array(1.0, 2.0), Array(3.0, 4.0))
    val b = Matrix.fromRows[Double](Array(5.0, 6.0))
    val r = a.vertcat(b)
    assert(r.isDenseColMajor)
    assert(r.hasSimpleContiguousMemoryLayout)
    r(0, 0) = -99.0
    assertEqualsDouble(a(0, 0), 1.0, 1e-9)

  test("vertcat rejects mismatched column counts"):
    val a = Matrix.fromRows[Double](Array(1.0, 2.0), Array(3.0, 4.0))
    val b = Matrix.fromRows[Double](Array(5.0, 6.0, 7.0))
    intercept[MatrixDimensionMismatch](a.vertcat(b))

  // Regression: horzcat used to be `m.raw.appendedAll(m2.raw)` behind an `isDenseColMajor` guard. A submatrix view of
  // the leading columns of a wider parent is dense column-major by stride, but keeps the parent's full backing array —
  // so that concat spliced the parent's trailing columns into the result and silently produced wrong numbers.
  test("horzcat of a leading-column submatrix view does not leak the parent's extra columns"):
    val parent = Matrix.fromRows[Double](Array(1.0, 2.0, 99.0), Array(3.0, 4.0, 99.0))
    val view = parent.submatrix(0 to 1, 0 to 1) // 2x2, dense col-major by stride, but raw.length == 6
    assert(view.isDenseColMajor)
    assert(!view.hasSimpleContiguousMemoryLayout)

    val r = view.horzcat(Matrix.fromRows[Double](Array(5.0), Array(6.0)))
    assertEquals(r.cols, 3)
    assertMatrixEquals(r, Matrix.fromRows[Double](Array(1.0, 2.0, 5.0), Array(3.0, 4.0, 6.0)))

end MatrixUtilSuite
