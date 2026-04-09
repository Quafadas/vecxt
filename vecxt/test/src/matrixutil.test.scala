package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

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

end MatrixUtilSuite
