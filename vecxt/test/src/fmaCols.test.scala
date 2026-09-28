package vecxt

import all.*

/** `fmaCols` is `out(i, j) = m(i, j) * multiply(j) + add(j)`. The JVM has a SIMD path for `rowStride == 1` and falls
  * back to the shared scalar loop otherwise; JS and Native always use the loop. Every case is checked against a naive
  * reference read through `m(i, j)`, so the layouts that pick different branches are held to the same answer.
  */
class FmaColsSuite extends munit.FunSuite:

  private def reference(m: Matrix[Double], multiply: Array[Double], add: Array[Double]): Matrix[Double] =
    val out = Array.ofDim[Double](m.rows * m.cols)
    for j <- 0 until m.cols; i <- 0 until m.rows do out(i + j * m.rows) = m(i, j) * multiply(j) + add(j)
    end for
    Matrix[Double](out, m.rows, m.cols)
  end reference

  private def assertDenseColMajor(m: Matrix[Double])(using munit.Location): Unit =
    assertEquals((m.rowStride, m.colStride, m.offset), (1, m.rows, 0))

  private def check(m: Matrix[Double], multiply: Array[Double], add: Array[Double])(using munit.Location): Unit =
    val out = m.fmaCols(multiply, add)
    assertDenseColMajor(out)
    assertMatrixEquals(out, reference(m, multiply, add))
  end check

  test("small hand computed example"):
    val m = Matrix.fromRows[Double](Array(1.0, 2.0), Array(3.0, 4.0))
    val out = m.fmaCols(Array(10.0, -1.0), Array(0.5, 100.0))
    assertMatrixEquals(out, Matrix.fromRows[Double](Array(10.5, 98.0), Array(30.5, 96.0)))

  test("dense column-major, row count not a multiple of common SIMD widths"):
    val m = Matrix[Double](Array.tabulate(37 * 3)(i => i * 0.25 - 3.0), 37, 3)
    check(m, Array(2.0, -0.5, 0.0), Array(1.0, 7.0, -3.0))

  test("dense column-major, large enough for many SIMD iterations per column"):
    val m = Matrix[Double](Array.tabulate(1000 * 4)(i => math.sin(i.toDouble)), 1000, 4)
    check(m, Array(1.5, -2.0, 3.25, 1e6), Array(-1.0, 0.0, 2.0, 1e-3))

  test("row-major input"):
    val m = Matrix[Double](Array.tabulate(5 * 3)(_.toDouble), 5, 3, 3, 1, 0)
    assertEquals(m.rowStride, 3)
    check(m, Array(1.0, 2.0, 3.0), Array(-1.0, -2.0, -3.0))

  test("padded column stride (rowStride 1, not contiguous)"):
    // rows=2, cols=2, colStride=3: columns are raw(0..1) and raw(3..4), raw(2)/raw(5) are padding
    val m = Matrix[Double](Array(1.0, 2.0, 99.0, 3.0, 4.0, 99.0), 2, 2, 1, 3, 0)
    assert(!m.hasSimpleContiguousMemoryLayout)
    val out = m.fmaCols(Array(2.0, 3.0), Array(1.0, 1.0))
    assertMatrixEquals(out, Matrix.fromRows[Double](Array(3.0, 10.0), Array(5.0, 13.0)))

  test("submatrix view with offset"):
    val base = Matrix[Double](Array.tabulate(20 * 6)(_.toDouble), 20, 6)
    // rows 3..17 of columns 2..4 of a 20 x 6 column-major matrix
    val view = Matrix[Double](base.raw, 15, 3, 1, 20, 3 + 2 * 20)
    assertEquals(view(0, 0), 43.0)
    check(view, Array(-1.0, 0.5, 2.0), Array(10.0, 20.0, 30.0))

  test("input is not modified"):
    val raw = Array.tabulate(9 * 2)(_.toDouble)
    val m = Matrix[Double](raw.clone, 9, 2)
    m.fmaCols(Array(3.0, 4.0), Array(1.0, 2.0))
    assertVecEquals(m.raw, raw)

  test("empty matrices"):
    val noRows = Matrix[Double](Array.empty[Double], 0, 3)
    val out = noRows.fmaCols(Array(1.0, 2.0, 3.0), Array(1.0, 2.0, 3.0))
    assertEquals((out.rows, out.cols), (0, 3))
    val noCols = Matrix[Double](Array.empty[Double], 4, 0)
    val out2 = noCols.fmaCols(Array.empty[Double], Array.empty[Double])
    assertEquals((out2.rows, out2.cols), (4, 0))

  test("wrong vector lengths throw"):
    val m = Matrix[Double](Array.fill(6)(1.0), 3, 2)
    intercept[IllegalArgumentException](m.fmaCols(Array(1.0), Array(1.0, 2.0)))
    intercept[IllegalArgumentException](m.fmaCols(Array(1.0, 2.0), Array(1.0, 2.0, 3.0)))
    val rowMajor = Matrix[Double](Array.fill(6)(1.0), 3, 2, 2, 1, 0)
    intercept[IllegalArgumentException](rowMajor.fmaCols(Array(1.0), Array(1.0, 2.0)))

end FmaColsSuite
