package vecxt

import munit.FunSuite

import matrix.*
import all.*

class IntMatrixJvmSuite extends FunSuite:

  private def assertIntMatrixEquals(actual: Matrix[Int], expected: Matrix[Int])(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(actual.rows, expected.rows, "rows mismatch")
    assertEquals(actual.cols, expected.cols, "cols mismatch")
    var i = 0
    while i < actual.raw.length do
      assertEquals(actual.raw(i), expected.raw(i), s"mismatch at raw index $i")
      i += 1
    end while
  end assertIntMatrixEquals

  // ── sum ──────────────────────────────────────────────────────────────────

  test("sum(dim=0) reduces each row to a single value"):
    // 3×2 matrix: rows [1,2], [3,4], [5,6]
    val mat = Matrix.fromRows[Int](
      Array[Int](1, 2),
      Array[Int](3, 4),
      Array[Int](5, 6)
    )
    // dim=0 → one value per row → shape (3,1)
    val result = mat.sum(0)
    assertEquals(result.rows, 3)
    assertEquals(result.cols, 1)
    assertIntMatrixEquals(result, Matrix(Array[Int](3, 7, 11), (3, 1)))

  test("sum(dim=1) reduces each column to a single value"):
    val mat = Matrix.fromRows[Int](
      Array[Int](1, 2),
      Array[Int](3, 4),
      Array[Int](5, 6)
    )
    // dim=1 → one value per column → shape (1,2)
    val result = mat.sum(1)
    assertEquals(result.rows, 1)
    assertEquals(result.cols, 2)
    assertIntMatrixEquals(result, Matrix(Array[Int](9, 12), (1, 2)))

  // ── max ──────────────────────────────────────────────────────────────────

  test("max(dim=0) returns row-wise maxima"):
    val mat = Matrix.fromRows[Int](
      Array[Int](1, 5),
      Array[Int](3, 2)
    )
    val result = mat.max(0)
    assertIntMatrixEquals(result, Matrix(Array[Int](5, 3), (2, 1)))

  test("max(dim=1) returns column-wise maxima"):
    val mat = Matrix.fromRows[Int](
      Array[Int](1, 5),
      Array[Int](3, 2)
    )
    val result = mat.max(1)
    assertIntMatrixEquals(result, Matrix(Array[Int](3, 5), (1, 2)))

  // ── min ──────────────────────────────────────────────────────────────────

  test("min(dim=0) returns row-wise minima"):
    val mat = Matrix.fromRows[Int](
      Array[Int](1, 5),
      Array[Int](3, 2)
    )
    val result = mat.min(0)
    assertIntMatrixEquals(result, Matrix(Array[Int](1, 2), (2, 1)))

  test("min(dim=1) returns column-wise minima"):
    val mat = Matrix.fromRows[Int](
      Array[Int](1, 5),
      Array[Int](3, 2)
    )
    val result = mat.min(1)
    assertIntMatrixEquals(result, Matrix(Array[Int](1, 2), (1, 2)))

  // ── product ──────────────────────────────────────────────────────────────

  test("product(dim=1) returns column-wise products"):
    val mat = Matrix.fromRows[Int](
      Array[Int](2, 3),
      Array[Int](4, 5)
    )
    val result = mat.product(1)
    assertIntMatrixEquals(result, Matrix(Array[Int](8, 15), (1, 2)))

  test("product(dim=0) returns row-wise products"):
    val mat = Matrix.fromRows[Int](
      Array[Int](2, 3),
      Array[Int](4, 5)
    )
    val result = mat.product(0)
    assertIntMatrixEquals(result, Matrix(Array[Int](6, 20), (2, 1)))

  // ---------------------------------------------------------------------------------------------------------------
  // Regression: the `hasSimpleContiguousMemoryLayout` fast path for `/`/`>=`/`>`/`<=`/`<` used to wrap its result
  // with `m.shape` (always column-major) instead of `m.layout`. That mislabels a dense *row-major* input — the
  // result array is still in row-major order but gets read back out as column-major — silently transposing the
  // result for any non-square matrix. `Matrix.fromRows` (used elsewhere in this file) stores column-major
  // internally, so it never exercised this path; these build a genuinely row-major layout directly to catch it.
  // Compared logically (via `(row, col)`), not via `.raw`, since `expected` and `actual` may legitimately differ in
  // storage order while agreeing on logical content.
  // ---------------------------------------------------------------------------------------------------------------

  private def assertBooleanMatrixEqualsLogical(actual: Matrix[Boolean], expected: Matrix[Boolean])(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(actual.shape, expected.shape, "matrix shape mismatch")
    for
      row <- 0 until actual.rows
      col <- 0 until actual.cols
    do assertEquals(actual(row, col), expected(row, col), s"at ($row, $col)")
    end for
  end assertBooleanMatrixEqualsLogical

  private def assertDoubleMatrixEqualsLogical(actual: Matrix[Double], expected: Matrix[Double])(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(actual.shape, expected.shape, "matrix shape mismatch")
    for
      row <- 0 until actual.rows
      col <- 0 until actual.cols
    do assertEqualsDouble(actual(row, col), expected(row, col), 1e-9, s"at ($row, $col)")
    end for
  end assertDoubleMatrixEqualsLogical

  private def assertFloatMatrixEqualsLogical(actual: Matrix[Float], expected: Matrix[Float])(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(actual.shape, expected.shape, "matrix shape mismatch")
    for
      row <- 0 until actual.rows
      col <- 0 until actual.cols
    do assertEqualsDouble(actual(row, col).toDouble, expected(row, col).toDouble, 1e-9, s"at ($row, $col)")
    end for
  end assertFloatMatrixEqualsLogical

  private def denseRowMajor2x3(values: Array[Int]): Matrix[Int] =
    Matrix[Int](values, Layout(2, 3, 3, 1, 0, 6))

  test(">=(scalar) on a dense row-major view does not transpose the result") {
    val m = denseRowMajor2x3(Array[Int](1, 2, 3, 4, 5, 6)) // row0=[1,2,3], row1=[4,5,6]
    val expected = Matrix.fromRows[Boolean](
      Array[Boolean](false, false, true),
      Array[Boolean](true, true, true)
    )
    assertBooleanMatrixEqualsLogical(m.>=(3), expected)
  }

  test(">(scalar) on a dense row-major view does not transpose the result") {
    val m = denseRowMajor2x3(Array[Int](1, 2, 3, 4, 5, 6))
    val expected = Matrix.fromRows[Boolean](
      Array[Boolean](false, false, false),
      Array[Boolean](true, true, true)
    )
    assertBooleanMatrixEqualsLogical(m.>(3), expected)
  }

  test("<=(scalar) on a dense row-major view does not transpose the result") {
    val m = denseRowMajor2x3(Array[Int](1, 2, 3, 4, 5, 6))
    val expected = Matrix.fromRows[Boolean](
      Array[Boolean](true, true, true),
      Array[Boolean](false, false, false)
    )
    assertBooleanMatrixEqualsLogical(m.<=(3), expected)
  }

  test("<(scalar) on a dense row-major view does not transpose the result") {
    val m = denseRowMajor2x3(Array[Int](1, 2, 3, 4, 5, 6))
    val expected = Matrix.fromRows[Boolean](
      Array[Boolean](true, true, false),
      Array[Boolean](false, false, false)
    )
    assertBooleanMatrixEqualsLogical(m.<(3), expected)
  }

  test("/(scalar: Double) on a dense row-major view does not transpose the result") {
    val m = denseRowMajor2x3(Array[Int](1, 2, 3, 4, 5, 6))
    val expected = Matrix.fromRows[Double](
      Array[Double](0.5, 1.0, 1.5),
      Array[Double](2.0, 2.5, 3.0)
    )
    assertDoubleMatrixEqualsLogical(m./(2.0), expected)
  }

  test("/(scalar: Float) on a dense row-major view does not transpose the result") {
    val m = denseRowMajor2x3(Array[Int](1, 2, 3, 4, 5, 6))
    val expected = Matrix.fromRows[Float](
      Array[Float](0.5f, 1.0f, 1.5f),
      Array[Float](2.0f, 2.5f, 3.0f)
    )
    assertFloatMatrixEqualsLogical(m./(2.0f), expected)
  }

end IntMatrixJvmSuite
