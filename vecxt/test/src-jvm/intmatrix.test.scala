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

  // ---------------------------------------------------------------------------------------------------------------
  //
  // The fixture below is the *same logical matrix* as the dense row-major one used above ([[1,2,3],[4,5,6]]), but
  // laid out with rowStride 1 and colStride 3 over a length-9 array: column j occupies raw(3j) and raw(3j+1), with
  // raw(3j+2) as padding. dataLength (9) != numel (6), so hasSimpleContiguousMemoryLayout is false and the new
  // elementwise foreach2D path is what runs. Note a dense *row-major* matrix would not do — that is contiguous, so
  // it takes the fast path, which is exactly what the block above already covers.
  //
  // Each case asserts the literal expected result *and* that it agrees with the dense fast path on the same logical
  // input. The second assertion is the one that matters: the two branches must not disagree.
  // ---------------------------------------------------------------------------------------------------------------

  private def paddedColMajor2x3 =
    Matrix[Int](Array[Int](1, 4, 99, 2, 5, 99, 3, 6, 99), 2, 3, 1, 3, 0)

  private def denseEquivalent2x3 = denseRowMajor2x3(Array[Int](1, 2, 3, 4, 5, 6))

  test("the padded fixture is genuinely non-contiguous, and logically equals the dense one") {
    val m = paddedColMajor2x3
    assert(!m.hasSimpleContiguousMemoryLayout, s"fixture must exercise the elementwise path, got ${m.layoutString}")
    assertEquals(m.shape, denseEquivalent2x3.shape)
    for
      row <- 0 until m.rows
      col <- 0 until m.cols
    do assertEquals(m(row, col), denseEquivalent2x3(row, col), s"at ($row, $col)")
    end for
  }

  test(">=(scalar) on a non-contiguous matrix") {
    val m = paddedColMajor2x3
    val expected = Matrix.fromRows[Boolean](
      Array[Boolean](false, false, true),
      Array[Boolean](true, true, true)
    )
    assertBooleanMatrixEqualsLogical(m.>=(3), expected)
    assertBooleanMatrixEqualsLogical(m.>=(3), denseEquivalent2x3.>=(3))
  }

  test(">(scalar) on a non-contiguous matrix") {
    val m = paddedColMajor2x3
    val expected = Matrix.fromRows[Boolean](
      Array[Boolean](false, false, false),
      Array[Boolean](true, true, true)
    )
    assertBooleanMatrixEqualsLogical(m.>(3), expected)
    assertBooleanMatrixEqualsLogical(m.>(3), denseEquivalent2x3.>(3))
  }

  test("<=(scalar) on a non-contiguous matrix") {
    val m = paddedColMajor2x3
    val expected = Matrix.fromRows[Boolean](
      Array[Boolean](true, true, true),
      Array[Boolean](false, false, false)
    )
    assertBooleanMatrixEqualsLogical(m.<=(3), expected)
    assertBooleanMatrixEqualsLogical(m.<=(3), denseEquivalent2x3.<=(3))
  }

  test("<(scalar) on a non-contiguous matrix") {
    val m = paddedColMajor2x3
    val expected = Matrix.fromRows[Boolean](
      Array[Boolean](true, true, false),
      Array[Boolean](false, false, false)
    )
    assertBooleanMatrixEqualsLogical(m.<(3), expected)
    assertBooleanMatrixEqualsLogical(m.<(3), denseEquivalent2x3.<(3))
  }

  test("/(scalar: Double) on a non-contiguous matrix") {
    val m = paddedColMajor2x3
    val expected = Matrix.fromRows[Double](
      Array[Double](0.5, 1.0, 1.5),
      Array[Double](2.0, 2.5, 3.0)
    )
    assertDoubleMatrixEqualsLogical(m./(2.0), expected)
    assertDoubleMatrixEqualsLogical(m./(2.0), denseEquivalent2x3./(2.0))
  }

  test("/(scalar: Float) on a non-contiguous matrix") {
    val m = paddedColMajor2x3
    val expected = Matrix.fromRows[Float](
      Array[Float](0.5f, 1.0f, 1.5f),
      Array[Float](2.0f, 2.5f, 3.0f)
    )
    assertFloatMatrixEqualsLogical(m./(2.0f), expected)
    assertFloatMatrixEqualsLogical(m./(2.0f), denseEquivalent2x3./(2.0f))
  }

  // Division truncation is a real hazard for an Int source: `/` on a Matrix[Int] widens to Double/Float rather than
  // doing integer division, and the elementwise branch must widen the same way the SIMD fast path does. A divisor
  // that does not divide evenly is what distinguishes the two.
  test("/(scalar) on a non-contiguous matrix widens rather than truncating") {
    val m = paddedColMajor2x3
    val expected = Matrix.fromRows[Double](
      Array[Double](0.25, 0.5, 0.75),
      Array[Double](1.0, 1.25, 1.5)
    )
    assertDoubleMatrixEqualsLogical(m./(4.0), expected)
    assertDoubleMatrixEqualsLogical(m./(4.0), denseEquivalent2x3./(4.0))
  }

  // An offset view is the other way to be non-contiguous, and it is the case where a wrong `linearIndex` would read
  // the parent's elements instead of the view's. Rows 0..1, cols 1..2 of a 3x3 parent.
  test("comparison and division agree with the dense equivalent on an offset submatrix view") {
    val parent = denseRowMajor2x3(Array[Int](1, 2, 3, 4, 5, 6))
    val view = parent.submatrix(0 to 1, 1 to 2) // [[2,3],[5,6]]
    assert(!view.hasSimpleContiguousMemoryLayout, s"expected a non-contiguous view, got ${view.layoutString}")

    val dense = Matrix.fromRows[Int](Array[Int](2, 3), Array[Int](5, 6))
    assertBooleanMatrixEqualsLogical(view.>=(3), dense.>=(3))
    assertBooleanMatrixEqualsLogical(view.<(5), dense.<(5))
    assertDoubleMatrixEqualsLogical(view./(2.0), dense./(2.0))
  }

end IntMatrixJvmSuite
