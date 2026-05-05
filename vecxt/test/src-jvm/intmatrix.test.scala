package vecxt

import munit.FunSuite

import all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

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

end IntMatrixJvmSuite
