package vecxt

import all.*
import munit.FunSuite

class MatrixBooleanSuite extends FunSuite:

  test("zeros") {
    val mat = Matrix.zeros[Boolean]((2, 2))
    assertVecEquals[Boolean](mat.raw, Array[Boolean](false, false, false, false))
  }

  test("eye") {
    val mat = Matrix.eye[Boolean](2)
    assertVecEquals[Boolean](mat.raw, Array[Boolean](true, false, false, true))
  }

  test("ones") {
    val mat = Matrix.ones[Double]((2, 2))
    assertVecEquals[Double](mat.raw, Array[Double](1.0, 1.0, 1.0, 1.0))
  }

  test("Matrix creation") {
    val array = Array[Boolean](true, false, true, false)
    val matrix = Matrix[Boolean](array, (2, 2))
    assertEquals(matrix.raw, array)
  }

  test("raw array retrieval") {
    val mat = Matrix[Boolean](Array[Boolean](true, false, true, false), (2, 2))
    assertVecEquals[Boolean](mat.raw, Array[Boolean](true, false, true, false))
  }

  test("slice syntax") {
    val mat = Matrix.fromRows[Boolean](
      Array[Boolean](true, false, true),
      Array[Boolean](false, true, false)
    )

    val b = mat(::, Array[Int](0))
    assertVecEquals[Boolean](Array[Boolean](true, false), b.raw)

  }

  test("logicals") {
    val mat = Matrix.fromRows[Double](
      Array[Double](1.0, 2.0),
      Array[Double](3.0, 4.0)
    )

    val lt = mat < 2.0
    assertVecEquals[Boolean](Array[Boolean](true, false, false, false), lt.raw)

    val gt = mat > 2.0
    assertVecEquals[Boolean](Array[Boolean](false, true, false, true), gt.raw)

    val gte = mat >= 2.0
    assertVecEquals[Boolean](Array[Boolean](false, true, true, true), gte.raw)

    val lte = mat <= 2.0
    assertVecEquals[Boolean](Array[Boolean](true, false, true, false), lte.raw)

  }

  test("elementwise mult double array") {
    val mat = Matrix.fromRows[Double](
      Array[Double](1.0, 2.0, 3.0),
      Array[Double](3.0, 4.0, 5.0)
    )

    val bools = Matrix.fromRows[Boolean](
      Array[Boolean](true, false, true),
      Array[Boolean](false, true, false)
    )

    val calc = mat *:* bools
    val result = Matrix.fromRows[Double](
      Array[Double](1.0, 0.0, 3.0),
      Array[Double](0.0, 4.0, 0.0)
    )
    assertVecEquals[Double](calc.raw, result.raw)
  }

  private def assertLogical(actual: Matrix[Boolean], expectedRows: Array[Boolean]*)(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(actual.rows, expectedRows.length)
    for i <- 0 until actual.rows do
      for j <- 0 until actual.cols do assertEquals(actual(i, j), expectedRows(i)(j), clue = s"at ($i, $j)")
      end for
    end for
  end assertLogical

  // 2 x 2 view on the middle of a 3 x 3 col-major array (rows 1-2, cols 1-2). Non-contiguous: offset 4, colStride 3.
  //   col-major raw: col0 = (T, F, F), col1 = (T, T, F), col2 = (F, T, F)
  //   view:  [ T, T ]
  //          [ F, F ]
  private def strided: Matrix[Boolean] =
    Matrix(Array[Boolean](true, false, false, true, true, false, false, true, false), 2, 2, 1, 3, 4)

  private def rowMajor: Matrix[Boolean] =
    // [ T, F, T ]
    // [ F, F, T ]
    Matrix(Array[Boolean](true, false, true, false, false, true), 2, 3, 3, 1, 0)

  test("allTrue") {
    assert(Matrix.fromRows[Boolean](Array(true, true), Array(true, true)).allTrue)
    assert(!Matrix.fromRows[Boolean](Array(true, true), Array(false, true)).allTrue)
    assert(!rowMajor.allTrue)
    assert(!strided.allTrue)

    val big = Matrix(Array.fill(1025 * 3)(true), 1025, 3)
    assert(big.allTrue)
    big.raw(2000) = false
    assert(!big.allTrue)

    val allTrueView = Matrix(Array(false, true, true, false, true, true), 2, 2, 1, 3, 1)
    assert(allTrueView.allTrue)
  }

  test("any") {
    assert(!Matrix.zeros[Boolean]((3, 3)).any)
    assert(Matrix.eye[Boolean](3).any)
    assert(rowMajor.any)
    assert(strided.any)

    val big = Matrix(Array.fill(1025 * 3)(false), 1025, 3)
    assert(!big.any)
    big.raw(2000) = true
    assert(big.any)

    // Only elements outside the view are true.
    val noneInView = Matrix(Array(true, false, false, true, false, false), 2, 2, 1, 3, 1)
    assert(!noneInView.any)
  }

  test("trues") {
    assertEquals(Matrix.eye[Boolean](4).trues, 4)
    assertEquals(Matrix.zeros[Boolean]((2, 5)).trues, 0)
    assertEquals(rowMajor.trues, 3)
    assertEquals(strided.trues, 2)

    val big = Matrix(Array.fill(1025 * 3)(true), 1025, 3)
    assertEquals(big.trues, 3075)
  }

  test("not - dense") {
    val m = Matrix.fromRows[Boolean](Array(true, false, true), Array(false, true, false))
    val n = m.not
    assertLogical(n, Array(false, true, false), Array(true, false, true))
    assertLogical(m, Array(true, false, true), Array(false, true, false)) // original untouched

    assertLogical(rowMajor.not, Array(false, true, false), Array(true, true, false))
  }

  test("not - strided view") {
    val v = strided
    val n = v.not
    assertLogical(n, Array(false, false), Array(true, true))
    assert(n.isDenseColMajor)
    assertLogical(v, Array(true, true), Array(false, false)) // view untouched
  }

  test("not! - dense") {
    val m = Matrix.fromRows[Boolean](Array(true, false), Array(false, false))
    m.`not!`
    assertLogical(m, Array(false, true), Array(true, true))

    val r = rowMajor
    r.`not!`
    assertLogical(r, Array(false, true, false), Array(true, true, false))
  }

  test("not! - strided view writes through and leaves the rest of the array alone") {
    val v = strided
    v.`not!`
    assertLogical(v, Array(false, false), Array(true, true))
    assertVecEquals[Boolean](v.raw, Array(true, false, false, true, false, true, false, false, true))
  }

  test("not! - aliased (broadcast) view throws") {
    val broadcastCol = Matrix(Array(true, false), 2, 3, 1, 0, 0)
    intercept[UnsupportedLayoutException](broadcastCol.`not!`)
  }

  test("&& and || - dense col-major") {
    val a = Matrix.fromRows[Boolean](Array(true, true, false), Array(false, true, false))
    val b = Matrix.fromRows[Boolean](Array(true, false, false), Array(true, true, true))
    assertLogical(a && b, Array(true, false, false), Array(false, true, false))
    assertLogical(a || b, Array(true, true, false), Array(true, true, true))
  }

  test("&& and || - large dense exercises the SIMD path") {
    val n = 1025
    val a = Matrix(Array.tabulate(n * 2)(_ % 2 == 0), n, 2)
    val b = Matrix(Array.tabulate(n * 2)(_ % 3 == 0), n, 2)
    val and = a && b
    val or = a || b
    for k <- 0 until n * 2 do
      assertEquals(and.raw(k), k % 6 == 0, clue = s"and at $k")
      assertEquals(or.raw(k), k % 2 == 0 || k % 3 == 0, clue = s"or at $k")
    end for
  }

  test("&& and || - dense row-major") {
    val other = Matrix(Array(true, true, false, true, false, true), 2, 3, 3, 1, 0)
    // other: [ T, T, F ]
    //        [ T, F, T ]
    assertLogical(rowMajor && other, Array(true, false, false), Array(false, false, true))
    assertLogical(rowMajor || other, Array(true, true, true), Array(true, false, true))
  }

  test("&& and || - mixed layouts") {
    val colMajor = Matrix.fromRows[Boolean](Array(true, true, false), Array(true, false, true))
    assertLogical(rowMajor && colMajor, Array(true, false, false), Array(false, false, true))
    assertLogical(colMajor || rowMajor, Array(true, true, true), Array(true, false, true))

    val dense = Matrix.fromRows[Boolean](Array(false, true), Array(true, false))
    assertLogical(strided && dense, Array(false, true), Array(false, false))
    assertLogical(dense || strided, Array(true, true), Array(true, false))
  }

  test("&& and || - dimension mismatch throws") {
    val a = Matrix.zeros[Boolean]((2, 2))
    val b = Matrix.zeros[Boolean]((2, 3))
    intercept[MatrixDimensionMismatch](a && b)
    intercept[MatrixDimensionMismatch](a || b)
  }

end MatrixBooleanSuite
