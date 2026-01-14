package vecxt

import all.*
import munit.FunSuite

import BoundsCheck.DoBoundsCheck.yes

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

end MatrixBooleanSuite
