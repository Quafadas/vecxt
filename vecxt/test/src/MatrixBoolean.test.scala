package vecxt

import all.*
import munit.FunSuite
import narr.*
import BoundsCheck.DoBoundsCheck.yes

class MatrixBooleanSuite extends FunSuite:

  test("zeros") {
    val mat = Matrix.zeros[Boolean]((2, 2))
    assertVecEquals[Boolean](mat.raw, NArray[Boolean](false, false, false, false))
  }

  test("eye") {
    val mat = Matrix.eye[Boolean](2)
    assertVecEquals[Boolean](mat.raw, NArray[Boolean](true, false, false, true))
  }

  test("ones") {
    val mat = Matrix.ones[Double]((2, 2))
    assertVecEquals[Double](mat.raw, NArray[Double](1.0, 1.0, 1.0, 1.0))
  }

  test("Matrix creation") {
    val array = NArray[Boolean](true, false, true, false)
    val matrix = Matrix[Boolean](array, (2, 2))
    assertEquals(matrix.raw, array)
  }

  test("raw array retrieval") {
    val mat = Matrix[Boolean](NArray[Boolean](true, false, true, false), (2, 2))
    assertVecEquals[Boolean](mat.raw, NArray[Boolean](true, false, true, false))
  }

  test("slice syntax") {
    val mat = Matrix.fromRows[Boolean](
      NArray[Boolean](true, false, true),
      NArray[Boolean](false, true, false)
    )

    val b = mat(::, NArray[Int](0))
    assertVecEquals[Boolean](NArray[Boolean](true, false), b.raw)

  }

  test("logicals") {
    val mat = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0),
      NArray[Double](3.0, 4.0)
    )

    val lt = mat < 2.0
    assertVecEquals[Boolean](NArray[Boolean](true, false, false, false), lt.raw)

    val gt = mat > 2.0
    assertVecEquals[Boolean](NArray[Boolean](false, true, false, true), gt.raw)

    val gte = mat >= 2.0
    assertVecEquals[Boolean](NArray[Boolean](false, true, true, true), gte.raw)

    val lte = mat <= 2.0
    assertVecEquals[Boolean](NArray[Boolean](true, false, true, false), lte.raw)

  }

  test("elementwise mult double array") {
    val mat = Matrix.fromRows[Double](
      NArray[Double](1.0, 2.0, 3.0),
      NArray[Double](3.0, 4.0, 5.0)
    )

    val bools = Matrix.fromRows[Boolean](
      NArray[Boolean](true, false, true),
      NArray[Boolean](false, true, false)
    )

    val calc = mat *:* bools
    val result = Matrix.fromRows[Double](
      NArray[Double](1.0, 0.0, 3.0),
      NArray[Double](0.0, 4.0, 0.0)
    )
    assertVecEquals[Double](calc.raw, result.raw)
  }

end MatrixBooleanSuite
