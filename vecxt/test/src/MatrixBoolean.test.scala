package vecxt

import munit.FunSuite
import narr.*
import vecxt.MatrixStuff.*
import vecxt.MatrixStuff.Matrix.*
import vecxt.extensions.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

class MatrixBooleanSuite extends FunSuite:

  test("zeros") {
    val tensor = Matrix.zeros[Boolean]((2, 2))
    assertVecEquals(tensor.raw, NArray[Boolean](false, false, false, false))
  }

  test("eye") {
    val tensor = Matrix.eye[Boolean](2)
    assertVecEquals(tensor.raw, NArray[Boolean](true, false, false, true))
  }

  test("ones") {
    val tensor = Matrix.ones[Double]((2, 2))
    assertVecEquals(tensor.raw, NArray[Double](1.0, 1.0, 1.0, 1.0))
  }

  test("Matrix creation") {
    val array = NArray[Boolean](true, false, true, false)
    val matrix = Matrix[(Int, Int), Boolean](array, (2, 2))
    assertEquals(matrix.raw, array)
  }

  test("Tensor raw array retrieval") {
    val mat = Matrix[(Int, Int), Boolean](NArray[Boolean](true, false, true, false), (2, 2))
    assertVecEquals(mat.raw, NArray[Boolean](true, false, true, false))
  }

  test("slice syntax") {
    val mat = Matrix.fromRows[Boolean](
      NArray(
        NArray[Boolean](true, false, true),
        NArray[Boolean](false, true, false)
      )
    )

    val b = mat(::, 0)
    assertVecEquals(NArray[Boolean](true, false), b.raw)

  }

  test("logicals") {
    val mat = Matrix.fromRows[Double](
      NArray(
        NArray[Double](1.0, 2.0),
        NArray[Double](3.0, 4.0)
      )
    )

    val lt = mat < 2.0
    assertVecEquals(NArray(true, false, false, false), lt.raw)

    val gt = mat > 2.0
    assertVecEquals(NArray(false, true, false, true), gt.raw)

    val gte = mat >= 2.0
    assertVecEquals(NArray(false, true, true, true), gte.raw)

    val lte = mat <= 2.0
    assertVecEquals(NArray(true, false, true, false), lte.raw)

  }

end MatrixBooleanSuite
