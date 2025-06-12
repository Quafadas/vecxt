package vecxt

import narr.*
import scala.util.chaining.*
import matrix.*
import arrays.*
import MatrixHelper.*
import MatrixInstance.*

class UpdateSuite extends munit.FunSuite:
  import BoundsCheck.DoBoundsCheck.yes

  val simpleMat = FunFixture[Matrix[Double]](
    setup = test =>
      val row1 = NArray[Double](1.0, 2.0)
      Matrix.fromRows(
        row1,
        row1 +:+ 10,
        row1 +:+ 20
      )
    ,
    teardown = _ => ()
  )

  test("array update") {
    val vec = NArray[Double](1.0, 2.0, 3.0, 4.0)
    vec(2) = 3.5
    assertEquals(vec(2), 3.5)
  }

  test("matrix update with function") {
    val mat = Matrix.fromRows(
      NArray(1.0, 2.0),
      NArray(3.0, 4.0)
    )
    mat(_ > 2.0) = 5.0
    assertEqualsDouble(mat(0, 0), 1.0, 0.0000001)
    assertEqualsDouble(mat(0, 1), 2.0, 0.0000001)
    assertEqualsDouble(mat(1, 0), 5.0, 0.0000001)
    assertEqualsDouble(mat(1, 1), 5.0, 0.0000001)
  }

  test("matrix update from boolean matrix") {
    val mat = Matrix.fromRows[Double](
      NArray(1.0, 2.0),
      NArray(3.0, 4.0)
    )
    val boolMat: Matrix[Boolean] = Matrix.fromRows[Boolean](
      NArray(true, false),
      NArray(false, true)
    )

    mat(boolMat) = 5.0

    assertEqualsDouble(mat(0, 0), 5.0, 0.0000001)
    assertEqualsDouble(mat(0, 1), 2.0, 0.0000001)
    assertEqualsDouble(mat(1, 0), 3.0, 0.0000001)
    assertEqualsDouble(mat(1, 1), 5.0, 0.0000001)
  }

  // TODO
  // This fails on JS. I don't think it should - but it's also a very painful difference in the way JS arrays react to i
  // index out of bounds problems. I don't know how to overload it meaninfully.
  test("array update should fail out of bounds".ignore) {
    val vec = NArray[Double](1.0, 2.0, 3.0, 4.0)
    intercept[java.lang.IndexOutOfBoundsException] {
      vec(5) = 3.5
      // println(vec.mkString(","))
    }
    intercept[java.lang.IndexOutOfBoundsException] {
      vec(-1) = 3.5
      // println(vec.mkString(","))
    }
  }

  simpleMat.test("Mat update fail out of bounds min row") { mat =>
    intercept[java.lang.IndexOutOfBoundsException] {
      mat((-1, 1)) = 2.0
    }
  }

  simpleMat.test("Mat update fail out of bounds min col") { mat =>
    intercept[java.lang.IndexOutOfBoundsException] {
      mat((1, -1)) = 2.0
    }
  }

  simpleMat.test("Mat update fail out of bounds max col") { mat =>
    intercept[java.lang.IndexOutOfBoundsException] {
      mat((1, 3)) = 2.0
    }
  }

  simpleMat.test("Mat update fail out of bounds max row") { mat =>
    intercept[java.lang.IndexOutOfBoundsException] {
      mat((4, 1)) = 2.0
    }
  }

  simpleMat.test("Matrix update") { mat =>
    mat((1, 1)) = 0.5
    assertEqualsDouble(mat(1, 1), 0.5, 0.0000001)
  }
end UpdateSuite
