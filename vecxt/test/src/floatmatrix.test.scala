package vecxt

import munit.FunSuite

import all.*

class FloatMatrixExtensionSuite extends FunSuite:

  test("scalar subtract/divide elementwise - Float") {
    val mat = Matrix[Float](Array(2.0f, 4.0f, 6.0f, 8.0f), (2, 2))

    val t1 = mat - 1.0f
    assertVecEquals[Float](t1.raw, Array[Float](1.0f, 3.0f, 5.0f, 7.0f))

    val t2 = t1 / 2.0f
    assertVecEquals[Float](t2.raw, Array[Float](0.5f, 1.5f, 2.5f, 3.5f))

    val inPlace = mat.deepCopy
    inPlace /= 2.0f
    assertVecEquals[Float](inPlace.raw, Array[Float](1.0f, 2.0f, 3.0f, 4.0f))
  }

  test("matrix addition and subtraction - Float") {
    val mat1 = Matrix.eye[Float](2)
    val mat2 = Matrix.eye[Float](2)
    val result = mat1 +:+ mat2
    assertVecEquals[Float](result.raw, Array[Float](2.0f, 0.0f, 0.0f, 2.0f))

    val result2 = mat1 -:- mat2
    assertVecEquals[Float](result2.raw, Array[Float](0.0f, 0.0f, 0.0f, 0.0f))

    val result3 = mat1 + mat2
    assertVecEquals[Float](result3.raw, Array[Float](2.0f, 0.0f, 0.0f, 2.0f))
  }

  test("hadamard product - Float") {
    val mat1 = Matrix[Float](Array(1.0f, 2.0f, 3.0f, 4.0f), (2, 2))
    val mat2 = Matrix[Float](Array(2.0f, 2.0f, 2.0f, 2.0f), (2, 2))

    val result = mat1.hadamard(mat2)
    assertVecEquals[Float](result.raw, Array[Float](2.0f, 4.0f, 6.0f, 8.0f))

    val result2 = mat1 * mat2
    assertVecEquals[Float](result2.raw, Array[Float](2.0f, 4.0f, 6.0f, 8.0f))
  }

  test("/:/ elementwise divide - Float") {
    val mat1 = Matrix[Float](Array(2.0f, 4.0f, 6.0f, 8.0f), (2, 2))
    val mat2 = Matrix[Float](Array(2.0f, 2.0f, 2.0f, 2.0f), (2, 2))

    val result = mat1 /:/ mat2
    assertVecEquals[Float](result.raw, Array[Float](1.0f, 2.0f, 3.0f, 4.0f))
  }

  test("element-wise maximum - Float") {
    val mat1 = Matrix[Float](Array(1.0f, 5.0f, 3.0f, 2.0f), (2, 2))
    val mat2 = Matrix[Float](Array(4.0f, 2.0f, 3.0f, 8.0f), (2, 2))

    val result = mat1.maximum(mat2)
    assertVecEquals[Float](result.raw, Array[Float](4.0f, 5.0f, 3.0f, 8.0f))
  }

  test("unary_- - Float") {
    val mat = Matrix[Float](Array(1.0f, -2.0f, 3.0f, -4.0f), (2, 2))
    val result = -mat
    assertVecEquals[Float](result.raw, Array[Float](-1.0f, 2.0f, -3.0f, 4.0f))
  }

  test("exp/log/sqrt/sin/cos/tan - Float") {
    val mat = Matrix[Float](Array(1.0f, 2.0f, 3.0f, 4.0f), (2, 2))

    assertEqualsFloat(mat.exp.raw(0), Math.exp(1.0).toFloat, 1e-5f)
    assertEqualsFloat(mat.log.raw(0), Math.log(1.0).toFloat, 1e-5f)
    assertEqualsFloat(mat.sqrt.raw(1), Math.sqrt(2.0).toFloat, 1e-5f)
    assertEqualsFloat(mat.sin.raw(2), Math.sin(3.0).toFloat, 1e-5f)
    assertEqualsFloat(mat.cos.raw(3), Math.cos(4.0).toFloat, 1e-5f)
    assertEqualsFloat(mat.tan.raw(0), Math.tan(1.0).toFloat, 1e-5f)

    val expInPlace = mat.deepCopy
    expInPlace.`exp!`
    assertEqualsFloat(expInPlace.raw(0), Math.exp(1.0).toFloat, 1e-5f)

    val logInPlace = mat.deepCopy
    logInPlace.`log!`
    assertEqualsFloat(logInPlace.raw(0), Math.log(1.0).toFloat, 1e-5f)

    val sqrtInPlace = mat.deepCopy
    sqrtInPlace.`sqrt!`
    assertEqualsFloat(sqrtInPlace.raw(1), Math.sqrt(2.0).toFloat, 1e-5f)

    val sinInPlace = mat.deepCopy
    sinInPlace.`sin!`
    assertEqualsFloat(sinInPlace.raw(2), Math.sin(3.0).toFloat, 1e-5f)

    val cosInPlace = mat.deepCopy
    cosInPlace.`cos!`
    assertEqualsFloat(cosInPlace.raw(3), Math.cos(4.0).toFloat, 1e-5f)

    val tanInPlace = mat.deepCopy
    tanInPlace.`tan!`
    assertEqualsFloat(tanInPlace.raw(0), Math.tan(1.0).toFloat, 1e-5f)
  }

  test("mean - Float") {
    val mat = Matrix[Float](Array(1.0f, 2.0f, 3.0f, 4.0f), (2, 2))
    assertEqualsFloat(mat.mean, 2.5f, 1e-5f)
  }

  test("** power - Float") {
    val mat = Matrix[Float](Array(1.0f, 2.0f, 3.0f, 4.0f), (2, 2))
    val result = mat ** 2.0f
    assertVecEquals[Float](result.raw, Array[Float](1.0f, 4.0f, 9.0f, 16.0f))
  }

  test("trace - Float") {
    val mat = Matrix[Float](Array(1.0f, 2.0f, 3.0f, 4.0f), (2, 2))
    assertEqualsFloat(mat.trace, 5.0f, 1e-5f)
  }

  test("sum/sumSIMD - Float") {
    val mat = Matrix[Float](Array(1.0f, 2.0f, 3.0f, 4.0f), (2, 2))
    assertEqualsFloat(mat.sum, 10.0f, 1e-5f)
    assertEqualsFloat(mat.sumSIMD, 10.0f, 1e-5f)
  }

  test("norm - Float") {
    val mat = Matrix[Float](Array(3.0f, 4.0f), (2, 1))
    assertEqualsFloat(mat.norm, 5.0f, 1e-5f)
  }

  test("kronecker - Float is not yet implemented") {
    val mat = Matrix[Float](Array(1.0f, 2.0f, 3.0f, 4.0f), (2, 2))
    intercept[NotImplementedError] {
      mat.kronecker(mat)
    }
  }

end FloatMatrixExtensionSuite
