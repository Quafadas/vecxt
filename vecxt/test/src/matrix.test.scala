package vecxt

import munit.FunSuite
import narr.*
import vecxt.matrix.*
import vecxt.arrays.`*`
import vecxt.BoundsCheck.DoBoundsCheck.yes
import vecxt.matrixUtil.*
import vecxt.MatrixHelper.*
import vecxt.MatrixInstance.*
import vecxt.arrayUtil.*
import vecxt.JsDoubleMatrix.*
import vecxt.JvmNativeDoubleMatrix.*
import vecxt.JvmDoubleMatrix.*
import vecxt.DoubleMatrix.*
import vecxt.NativeDoubleMatrix.*

class MatrixExtensionSuite extends FunSuite:

  // TODO will fail on JS, grrr.
  // test("print") {
  //   val mat1 = Matrix[Double](NArray(1.0, 4.0, 2.0, 5.0), (2, 2))
  //   assert(mat1.printMat.contains("4"))

  // }

  // test("transpose etc".only) {
  //   val mat1 = Matrix(NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (2, 3))
  //   val mat2 = Matrix(NArray(7.0, 9.0, 11.0, 8.0, 10, 12.0), (3, 2))
  //   val result2 = mat1 @@ mat2

  //   result2.printMat
  //   val result3 = Matrix.eye(2) + mat1 @@ mat2
  //   result3.printMat
  //   val mat3 = mat2.transpose + mat1
  //   println(mat2.transpose.printMat)
  //   mat3.raw.printArr
  //   mat3.printMat
  // }

  test("element access") {

    val orig = Matrix.fromRows[Double](
      NArray(
        NArray(2.0, 0.0, -1.0),
        NArray(0.0, 3.0, 4.0)
      )
    )

    assertEqualsDouble(orig((0, 0)), 2, 0.0001)
    assertEqualsDouble(orig((0, 1)), 0, 0.0001)
    assertEqualsDouble(orig((0, 2)), -1, 0.0001)
    assertEqualsDouble(orig((1, 0)), 0, 0.0001)
    assertEqualsDouble(orig((1, 1)), 3, 0.0001)
    assertEqualsDouble(orig((1, 2)), 4, 0.0001)

  }

  test("diagonal") {
    val orig = Matrix.fromRows[Double](
      NArray(
        NArray(2.0, 0.0, -1.0),
        NArray(1.0, 3.0, 4.0),
        NArray(0.0, 9.0, 5.0)
      )
    )

    val diag = orig.diag
    assertVecEquals(diag, NArray[Double](2.0, 3.0, 5.0))

    val diag2 = orig.diag(1: Col, Vertical.Top, Horizontal.Right)
    assertVecEquals(diag2, NArray[Double](0.0, 4.0))

    val diag3 = orig.diag(1: Col, Vertical.Top, Horizontal.Left)

    assertVecEquals(diag3, NArray[Double](0.0, 1.0))

    val diag4 = orig.diag(1: Col, Vertical.Bottom, Horizontal.Left)
    assertVecEquals(diag4, NArray[Double](9.0, 1.0))

    val diag5 = orig.diag(1: Row, Horizontal.Left, Vertical.Top)
    assertVecEquals(diag5, NArray[Double](1.0, 0.0))

    val diag6 = orig.diag(orig.cols - 1, Vertical.Top, Horizontal.Left)
    assertVecEquals(diag6, NArray[Double](-1.0, 3.0, 0.0))

    val diag7 = orig.diag(orig.cols - 1, Vertical.Bottom, Horizontal.Left)
    assertVecEquals(diag7, NArray[Double](5.0, 3.0, 2.0))

    val diag8 = orig.diag(1: Row, Horizontal.Right, Vertical.Top)
    assertVecEquals(diag8, NArray[Double](4.0, 0.0))
  }

  test("diagonal irregular") {
    val orig = Matrix.fromRows[Double](
      NArray(
        NArray(2.0, 0.0),
        NArray(1.0, 3.0),
        NArray(0.0, 9.0)
      )
    )

    val diag = orig.diag
    assertVecEquals(diag, NArray[Double](2.0, 3.0))

    val diag5 = orig.diag(1: Row, Horizontal.Left, Vertical.Top)
    assertVecEquals(diag5, NArray[Double](1.0, 0.0))
  }

  test("operator precedance") {
    val mat1 = Matrix.eye[Double](2)
    val mat2 = Matrix[Double](mat1.raw * 2, (mat1.rows, mat1.cols))

    val mat3 = mat1 + mat1 @@ mat2
    assertEqualsDouble(mat3((0, 0)), 3.0, 0.00001)

  }

  test("zeros") {
    val tensor = Matrix.zeros[Double](2, 2)
    assertVecEquals(tensor.raw, NArray[Double](0.0, 0.0, 0.0, 0.0))
  }

  test("eye") {
    val tensor = Matrix.eye[Double](2)
    assertVecEquals(tensor.raw, NArray[Double](1.0, 0.0, 0.0, 1.0))
  }

  test("ones") {
    val tensor = Matrix.ones[Double]((2, 2))
    assertVecEquals(tensor.raw, NArray[Double](1.0, 1.0, 1.0, 1.0))
  }

  test("Matrix creation from nested NArray") {
    val nestedArr = NArray(
      NArray[Double](1.0, 2.0, 3.5), // col 1
      NArray[Double](3.0, 4.0, 5.0), // col 2
      NArray[Double](6.0, 7.0, 8.0) // col 2
    )
    val matrix = Matrix.fromColumns[Double](nestedArr)
    assertVecEquals(matrix.raw, NArray[Double](1.0, 2.0, 3.5, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0))

    val nestedArr3 = NArray(
      NArray[Double](1.0, 2.0, 3.5), // col 1
      NArray[Double](3.0, 4.0, 5.0) // col 2
    )
    val matrix3 = Matrix.fromColumns[Double](nestedArr3)
    assertVecEquals(matrix3.raw, NArray[Double](1.0, 2.0, 3.5, 3.0, 4.0, 5.0))

    val nestedArr2 = NArray(
      NArray[Double](1.0, 3.0), // row 1
      NArray[Double](2.0, 4.0), // row 2
      NArray[Double](3.5, 5.0) // row 3
    )
    val matrix2 = Matrix.fromRows[Double](nestedArr2)
    assertVecEquals(matrix2.raw, NArray[Double](1.0, 2.0, 3.5, 3.0, 4.0, 5.0))

  }

  test("Matrix multiplication") {
    val mat1 = Matrix[Double](NArray(0.0, 0.0, 1.0, 0.0), (2, 2))
    val mat2 = Matrix[Double](NArray(0.0, 1.0, 0.0, 0.0), (2, 2))

    val result2 = mat1 @@ mat2

    val result = mat1.matmul(mat2)
    assertVecEquals(result.raw, NArray(1.0, 0.0, 0.0, 0.0))
    assertVecEquals(result2.raw, NArray(1.0, 0.0, 0.0, 0.0))
  }

  test("Matrix multiplication2") {
    val mat1 = Matrix[Double](NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (2, 3))
    val mat2 = Matrix[Double](NArray(7.0, 9.0, 11.0, 8.0, 10, 12.0), (3, 2))
    val result = mat1.matmul(mat2)
    assertEquals(result.rows, 2)
    assertEquals(result.cols, 2)
    assertVecEquals(result.raw, NArray(58.0, 139.0, 64.0, 154.0))
  }

  test("Matrix transpose") {
    val orig = Matrix.fromRows(
      NArray(
        NArray(2.0, 0.0, -1.0),
        NArray(0.0, 3.0, 4.0)
      )
    )
    val transpose = Matrix.fromRows(
      NArray(
        NArray(2.0, 0.0),
        NArray(0.0, 3.0),
        NArray(-1.0, 4.0)
      )
    )

    val transposedMatrix = orig.transpose

    assertEquals(transposedMatrix.raw.toList, transpose.raw.toList)

  }

  test("Tensor raw array retrieval") {
    val mat = Matrix[Double](NArray[Double](1.0, 2.0, 3.0, 4.0), (2, 2))
    assertVecEquals(mat.raw, NArray(1.0, 2.0, 3.0, 4.0))

  }

  test("Tensor elementAt retrieval for 2D tensor") {
    val tensor = Matrix.fromRows(
      NArray(
        NArray[Double](1.0, 2.0),
        NArray[Double](3.0, 4.0)
      )
    )
    assertEquals(tensor((0, 0)), 1.0)
    assertEquals(tensor((0, 1)), 2.0)
    assertEquals(tensor((1, 0)), 3.0)
    assertEquals(tensor((1, 1)), 4.0)
  }

  // test("Tensor elementAt retrieval for 3D tensor") {
  //   val tensor = Matrix(NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), (2, 2, 2))
  //   assertEquals(tensor.elementAt((0, 0, 0)), 1.0)
  //   assertEquals(tensor.elementAt((0, 0, 1)), 2.0)
  //   assertEquals(tensor.elementAt((0, 1, 0)), 3.0)
  //   assertEquals(tensor.elementAt((0, 1, 1)), 4.0)
  //   assertEquals(tensor.elementAt((1, 0, 0)), 5.0)
  //   assertEquals(tensor.elementAt((1, 0, 1)), 6.0)
  //   assertEquals(tensor.elementAt((1, 1, 0)), 7.0)
  //   assertEquals(tensor.elementAt((1, 1, 1)), 8.0)
  // }

  // test("Tensor creation") {
  //   val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
  //   val tensor = Tensor(array, (2, 2))
  //   assertEquals(tensor.raw, array)
  // }

  // test("Vector creation") {
  //   val array = NArray[Double](1.0, 2.0, 3.0)
  //   val vector = Vector(array)
  //   assertEquals(vector.raw, array)
  // }

  test("Matrix creation") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Matrix[Double](array, (2, 2))
    assertEquals(matrix.raw, array)
  }

  test("Matrix rows and cols") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Matrix[Double](array, (2, 2))
    assertEquals(matrix.rows, 2)
    assertEquals(matrix.cols, 2)
  }

  test("Matrix row extraction") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Matrix[Double](array, (2, 2))
    val row = matrix.row(0)
    assertVecEquals(row, NArray[Double](1.0, 3.0))

    val row2 = matrix.row(1)
    assertVecEquals(row2, NArray[Double](2.0, 4.0))
  }

  test("update") {
    val mat = Matrix[Double](NArray(1.0, 2.0, 3.0, 4.0), (2, 2))
    mat((0, 0)) = 5.0
    assertEquals(mat.raw(0), 5.0)

    mat((1, 1)) = 6.0
    assertEquals(mat.raw(3), 6.0)

    mat((1, 0)) = 7.0
    assertEquals(mat.raw(1), 7.0)
  }

  test("trace") {
    val mat = Matrix[Double](NArray(1.0, 2.0, 3.0, 4.0), (2, 2))
    assertEquals(mat.trace, 5.0)
  }

  test("Matrix column extraction") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Matrix[Double](array, (2, 2))
    val col1 = matrix.col(0)
    assertVecEquals(col1, NArray[Double](1.0, 2.0))

    val col = matrix.col(1)
    assertVecEquals(col, NArray[Double](3.0, 4.0))

  }

  test("That we have to provide ints for dimensions") {
    // We should allow non-integer values in the dimensions of the Tuple.
    val code = "Matrix((1, 1.0), NArray.ofSize[Double](6))"
    val code2 = """Matrix((1, "s"), NArray.ofSize[Double](6))"""
    compileErrors(code)
    compileErrors(code2)
  }

  test("matrix scale") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Matrix[Double](array, (2, 2))
    val col1 = matrix *:*= (2)
    assertVecEquals(matrix.raw, NArray[Double](2.0, 4.0, 6.0, 8.0))
  }

  test("matrix ops") {
    val mat1 = Matrix.eye[Double](3) // multiplication in place
    mat1 *:*= 2
    val mat2 = Matrix.eye[Double](3) + Matrix.eye[Double](3) // addition
    assertVecEquals(mat1.raw, mat2.raw)
  }

  test("invalid matrix fails to build") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    intercept[InvalidMatrix](
      Matrix[Double](array, (2, 3))
    )

    intercept[InvalidMatrix](
      Matrix[Double]((2, 3), array)
    )

    intercept[MatrixDimensionMismatch](
      Matrix.eye[Double](1) + Matrix.eye[Double](2)
    )

    intercept[java.lang.AssertionError](
      Matrix.fromColumns(
        NArray(
          NArray[Double](3.0, 2.0, 3.0),
          NArray[Double](2.0, 1.0)
        )
      )
    )

    intercept[java.lang.AssertionError](
      Matrix.fromRows(
        NArray(
          NArray[Double](3.0, 2.0, 3.0),
          NArray[Double](2.0, 1.0)
        )
      )
    )
  }

  test("slice syntax") {
    val mat = Matrix.fromRows(
      NArray(
        NArray[Double](1.0, 2.0, 3.0),
        NArray[Double](4.0, 5.0, 6.0),
        NArray[Double](7.0, 8.0, 9.0)
      )
    )
    val a = mat(::, ::)
    assertVecEquals(a.raw, mat.raw)

    val b = mat(1, ::)
    assertVecEquals(NArray[Double](4.0, 5.0, 6.0), b.raw)

    val c = mat(::, 1)
    assertVecEquals(NArray[Double](2.0, 5.0, 8.0), c.raw)

    val d = mat(1, 1)
    assertVecEquals(NArray[Double](5.0), d.raw)

    val e = mat(0 to 1, 0 to 1)
    assertVecEquals(NArray[Double](1.0, 4.0, 2.0, 5.0), e.raw)

    val f = mat(NArray.from[Int](Array(0, 2)), 0 to 1)
    assertVecEquals(NArray[Double](1.0, 7.0, 2.0, 8.0), f.raw)

  }

end MatrixExtensionSuite
