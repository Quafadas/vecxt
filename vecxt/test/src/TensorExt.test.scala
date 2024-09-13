package vecxt

import munit.FunSuite
import narr.*
import vecxt.Tensors.*
import vecxt.extensions.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

class TensorExtensionSuite extends FunSuite:

  test("Matrix creation from nested NArray") {
    val nestedArr = NArray(
      NArray[Double](1.0, 2.0, 3.5), // col 1
      NArray[Double](3.0, 4.0, 5.0), // col 2
      NArray[Double](6.0, 7.0, 8.0) // col 2
    )
    val matrix = Matrix.fromColumns(nestedArr)
    assertVecEquals(matrix.raw, NArray[Double](1.0, 2.0, 3.5, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0))

    val nestedArr3 = NArray(
      NArray[Double](1.0, 2.0, 3.5), // col 1
      NArray[Double](3.0, 4.0, 5.0) // col 2
    )
    val matrix3 = Matrix.fromColumns(nestedArr3)
    assertVecEquals(matrix3.raw, NArray[Double](1.0, 2.0, 3.5, 3.0, 4.0, 5.0))

    val nestedArr2 = NArray(
      NArray[Double](1.0, 3.0), // row 1
      NArray[Double](2.0, 4.0), // row 2
      NArray[Double](3.5, 5.0) // row 3
    )
    val matrix2 = Matrix.fromRows(nestedArr2)
    assertVecEquals(matrix2.raw, NArray[Double](1.0, 2.0, 3.5, 3.0, 4.0, 5.0))

  }

  test("Matrix multiplication") {
    val mat1 = Matrix(NArray(0.0, 0.0, 1.0, 0.0), (2, 2))
    val mat2 = Matrix(NArray(0.0, 1.0, 0.0, 0.0), (2, 2))

    val result2 = mat1 :@ mat2

    val result = mat1.matmul(mat2)
    assertVecEquals(result.raw, NArray(1.0, 0.0, 0.0, 0.0))
    assertVecEquals(result2.raw, NArray(1.0, 0.0, 0.0, 0.0))
  }

  test("Matrix multiplication2") {
    val mat1 = Matrix(NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (2, 3))
    val mat2 = Matrix(NArray(7.0, 9.0, 11.0, 8.0, 10, 12.0), (3, 2))
    val result = mat1.matmul(mat2)
    assertEquals(result.rows, 2)
    assertEquals(result.cols, 2)
    assertVecEquals(result.raw, NArray(58.0, 139.0, 64.0, 154.0))
  }

  test("Matrix transpose") {
    val originalArray = NArray[Double](1, 2, 3, 4, 5, 6)
    val matrix = Matrix(originalArray, (2, 3))

    val transposedMatrix = matrix.transpose

    val expectedArray = NArray[Double](1, 4, 2, 5, 3, 6)
    val expectedMatrix = Matrix(expectedArray, (3, 2))

    assertEquals(transposedMatrix.raw.toList, expectedMatrix.raw.toList)
    assertEquals(transposedMatrix.rows, expectedMatrix.rows)
    assertEquals(transposedMatrix.cols, expectedMatrix.cols)
  }

  test("Tensor raw array retrieval") {
    val vec = Vector(NArray[Double](1.0, 2.0, 3.0))
    assertVecEquals(vec.raw, NArray(1.0, 2.0, 3.0))

    val mat = Matrix(NArray[Double](1.0, 2.0, 3.0, 4.0), (2, 2))
    assertVecEquals(mat.raw, NArray(1.0, 2.0, 3.0, 4.0))

  }

  test("Tensor elementAt retrieval for 1D tensor") {
    val tensor = Vector(NArray[Double](1.0, 2.0, 3.0))
    assertEquals(tensor.elementAt(Tuple1(0)), 1.0)
    assertEquals(tensor.elementAt(Tuple1(1)), 2.0)
    assertEquals(tensor.elementAt(Tuple1(2)), 3.0)
  }

  test("Tensor elementAt retrieval for 2D tensor") {
    val tensor = Matrix(NArray[Double](1.0, 2.0, 3.0, 4.0), (2, 2))
    assertEquals(tensor.elementAt((0, 0)), 1.0)
    assertEquals(tensor.elementAt((0, 1)), 2.0)
    assertEquals(tensor.elementAt((1, 0)), 3.0)
    assertEquals(tensor.elementAt((1, 1)), 4.0)
  }

  test("Tensor elementAt retrieval for 3D tensor") {
    val tensor = Tensors.Tensor(NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), (2, 2, 2))
    assertEquals(tensor.elementAt((0, 0, 0)), 1.0)
    assertEquals(tensor.elementAt((0, 0, 1)), 2.0)
    assertEquals(tensor.elementAt((0, 1, 0)), 3.0)
    assertEquals(tensor.elementAt((0, 1, 1)), 4.0)
    assertEquals(tensor.elementAt((1, 0, 0)), 5.0)
    assertEquals(tensor.elementAt((1, 0, 1)), 6.0)
    assertEquals(tensor.elementAt((1, 1, 0)), 7.0)
    assertEquals(tensor.elementAt((1, 1, 1)), 8.0)
  }

  test("Tensor creation") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val tensor = Tensors.Tensor(array, (2, 2))
    assertEquals(tensor.raw, array)
  }

  test("Vector creation") {
    val array = NArray[Double](1.0, 2.0, 3.0)
    val vector = Tensors.Vector(array)
    assertEquals(vector.raw, array)
  }

  test("Matrix creation") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Tensors.Matrix(array, (2, 2))
    assertEquals(matrix.raw, array)
  }

  test("Matrix rows and cols") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Tensors.Matrix(array, (2, 2))
    assertEquals(matrix.rows, 2)
    assertEquals(matrix.cols, 2)
  }

  test("Matrix row extraction") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Tensors.Matrix(array, (2, 2))
    val row = matrix.row(0)
    assertVecEquals(row, NArray[Double](1.0, 3.0))

    val row2 = matrix.row(1)
    assertVecEquals(row2, NArray[Double](2.0, 4.0))
  }

  test("Matrix column extraction") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val matrix = Tensors.Matrix(array, (2, 2))
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
    val matrix = Tensors.Matrix(array, (2, 2))
    val col1 = matrix.scale(2)
    assertVecEquals(matrix.raw, NArray[Double](2.0, 4.0, 6.0, 8.0))
  }

  test("invalid matrix fails to build") {
    val array = NArray[Double](1.0, 2.0, 3.0, 4.0)
    intercept[InvalidMatrix](
      Matrix(array, (2, 3))
    )

    intercept[InvalidMatrix](
      Matrix((2, 3), array)
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

end TensorExtensionSuite
